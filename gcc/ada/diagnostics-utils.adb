------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     D I A G N O S T I C S . U T I L S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Diagnostics.Repository;        use Diagnostics.Repository;
with Diagnostics.Switch_Repository; use Diagnostics.Switch_Repository;
with Errout;                        use Errout;
with Erroutc;                       use Erroutc;
with Namet;                         use Namet;
with Opt;                           use Opt;
with Sinput;                        use Sinput;
with Sinfo.Nodes;                   use Sinfo.Nodes;
with Warnsw;                        use Warnsw;

package body Diagnostics.Utils is

   ------------------
   -- Get_Human_Id --
   ------------------

   function Get_Human_Id (D : Diagnostic_Type) return String_Ptr is
   begin
      if D.Switch = No_Switch_Id then
         return Diagnostic_Entries (D.Id).Human_Id;
      else
         return Get_Switch (D).Human_Id;
      end if;
   end Get_Human_Id;

   ------------------
   -- To_File_Name --
   ------------------

   function To_File_Name (Sptr : Source_Ptr) return String is
      Sfile    : constant Source_File_Index := Get_Source_File_Index (Sptr);
      Ref_Name : constant File_Name_Type    :=
        (if Full_Path_Name_For_Brief_Errors then Full_Ref_Name (Sfile)
         else Reference_Name (Sfile));

   begin
      return Get_Name_String (Ref_Name);
   end To_File_Name;

   --------------------
   -- Line_To_String --
   --------------------

   function Line_To_String (Sptr : Source_Ptr) return String is
      Line    : constant Logical_Line_Number := Get_Logical_Line_Number (Sptr);
      Img_Raw : constant String  := Int'Image (Int (Line));

   begin
      return Img_Raw (Img_Raw'First + 1 .. Img_Raw'Last);
   end Line_To_String;

   ----------------------
   -- Column_To_String --
   ----------------------

   function Column_To_String (Sptr : Source_Ptr) return String is
      Col : constant Column_Number := Get_Column_Number (Sptr);
      Img_Raw : constant String  := Int'Image (Int (Col));

   begin
      return
        (if Col < 10 then "0" else "")
        & Img_Raw (Img_Raw'First + 1 .. Img_Raw'Last);
   end Column_To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Sptr : Source_Ptr) return String is
   begin
      return
        To_File_Name (Sptr) & ":" & Line_To_String (Sptr) & ":"
        & Column_To_String (Sptr);
   end To_String;

   --------------------
   -- Sloc_To_String --
   --------------------

   function Sloc_To_String
     (N : Node_Or_Entity_Id; Ref : Source_Ptr) return String
   is

   begin
      return Sloc_To_String (Sloc (N), Ref);
   end Sloc_To_String;

   --------------------
   -- Sloc_To_String --
   --------------------

   function Sloc_To_String (Sptr : Source_Ptr; Ref : Source_Ptr) return String
   is

   begin
      if Sptr = No_Location then
         return "at unknown location";

      elsif Sptr = System_Location then
         return "in package System";

      elsif Sptr = Standard_Location then
         return "in package Standard";

      elsif Sptr = Standard_ASCII_Location then
         return "in package Standard.ASCII";

      else
         if Full_File_Name (Get_Source_File_Index (Sptr))
            /= Full_File_Name (Get_Source_File_Index (Ref))
         then
            return "at " & To_String (Sptr);
         else
            return "at line " & Line_To_String (Sptr);
         end if;
      end if;
   end Sloc_To_String;

   ------------------
   -- To_Full_Span --
   ------------------

   function To_Full_Span (N : Node_Id) return Source_Span
   is
      Fst, Lst : Node_Id;
   begin
      First_And_Last_Nodes (N, Fst, Lst);
      return To_Span (Ptr   => Sloc (N),
                      First => First_Sloc (Fst),
                      Last  => Last_Sloc (Lst));
   end To_Full_Span;

   ---------------
   -- To_String --
   ---------------

   function To_String (Id : Diagnostic_Id) return String is
   begin
      if Id = No_Diagnostic_Id then
         return "GNAT0000";
      else
         return Id'Img;
      end if;
   end To_String;

   -------------
   -- To_Name --
   -------------

   function To_Name (E : Entity_Id) return String is
   begin
      --  The name of the node operator "&" has many special cases. Reuse the
      --  node to name conversion implementation from the errout package for
      --  now.

      Error_Msg_Node_1 := E;
      Set_Msg_Text ("&", Sloc (E));

      return Msg_Buffer (1 .. Msglen);
   end To_Name;

   ------------------
   -- To_Type_Name --
   ------------------

   function To_Type_Name (E : Entity_Id) return String is
   begin
      Error_Msg_Node_1 := E;
      Set_Msg_Text ("}", Sloc (E));

      return Msg_Buffer (1 .. Msglen);
   end To_Type_Name;

   --------------------
   -- Kind_To_String --
   --------------------

   function Kind_To_String
     (D : Sub_Diagnostic_Type;
      Parent : Diagnostic_Type) return String
   is
     (case D.Kind is
        when Continuation => Kind_To_String (Parent),
        when Help => "help",
        when Note => "note",
        when Suggestion => "suggestion");

   --------------------
   -- Kind_To_String --
   --------------------

   function Kind_To_String (D : Diagnostic_Type) return String is
     (if D.Warn_Err then "error"
      else
       (case D.Kind is
        when Diagnostics.Error | Non_Serious_Error => "error",
        when Warning | Restriction_Warning | Default_Warning |
             Tagless_Warning => "warning",
        when Style => "style",
        when Info => "info"));

   ------------------------------
   -- Get_Primary_Labeled_Span --
   ------------------------------

   function Get_Primary_Labeled_Span (Spans : Labeled_Span_List)
                                      return Labeled_Span_Type
   is
      use Labeled_Span_Lists;

      S  : Labeled_Span_Type;
      It : Iterator;
   begin
      if Present (Spans) then
         It := Iterate (Spans);
         while Has_Next (It) loop
            Next (It, S);
            if S.Is_Primary then
               return S;
            end if;
         end loop;
      end if;

      return No_Labeled_Span;
   end Get_Primary_Labeled_Span;

   --------------------
   -- Get_Doc_Switch --
   --------------------

   function Get_Doc_Switch (Diag : Diagnostic_Type) return String is
   begin
      if Warning_Doc_Switch
        and then Diag.Kind in Default_Warning
          | Info
          | Restriction_Warning
          | Style
          | Warning
      then
         if Diag.Switch = No_Switch_Id then
            if Diag.Kind = Restriction_Warning then
               return "[restriction warning]";

               --  Info messages can have a switch tag but they should not have
               --  a default switch tag.

            elsif Diag.Kind /= Info then

               --  For Default_Warning

               return "[enabled by default]";
            end if;
         else
            declare
               S : constant Switch_Type := Get_Switch (Diag);
            begin
               return "[-" & S.Short_Name.all & "]";
            end;
         end if;
      end if;

      return "";
   end Get_Doc_Switch;

   --------------------
   -- Appears_Before --
   --------------------

   function Appears_Before (D1, D2 : Diagnostic_Type) return Boolean is

   begin
      return Appears_Before (Primary_Location (D1).Span.Ptr,
                             Primary_Location (D2).Span.Ptr);
   end Appears_Before;

   --------------------
   -- Appears_Before --
   --------------------

   function Appears_Before (P1, P2 : Source_Ptr) return Boolean is

   begin
      if Get_Source_File_Index (P1) = Get_Source_File_Index (P2) then
         if Get_Logical_Line_Number (P1) = Get_Logical_Line_Number (P2) then
            return Get_Column_Number (P1) < Get_Column_Number (P2);
         else
            return Get_Logical_Line_Number (P1) < Get_Logical_Line_Number (P2);
         end if;
      else
         return Get_Source_File_Index (P1) < Get_Source_File_Index (P2);
      end if;
   end Appears_Before;

   ------------------------------
   -- Insert_Based_On_Location --
   ------------------------------

   procedure Insert_Based_On_Location
     (List : Diagnostic_List;
      Diagnostic : Diagnostic_Type)
   is
      use Diagnostics_Lists;

      It : Iterator := Iterate (List);
      D  : Diagnostic_Type;
   begin
      --  This is the common scenario where the error is reported at the
      --  natural order the tree is processed. This saves a lot of time when
      --  looking for the correct position in the list when there are a lot of
      --  diagnostics.

      if Present (List) and then
         not Is_Empty (List) and then
         Appears_Before (Last (List), Diagnostic)
      then
         Append (List, Diagnostic);
      else
         while Has_Next (It) loop
            Next (It, D);

            if Appears_Before (Diagnostic, D) then
               Insert_Before (List, D, Diagnostic);
               return;
            end if;
         end loop;

         Append (List, Diagnostic);
      end if;
   end Insert_Based_On_Location;

end Diagnostics.Utils;
