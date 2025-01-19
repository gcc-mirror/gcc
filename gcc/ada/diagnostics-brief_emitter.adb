------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             D I A G N O S T I C S . B R I E F _ E M I T T E R            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with Diagnostics.Utils;             use Diagnostics.Utils;
with Erroutc;                       use Erroutc;
with Opt;                           use Opt;
with Output;                        use Output;

package body Diagnostics.Brief_Emitter is

   procedure Print_Sub_Diagnostic
     (Sub_Diag : Sub_Diagnostic_Type;
      Diag     : Diagnostic_Type);

   --------------------------
   -- Print_Sub_Diagnostic --
   --------------------------

   procedure Print_Sub_Diagnostic
     (Sub_Diag : Sub_Diagnostic_Type;
      Diag     : Diagnostic_Type)
   is
      --  In GNAT sub messages were grouped by the main messages by also having
      --  the same location. In the brief printer we use the primary location
      --  of the main diagnostic for all of the subdiagnostics.
      Prim_Loc : constant Labeled_Span_Type := Primary_Location (Diag);

      Sptr     : constant Source_Ptr        := Prim_Loc.Span.Ptr;

      Text     : String_Ptr;

      Line_Length : constant Nat := (if Error_Msg_Line_Length = 0 then Nat'Last
          else Error_Msg_Line_Length);

      Switch_Str : constant String := Get_Doc_Switch (Diag);
   begin
      Text := new String'(To_String (Sptr) & ": "
         & Kind_To_String (Sub_Diag, Diag)  & ": "
         & Sub_Diag.Message.all);

      if Switch_Str /= "" then
         Text := new String'(Text.all & " " & Switch_Str);
      end if;

      if Diag.Warn_Err then
         Text := new String'(Text.all & " [warning-as-error]");
      end if;

      Output_Text_Within (Text, Line_Length);
      Write_Eol;
   end Print_Sub_Diagnostic;

   ----------------------
   -- Print_Diagnostic --
   ----------------------

   procedure Print_Diagnostic (Diag : Diagnostic_Type) is
      use Sub_Diagnostic_Lists;

      Prim_Loc : constant Labeled_Span_Type := Primary_Location (Diag);

      Sptr     : constant Source_Ptr        := Prim_Loc.Span.Ptr;

      Text     : String_Ptr;

      Line_Length : constant Nat := (if Error_Msg_Line_Length = 0 then Nat'Last
          else Error_Msg_Line_Length);

      Switch_Str : constant String := Get_Doc_Switch (Diag);
   begin
      Write_Str (To_String (Sptr) & ": ");

      --  Ignore the message prefix on Style messages. They will use
      --  the (style) prefix within the message.
      --
      --  Also disable the "error:" prefix if Unique_Error_Tag is unset.

      if (Diag.Kind = Style and then not Diag.Warn_Err)
         or else (Diag.Kind = Error and then not Unique_Error_Tag)
      then
         Text := new String'("");
      else
         Text := new String'(Kind_To_String (Diag) & ": ");
      end if;

      Text := new String'(Text.all & Diag.Message.all);

      if Switch_Str /= "" then
         Text := new String'(Text.all & " " & Switch_Str);
      end if;

      if Diag.Warn_Err then
         Text := new String'(Text.all & " [warning-as-error]");
      end if;

      Output_Text_Within (Text, Line_Length);
      Write_Eol;

      if Present (Diag.Sub_Diagnostics) then
         declare

            Sub_Diag : Sub_Diagnostic_Type;

            It : Iterator := Iterate (Diag.Sub_Diagnostics);
         begin
            while Has_Next (It) loop
               Next (It, Sub_Diag);

               Print_Sub_Diagnostic (Sub_Diag, Diag);
            end loop;
         end;
      end if;

   end Print_Diagnostic;
end Diagnostics.Brief_Emitter;
