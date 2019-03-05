------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    A D A . S T R I N G S . F I X E D                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Note: This code is derived from the ADAR.CSH public domain Ada 83 versions
--  of the Appendix C string handling packages. One change is to avoid the use
--  of Is_In, so that we are not dependent on inlining. Note that the search
--  function implementations are to be found in the auxiliary package
--  Ada.Strings.Search. Also the Move procedure is directly incorporated (ADAR
--  used a subunit for this procedure). The number of errors having to do with
--  bounds of function return results were also fixed, and use of & removed for
--  efficiency reasons.

with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Search;

package body Ada.Strings.Fixed is

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   renames Ada.Strings.Search.Index;

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   renames Ada.Strings.Search.Index;

   function Index
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural
   renames Ada.Strings.Search.Index;

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   renames Ada.Strings.Search.Index;

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   renames Ada.Strings.Search.Index;

   function Index
     (Source  : String;
      Set     : Maps.Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural
   renames Ada.Strings.Search.Index;

   function Index_Non_Blank
     (Source : String;
      Going  : Direction := Forward) return Natural
   renames Ada.Strings.Search.Index_Non_Blank;

   function Index_Non_Blank
     (Source : String;
      From   : Positive;
      Going  : Direction := Forward) return Natural
   renames Ada.Strings.Search.Index_Non_Blank;

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   renames Ada.Strings.Search.Count;

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function) return Natural
   renames Ada.Strings.Search.Count;

   function Count
     (Source : String;
      Set    : Maps.Character_Set) return Natural
   renames Ada.Strings.Search.Count;

   procedure Find_Token
     (Source : String;
      Set    : Maps.Character_Set;
      From   : Positive;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   renames Ada.Strings.Search.Find_Token;

   procedure Find_Token
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   renames Ada.Strings.Search.Find_Token;

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : Natural;
      Right : Character) return String
   is
      Result : String (1 .. Left);

   begin
      for J in Result'Range loop
         Result (J) := Right;
      end loop;

      return Result;
   end "*";

   function "*"
     (Left  : Natural;
      Right : String) return String
   is
      Result : String (1 .. Left * Right'Length);
      Ptr    : Integer := 1;

   begin
      for J in 1 .. Left loop
         Result (Ptr .. Ptr + Right'Length - 1) := Right;
         Ptr := Ptr + Right'Length;
      end loop;

      return Result;
   end "*";

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : String;
      From    : Positive;
      Through : Natural) return String
   is
   begin
      if From > Through then
         declare
            subtype Result_Type is String (1 .. Source'Length);

         begin
            return Result_Type (Source);
         end;

      elsif From not in Source'Range
        or else Through > Source'Last
      then
         raise Index_Error;

      else
         declare
            Front  : constant Integer := From - Source'First;
            Result : String (1 .. Source'Length - (Through - From + 1));

         begin
            Result (1 .. Front) :=
              Source (Source'First .. From - 1);
            Result (Front + 1 .. Result'Last) :=
              Source (Through + 1 .. Source'Last);

            return Result;
         end;
      end if;
   end Delete;

   procedure Delete
     (Source  : in out String;
      From    : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   is
   begin
      Move (Source  => Delete (Source, From, Through),
            Target  => Source,
            Justify => Justify,
            Pad     => Pad);
   end Delete;

   ----------
   -- Head --
   ----------

   function Head
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String
   is
      subtype Result_Type is String (1 .. Count);

   begin
      if Count < Source'Length then
         return
           Result_Type (Source (Source'First .. Source'First + Count - 1));

      else
         declare
            Result : Result_Type;

         begin
            Result (1 .. Source'Length) := Source;

            for J in Source'Length + 1 .. Count loop
               Result (J) := Pad;
            end loop;

            return Result;
         end;
      end if;
   end Head;

   procedure Head
     (Source  : in out String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   is
   begin
      Move (Source  => Head (Source, Count, Pad),
            Target  => Source,
            Drop    => Error,
            Justify => Justify,
            Pad     => Pad);
   end Head;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : String;
      Before   : Positive;
      New_Item : String) return String
   is
      Result : String (1 .. Source'Length + New_Item'Length);
      Front  : constant Integer := Before - Source'First;

   begin
      if Before not in Source'First .. Source'Last + 1 then
         raise Index_Error;
      end if;

      Result (1 .. Front) :=
        Source (Source'First .. Before - 1);
      Result (Front + 1 .. Front + New_Item'Length) :=
        New_Item;
      Result (Front + New_Item'Length + 1 .. Result'Last) :=
        Source (Before .. Source'Last);

      return Result;
   end Insert;

   procedure Insert
     (Source   : in out String;
      Before   : Positive;
      New_Item : String;
      Drop     : Truncation := Error)
   is
   begin
      Move (Source => Insert (Source, Before, New_Item),
            Target => Source,
            Drop   => Drop);
   end Insert;

   ----------
   -- Move --
   ----------

   procedure Move
     (Source  : String;
      Target  : out String;
      Drop    : Truncation := Error;
      Justify : Alignment  := Left;
      Pad     : Character  := Space)
   is
      Sfirst  : constant Integer := Source'First;
      Slast   : constant Integer := Source'Last;
      Slength : constant Integer := Source'Length;

      Tfirst  : constant Integer := Target'First;
      Tlast   : constant Integer := Target'Last;
      Tlength : constant Integer := Target'Length;

      function Is_Padding (Item : String) return Boolean;
      --  Check if Item is all Pad characters, return True if so, False if not

      function Is_Padding (Item : String) return Boolean is
      begin
         for J in Item'Range loop
            if Item (J) /= Pad then
               return False;
            end if;
         end loop;

         return True;
      end Is_Padding;

   --  Start of processing for Move

   begin
      if Slength = Tlength then
         Target := Source;

      elsif Slength > Tlength then
         case Drop is
            when Left =>
               Target := Source (Slast - Tlength + 1 .. Slast);

            when Right =>
               Target := Source (Sfirst .. Sfirst + Tlength - 1);

            when Error =>
               case Justify is
                  when Left =>
                     if Is_Padding (Source (Sfirst + Tlength .. Slast)) then
                        Target :=
                          Source (Sfirst .. Sfirst + Target'Length - 1);
                     else
                        raise Length_Error;
                     end if;

                  when Right =>
                     if Is_Padding (Source (Sfirst .. Slast - Tlength)) then
                        Target := Source (Slast - Tlength + 1 .. Slast);
                     else
                        raise Length_Error;
                     end if;

                  when Center =>
                     raise Length_Error;
               end case;
         end case;

      --  Source'Length < Target'Length

      else
         case Justify is
            when Left =>
               Target (Tfirst .. Tfirst + Slength - 1) := Source;

               for I in Tfirst + Slength .. Tlast loop
                  Target (I) := Pad;
               end loop;

            when Right =>
               for I in Tfirst .. Tlast - Slength loop
                  Target (I) := Pad;
               end loop;

               Target (Tlast - Slength + 1 .. Tlast) := Source;

            when Center =>
               declare
                  Front_Pad   : constant Integer := (Tlength - Slength) / 2;
                  Tfirst_Fpad : constant Integer := Tfirst + Front_Pad;

               begin
                  for I in Tfirst .. Tfirst_Fpad - 1 loop
                     Target (I) := Pad;
                  end loop;

                  Target (Tfirst_Fpad .. Tfirst_Fpad + Slength - 1) := Source;

                  for I in Tfirst_Fpad + Slength .. Tlast loop
                     Target (I) := Pad;
                  end loop;
               end;
         end case;
      end if;
   end Move;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : String;
      Position : Positive;
      New_Item : String) return String
   is
   begin
      if Position not in Source'First .. Source'Last + 1 then
         raise Index_Error;
      end if;

      declare
         Result_Length : constant Natural :=
           Integer'Max
             (Source'Length,
              Position - Source'First + New_Item'Length);

         Result : String (1 .. Result_Length);
         Front  : constant Integer := Position - Source'First;

      begin
         Result (1 .. Front) :=
           Source (Source'First .. Position - 1);
         Result (Front + 1 .. Front + New_Item'Length) :=
           New_Item;
         Result (Front + New_Item'Length + 1 .. Result'Length) :=
           Source (Position + New_Item'Length .. Source'Last);
         return Result;
      end;
   end Overwrite;

   procedure Overwrite
     (Source   : in out String;
      Position : Positive;
      New_Item : String;
      Drop     : Truncation := Right)
   is
   begin
      Move (Source => Overwrite (Source, Position, New_Item),
            Target => Source,
            Drop   => Drop);
   end Overwrite;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice
     (Source : String;
      Low    : Positive;
      High   : Natural;
      By     : String) return String
   is
   begin
      if Low > Source'Last + 1 or else High < Source'First - 1 then
         raise Index_Error;
      end if;

      if High >= Low then
         declare
            Front_Len : constant Integer :=
              Integer'Max (0, Low - Source'First);
            --  Length of prefix of Source copied to result

            Back_Len : constant Integer :=
              Integer'Max (0, Source'Last - High);
            --  Length of suffix of Source copied to result

            Result_Length : constant Integer :=
              Front_Len + By'Length + Back_Len;
            --  Length of result

            Result : String (1 .. Result_Length);

         begin
            Result (1 .. Front_Len) := Source (Source'First .. Low - 1);
            Result (Front_Len + 1 .. Front_Len + By'Length) := By;
            Result (Front_Len + By'Length + 1 .. Result'Length) :=
              Source (High + 1 .. Source'Last);
            return Result;
         end;

      else
         return Insert (Source, Before => Low, New_Item => By);
      end if;
   end Replace_Slice;

   procedure Replace_Slice
     (Source   : in out String;
      Low      : Positive;
      High     : Natural;
      By       : String;
      Drop     : Truncation := Error;
      Justify  : Alignment  := Left;
      Pad      : Character  := Space)
   is
   begin
      Move (Replace_Slice (Source, Low, High, By), Source, Drop, Justify, Pad);
   end Replace_Slice;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String
   is
      subtype Result_Type is String (1 .. Count);

   begin
      if Count < Source'Length then
         return Result_Type (Source (Source'Last - Count + 1 .. Source'Last));

      --  Pad on left

      else
         declare
            Result : Result_Type;

         begin
            for J in 1 .. Count - Source'Length loop
               Result (J) := Pad;
            end loop;

            Result (Count - Source'Length + 1 .. Count) := Source;
            return Result;
         end;
      end if;
   end Tail;

   procedure Tail
     (Source  : in out String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   is
   begin
      Move (Source  => Tail (Source, Count, Pad),
            Target  => Source,
            Drop    => Error,
            Justify => Justify,
            Pad     => Pad);
   end Tail;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Source  : String;
      Mapping : Maps.Character_Mapping) return String
   is
      Result : String (1 .. Source'Length);

   begin
      for J in Source'Range loop
         Result (J - (Source'First - 1)) := Value (Mapping, Source (J));
      end loop;

      return Result;
   end Translate;

   procedure Translate
     (Source  : in out String;
      Mapping : Maps.Character_Mapping)
   is
   begin
      for J in Source'Range loop
         Source (J) := Value (Mapping, Source (J));
      end loop;
   end Translate;

   function Translate
     (Source  : String;
      Mapping : Maps.Character_Mapping_Function) return String
   is
      Result : String (1 .. Source'Length);
      pragma Unsuppress (Access_Check);

   begin
      for J in Source'Range loop
         Result (J - (Source'First - 1)) := Mapping.all (Source (J));
      end loop;

      return Result;
   end Translate;

   procedure Translate
     (Source  : in out String;
      Mapping : Maps.Character_Mapping_Function)
   is
      pragma Unsuppress (Access_Check);
   begin
      for J in Source'Range loop
         Source (J) := Mapping.all (Source (J));
      end loop;
   end Translate;

   ----------
   -- Trim --
   ----------

   function Trim
     (Source : String;
      Side   : Trim_End) return String
   is
   begin
      case Side is
         when Strings.Left =>
            declare
               Low : constant Natural := Index_Non_Blank (Source, Forward);
            begin
               --  All blanks case

               if Low = 0 then
                  return "";
               end if;

               declare
                  subtype Result_Type is String (1 .. Source'Last - Low + 1);
               begin
                  return Result_Type (Source (Low .. Source'Last));
               end;
            end;

         when Strings.Right =>
            declare
               High : constant Natural := Index_Non_Blank (Source, Backward);
            begin
               --  All blanks case

               if High = 0 then
                  return "";
               end if;

               declare
                  subtype Result_Type is String (1 .. High - Source'First + 1);
               begin
                  return Result_Type (Source (Source'First .. High));
               end;
            end;

         when Strings.Both =>
            declare
               Low : constant Natural := Index_Non_Blank (Source, Forward);
            begin
               --  All blanks case

               if Low = 0 then
                  return "";
               end if;

               declare
                  High : constant Natural :=
                    Index_Non_Blank (Source, Backward);
                  subtype Result_Type is String (1 .. High - Low + 1);
               begin
                  return Result_Type (Source (Low .. High));
               end;
            end;
      end case;
   end Trim;

   procedure Trim
     (Source  : in out String;
      Side    : Trim_End;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   is
   begin
      Move (Trim (Source, Side),
            Source,
            Justify => Justify,
            Pad => Pad);
   end Trim;

   function Trim
     (Source : String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set) return String
   is
      High, Low : Integer;

   begin
      Low := Index (Source, Set => Left, Test  => Outside, Going => Forward);

      --  Case where source comprises only characters in Left

      if Low = 0 then
         return "";
      end if;

      High :=
        Index (Source, Set => Right, Test  => Outside, Going => Backward);

      --  Case where source comprises only characters in Right

      if High = 0 then
         return "";
      end if;

      declare
         subtype Result_Type is String (1 .. High - Low + 1);

      begin
         return Result_Type (Source (Low .. High));
      end;
   end Trim;

   procedure Trim
     (Source  : in out String;
      Left    : Maps.Character_Set;
      Right   : Maps.Character_Set;
      Justify : Alignment := Strings.Left;
      Pad     : Character := Space)
   is
   begin
      Move (Source  => Trim (Source, Left, Right),
            Target  => Source,
            Justify => Justify,
            Pad     => Pad);
   end Trim;

end Ada.Strings.Fixed;
