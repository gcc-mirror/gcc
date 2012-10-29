------------------------------------------------------------------------------
--                                                                          --
--                        GNAAMP COMPILER COMPONENTS                        --
--                                                                          --
--                              A A _ U T I L                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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
------------------------------------------------------------------------------

with Sem_Aux; use Sem_Aux;
with Sinput;  use Sinput;
with Stand;   use Stand;
with Stringt; use Stringt;

with GNAT.Case_Util;  use GNAT.Case_Util;

package body AA_Util is

   ----------------------
   -- Is_Global_Entity --
   ----------------------

   function Is_Global_Entity (E : Entity_Id) return Boolean is
   begin
      return Enclosing_Dynamic_Scope (E) = Standard_Standard;
   end Is_Global_Entity;

   -----------------
   -- New_Name_Id --
   -----------------

   function New_Name_Id (Name : String) return Name_Id is
   begin
      for J in 1 .. Name'Length loop
         Name_Buffer (J) := Name (Name'First + (J - 1));
      end loop;

      Name_Len := Name'Length;
      return Name_Find;
   end New_Name_Id;

   -----------------
   -- Name_String --
   -----------------

   function Name_String (Name : Name_Id) return String is
   begin
      pragma Assert (Name /= No_Name);
      return Get_Name_String (Name);
   end Name_String;

   -------------------
   -- New_String_Id --
   -------------------

   function New_String_Id (S : String) return String_Id is
   begin
      for J in 1 .. S'Length loop
         Name_Buffer (J) := S (S'First + (J - 1));
      end loop;

      Name_Len := S'Length;
      return String_From_Name_Buffer;
   end New_String_Id;

   ------------------
   -- String_Value --
   ------------------

   function String_Value (Str_Id : String_Id) return String is
   begin
      --  ??? pragma Assert (Str_Id /= No_String);

      if Str_Id = No_String then
         return "";
      end if;

      String_To_Name_Buffer (Str_Id);

      return Name_Buffer (1 .. Name_Len);
   end String_Value;

   ---------------
   -- Next_Name --
   ---------------

   function Next_Name
     (Name_Seq    : not null access Name_Sequencer;
      Name_Prefix : String) return Name_Id
   is
   begin
      Name_Seq.Sequence_Number := Name_Seq.Sequence_Number + 1;

      declare
         Number_Image : constant String := Name_Seq.Sequence_Number'Img;
      begin
         return New_Name_Id
                  (Name_Prefix & "__" & Number_Image (2 .. Number_Image'Last));
      end;
   end Next_Name;

   --------------------
   -- Elab_Spec_Name --
   --------------------

   function Elab_Spec_Name (Module_Name : Name_Id) return Name_Id is
   begin
      return New_Name_Id (Name_String (Module_Name) & "___elabs");
   end Elab_Spec_Name;

   --------------------
   -- Elab_Spec_Name --
   --------------------

   function Elab_Body_Name (Module_Name : Name_Id) return Name_Id is
   begin
      return New_Name_Id (Name_String (Module_Name) & "___elabb");
   end Elab_Body_Name;

   --------------------------------
   -- Source_Name_Without_Suffix --
   --------------------------------

   function File_Name_Without_Suffix (File_Name : String) return String is
      Name_Index : Natural := File_Name'Last;

   begin
      pragma Assert (File_Name'Length > 0);

      --  We loop in reverse to ensure that file names that follow nonstandard
      --  naming conventions that include additional dots are handled properly,
      --  preserving dots in front of the main file suffix (for example,
      --  main.2.ada => main.2).

      while Name_Index >= File_Name'First
        and then File_Name (Name_Index) /= '.'
      loop
         Name_Index := Name_Index - 1;
      end loop;

      --  Return the part of the file name up to but not including the last dot
      --  in the name, or return the whole name as is if no dot character was
      --  found.

      if Name_Index >= File_Name'First then
         return File_Name (File_Name'First .. Name_Index - 1);

      else
         return File_Name;
      end if;
   end File_Name_Without_Suffix;

   -----------------
   -- Source_Name --
   -----------------

   function Source_Name (Sloc : Source_Ptr) return File_Name_Type is
   begin
      if Sloc = No_Location or Sloc = Standard_Location then
         return No_File;
      else
         return File_Name (Get_Source_File_Index (Sloc));
      end if;
   end Source_Name;

   --------------------------------
   -- Source_Name_Without_Suffix --
   --------------------------------

   function Source_Name_Without_Suffix (Sloc : Source_Ptr) return String is
      Src_Name  : constant String :=
        Name_String (Name_Id (Source_Name (Sloc)));
      Src_Index : Natural         := Src_Name'Last;

   begin
      pragma Assert (Src_Name'Length > 0);

      --  Treat the presence of a ".dg" suffix specially, stripping it off
      --  in addition to any suffix preceding it.

      if Src_Name'Length >= 4
        and then Src_Name (Src_Name'Last - 2 .. Src_Name'Last) = ".dg"
      then
         Src_Index := Src_Index - 3;
      end if;

      return File_Name_Without_Suffix (Src_Name (Src_Name'First .. Src_Index));
   end Source_Name_Without_Suffix;

   ----------------------
   -- Source_Id_String --
   ----------------------

   function Source_Id_String (Unit_Name : Name_Id) return String is
      Unit_String : String   := Name_String (Unit_Name);
      Name_Last   : Positive := Unit_String'Last;
      Name_Index  : Positive := Unit_String'First;

   begin
      To_Mixed (Unit_String);

      --  Replace any embedded sequences of two or more '_' characters
      --  with a single '.' character. Note that this will leave any
      --  leading or trailing single '_' characters untouched, but those
      --  should normally not occur in compilation unit names (and if
      --  they do then it's better to leave them as is).

      while Name_Index <= Name_Last loop
         if Unit_String (Name_Index) = '_'
           and then Name_Index /= Name_Last
           and then Unit_String (Name_Index + 1) = '_'
         then
            Unit_String (Name_Index) := '.';
            Name_Index := Name_Index + 1;

            while Unit_String (Name_Index) = '_'
              and then Name_Index <= Name_Last
            loop
               Unit_String (Name_Index .. Name_Last - 1)
                 := Unit_String (Name_Index + 1 .. Name_Last);
               Name_Last := Name_Last - 1;
            end loop;

         else
            Name_Index := Name_Index + 1;
         end if;
      end loop;

      return Unit_String (Unit_String'First .. Name_Last);
   end Source_Id_String;

   --  This version of Source_Id_String is obsolescent and is being
   --  replaced with the above function.

   function Source_Id_String (Sloc : Source_Ptr) return String is
      File_Index : Source_File_Index;

   begin
      --  Use an arbitrary artificial 22-character value for package Standard,
      --  since Standard doesn't have an associated source file.

      if Sloc <= Standard_Location then
         return "20010101010101standard";

      --  Return the concatentation of the source file's timestamp and
      --  its 8-digit hex checksum.

      else
         File_Index := Get_Source_File_Index (Sloc);

         return String (Time_Stamp (File_Index))
                  & Get_Hex_String (Source_Checksum (File_Index));
      end if;
   end Source_Id_String;

   ---------------
   -- Source_Id --
   ---------------

   function Source_Id (Unit_Name : Name_Id) return String_Id is
   begin
      return New_String_Id (Source_Id_String (Unit_Name));
   end Source_Id;

   --  This version of Source_Id is obsolescent and is being
   --  replaced with the above function.

   function Source_Id (Sloc : Source_Ptr) return String_Id is
   begin
      return New_String_Id (Source_Id_String (Sloc));
   end Source_Id;

   -----------
   -- Image --
   -----------

   function Image (I : Int) return String is
      Image_String : constant String := Pos'Image (I);
   begin
      if Image_String (1) = ' ' then
         return Image_String (2 .. Image_String'Last);
      else
         return Image_String;
      end if;
   end Image;

   --------------
   -- UI_Image --
   --------------

   function UI_Image (I : Uint; Format : Integer_Image_Format) return String is
   begin
      if Format = Decimal then
         UI_Image (I, Format => Decimal);
         return UI_Image_Buffer (1 .. UI_Image_Length);

      elsif Format = Ada_Hex then
         UI_Image (I, Format => Hex);
         return UI_Image_Buffer (1 .. UI_Image_Length);

      else
         pragma Assert (I >= Uint_0);

         UI_Image (I, Format => Hex);

         pragma Assert (UI_Image_Buffer (1 .. 3) = "16#"
                         and then UI_Image_Buffer (UI_Image_Length) = '#');

         --  Declare a string where we will copy the digits from the UI_Image,
         --  interspersing '_' characters as 4-digit group separators. The
         --  underscores in UI_Image's result are not always at the places
         --  where we want them, which is why we do the following copy
         --  (e.g., we map "16#ABCD_EF#" to "^AB_CDEF^").

         declare
            Hex_String     : String (1 .. UI_Image_Max);
            Last_Index     : Natural;
            Digit_Count    : Natural := 0;
            UI_Image_Index : Natural := 4; -- Skip past the "16#" bracket
            Sep_Count      : Natural := 0;

         begin
            --  Count up the number of non-underscore characters in the
            --  literal value portion of the UI_Image string.

            while UI_Image_Buffer (UI_Image_Index) /= '#' loop
               if UI_Image_Buffer (UI_Image_Index) /= '_' then
                  Digit_Count := Digit_Count + 1;
               end if;

               UI_Image_Index := UI_Image_Index + 1;
            end loop;

            UI_Image_Index := 4; -- Reset the index past the "16#" bracket

            Last_Index := 1;

            Hex_String (Last_Index) := '^';
            Last_Index := Last_Index + 1;

            --  Copy digits from UI_Image_Buffer to Hex_String, adding
            --  underscore separators as appropriate. The initial value
            --  of Sep_Count accounts for the leading '^' and being one
            --  character ahead after inserting a digit.

            Sep_Count := 2;

            while UI_Image_Buffer (UI_Image_Index) /= '#' loop
               if UI_Image_Buffer (UI_Image_Index) /= '_' then
                  Hex_String (Last_Index) := UI_Image_Buffer (UI_Image_Index);

                  Last_Index := Last_Index + 1;

                  --  Add '_' characters to separate groups of four hex
                  --  digits for readability (grouping from right to left).

                  if (Digit_Count - (Last_Index - Sep_Count)) mod 4 = 0 then
                     Hex_String (Last_Index) := '_';
                     Last_Index := Last_Index + 1;
                     Sep_Count := Sep_Count + 1;
                  end if;
               end if;

               UI_Image_Index := UI_Image_Index + 1;
            end loop;

            --  Back up before any trailing underscore

            if Hex_String (Last_Index - 1) = '_' then
               Last_Index := Last_Index - 1;
            end if;

            Hex_String (Last_Index) := '^';

            return Hex_String (1 .. Last_Index);
         end;
      end if;
   end UI_Image;

   --------------
   -- UR_Image --
   --------------

   --  Shouldn't this be added to Urealp???

   function UR_Image (R : Ureal) return String is

      --  The algorithm used here for conversion of Ureal values
      --  is taken from the JGNAT back end.

      Num    : Long_Long_Float := 0.0;
      Den    : Long_Long_Float := 0.0;
      Sign   : Long_Long_Float := 1.0;
      Result : Long_Long_Float;
      Tmp    : Uint;
      Index  : Integer;

   begin
      if UR_Is_Negative (R) then
         Sign := -1.0;
      end if;

      --  In the following calculus, we consider numbers modulo 2 ** 31,
      --  so that we don't have problems with signed Int...

      Tmp := abs (Numerator (R));
      Index := 0;
      while Tmp > 0 loop
         Num := Num
           + Long_Long_Float (UI_To_Int (Tmp mod (Uint_2 ** 31)))
           * (2.0 ** Index);
         Tmp := Tmp / Uint_2 ** 31;
         Index := Index + 31;
      end loop;

      Tmp := abs (Denominator (R));
      if Rbase (R) /= 0 then
         Tmp := Rbase (R) ** Tmp;
      end if;

      Index := 0;
      while Tmp > 0 loop
         Den := Den
           + Long_Long_Float (UI_To_Int (Tmp mod (Uint_2 ** 31)))
           * (2.0 ** Index);
         Tmp := Tmp / Uint_2 ** 31;
         Index := Index + 31;
      end loop;

      --  If the denominator denotes a negative power of Rbase,
      --  then multiply by the denominator.

      if Rbase (R) /= 0 and then Denominator (R) < 0 then
         Result := Sign * Num * Den;

      --  Otherwise compute the quotient

      else
         Result := Sign * Num / Den;
      end if;

      return Long_Long_Float'Image (Result);
   end UR_Image;

end AA_Util;
