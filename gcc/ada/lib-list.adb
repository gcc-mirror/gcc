------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . L I S T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2002 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Output; use Output;

separate (Lib)
procedure List (File_Names_Only : Boolean := False) is

   Num_Units : constant Nat := Int (Units.Last) - Int (Units.First) + 1;
   --  Number of units in file table

   Sorted_Units : Unit_Ref_Table (1 .. Num_Units);
   --  Table of unit numbers that we will sort

   Unit_Hed : constant String := "Unit name                        ";
   Unit_Und : constant String := "---------                        ";
   Unit_Bln : constant String := "                                 ";
   File_Hed : constant String := "File name                     ";
   File_Und : constant String := "---------                     ";
   File_Bln : constant String := "                              ";
   Time_Hed : constant String := "Time stamp";
   Time_Und : constant String := "----------";

   Unit_Length : constant Natural := Unit_Hed'Length;
   File_Length : constant Natural := File_Hed'Length;

begin
   --  First step is to make a sorted table of units

   for J in 1 .. Num_Units loop
      Sorted_Units (J) := Unit_Number_Type (Int (Units.First) + J - 1);
   end loop;

   Sort (Sorted_Units);

   --  Now we can generate the unit table listing

   Write_Eol;

   if not File_Names_Only then
      Write_Str (Unit_Hed);
      Write_Str (File_Hed);
      Write_Str (Time_Hed);
      Write_Eol;

      Write_Str (Unit_Und);
      Write_Str (File_Und);
      Write_Str (Time_Und);
      Write_Eol;
      Write_Eol;
   end if;

   for R in Sorted_Units'Range loop
      if File_Names_Only then
         if not Is_Internal_File_Name
                  (File_Name (Source_Index (Sorted_Units (R))))
         then
            Write_Name (Full_File_Name (Source_Index (Sorted_Units (R))));
            Write_Eol;
         end if;

      else
         Write_Unit_Name (Unit_Name (Sorted_Units (R)));

         if Name_Len > (Unit_Length - 1) then
            Write_Eol;
            Write_Str (Unit_Bln);
         else
            for J in Name_Len + 1 .. Unit_Length loop
               Write_Char (' ');
            end loop;
         end if;

         Write_Name (Full_File_Name (Source_Index (Sorted_Units (R))));

         if Name_Len > (File_Length - 1) then
            Write_Eol;
            Write_Str (Unit_Bln);
            Write_Str (File_Bln);
         else
            for J in Name_Len + 1 .. File_Length loop
               Write_Char (' ');
            end loop;
         end if;

         Write_Str (String (Time_Stamp (Source_Index (Sorted_Units (R)))));
         Write_Eol;
      end if;
   end loop;

   Write_Eol;
end List;
