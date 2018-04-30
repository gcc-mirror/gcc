------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   I N T E R F A C E S . F O R T R A N                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
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

package body Interfaces.Fortran is

   ------------
   -- To_Ada --
   ------------

   --  Single character case

   function To_Ada (Item : Character_Set) return Character is
   begin
      return Character (Item);
   end To_Ada;

   --  String case (function returning converted result)

   function To_Ada (Item : Fortran_Character) return String is
      T : String (1 .. Item'Length);

   begin
      for J in T'Range loop
         T (J) := Character (Item (J - 1 + Item'First));
      end loop;

      return T;
   end To_Ada;

   --  String case (procedure copying converted string to given buffer)

   procedure To_Ada
     (Item   : Fortran_Character;
      Target : out String;
      Last   : out Natural)
   is
   begin
      if Item'Length = 0 then
         Last := 0;
         return;

      elsif Target'Length = 0 then
         raise Constraint_Error;

      else
         Last := Target'First - 1;

         for J in Item'Range loop
            Last := Last + 1;

            if Last > Target'Last then
               raise Constraint_Error;
            else
               Target (Last) := Character (Item (J));
            end if;
         end loop;
      end if;
   end To_Ada;

   ----------------
   -- To_Fortran --
   ----------------

   --  Character case

   function To_Fortran (Item : Character) return Character_Set is
   begin
      return Character_Set (Item);
   end To_Fortran;

   --  String case (function returning converted result)

   function To_Fortran (Item : String) return Fortran_Character is
      T : Fortran_Character (1 .. Item'Length);

   begin
      for J in T'Range loop
         T (J) := Character_Set (Item (J - 1 + Item'First));
      end loop;

      return T;
   end To_Fortran;

   --  String case (procedure copying converted string to given buffer)

   procedure To_Fortran
     (Item   : String;
      Target : out Fortran_Character;
      Last   : out Natural)
   is
   begin
      if Item'Length = 0 then
         Last := 0;
         return;

      elsif Target'Length = 0 then
         raise Constraint_Error;

      else
         Last := Target'First - 1;

         for J in Item'Range loop
            Last := Last + 1;

            if Last > Target'Last then
               raise Constraint_Error;
            else
               Target (Last) := Character_Set (Item (J));
            end if;
         end loop;
      end if;
   end To_Fortran;

end Interfaces.Fortran;
