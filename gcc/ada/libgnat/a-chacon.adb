------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           A D A . C H A R A C T E R S . C O N V E R S I O N S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2005-2024, Free Software Foundation, Inc.         --
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

package body Ada.Characters.Conversions is

   ------------------
   -- Is_Character --
   ------------------

   function Is_Character (Item : Wide_Character) return Boolean is
   begin
      return Wide_Character'Pos (Item) < 256;
   end Is_Character;

   function Is_Character (Item : Wide_Wide_Character) return Boolean is
   begin
      return Wide_Wide_Character'Pos (Item) < 256;
   end Is_Character;

   ---------------
   -- Is_String --
   ---------------

   function Is_String (Item : Wide_String) return Boolean is
   begin
      for J in Item'Range loop
         if Wide_Character'Pos (Item (J)) >= 256 then
            return False;
         end if;
      end loop;

      return True;
   end Is_String;

   function Is_String (Item : Wide_Wide_String) return Boolean is
   begin
      for J in Item'Range loop
         if Wide_Wide_Character'Pos (Item (J)) >= 256 then
            return False;
         end if;
      end loop;

      return True;
   end Is_String;

   -----------------------
   -- Is_Wide_Character --
   -----------------------

   function Is_Wide_Character (Item : Wide_Wide_Character) return Boolean is
   begin
      return Wide_Wide_Character'Pos (Item) < 2**16;
   end Is_Wide_Character;

   --------------------
   -- Is_Wide_String --
   --------------------

   function Is_Wide_String (Item : Wide_Wide_String) return Boolean is
   begin
      for J in Item'Range loop
         if Wide_Wide_Character'Pos (Item (J)) >= 2**16 then
            return False;
         end if;
      end loop;

      return True;
   end Is_Wide_String;

   ------------------
   -- To_Character --
   ------------------

   function To_Character
     (Item       : Wide_Character;
      Substitute : Character := ' ') return Character
   is
   begin
      if Is_Character (Item) then
         return Character'Val (Wide_Character'Pos (Item));
      else
         return Substitute;
      end if;
   end To_Character;

   function To_Character
     (Item       : Wide_Wide_Character;
      Substitute : Character := ' ') return Character
   is
   begin
      if Is_Character (Item) then
         return Character'Val (Wide_Wide_Character'Pos (Item));
      else
         return Substitute;
      end if;
   end To_Character;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Item       : Wide_String;
      Substitute : Character := ' ') return String
   is
      Result : String (1 .. Item'Length);

   begin
      for J in Item'Range loop
         Result (J - (Item'First - 1)) := To_Character (Item (J), Substitute);
      end loop;

      return Result;
   end To_String;

   function To_String
     (Item       : Wide_Wide_String;
      Substitute : Character := ' ') return String
   is
      Result : String (1 .. Item'Length);

   begin
      for J in Item'Range loop
         Result (J - (Item'First - 1)) := To_Character (Item (J), Substitute);
      end loop;

      return Result;
   end To_String;

   -----------------------
   -- To_Wide_Character --
   -----------------------

   function To_Wide_Character
     (Item : Character) return Wide_Character
   is
   begin
      return Wide_Character'Val (Character'Pos (Item));
   end To_Wide_Character;

   function To_Wide_Character
     (Item       : Wide_Wide_Character;
      Substitute : Wide_Character := ' ') return Wide_Character
   is
   begin
      if Wide_Wide_Character'Pos (Item) < 2**16 then
         return Wide_Character'Val (Wide_Wide_Character'Pos (Item));
      else
         return Substitute;
      end if;
   end To_Wide_Character;

   --------------------
   -- To_Wide_String --
   --------------------

   function To_Wide_String
     (Item : String) return Wide_String
   is
      Result : Wide_String (1 .. Item'Length);

   begin
      for J in Item'Range loop
         Result (J - (Item'First - 1)) := To_Wide_Character (Item (J));
      end loop;

      return Result;
   end To_Wide_String;

   function To_Wide_String
     (Item       : Wide_Wide_String;
      Substitute : Wide_Character := ' ') return Wide_String
   is
      Result : Wide_String (1 .. Item'Length);

   begin
      for J in Item'Range loop
         Result (J - (Item'First - 1)) :=
           To_Wide_Character (Item (J), Substitute);
      end loop;

      return Result;
   end To_Wide_String;

   ----------------------------
   -- To_Wide_Wide_Character --
   ----------------------------

   function To_Wide_Wide_Character
     (Item : Character) return Wide_Wide_Character
   is
   begin
      return Wide_Wide_Character'Val (Character'Pos (Item));
   end To_Wide_Wide_Character;

   function To_Wide_Wide_Character
     (Item : Wide_Character) return Wide_Wide_Character
   is
   begin
      return Wide_Wide_Character'Val (Wide_Character'Pos (Item));
   end To_Wide_Wide_Character;

   -------------------------
   -- To_Wide_Wide_String --
   -------------------------

   function To_Wide_Wide_String
     (Item : String) return Wide_Wide_String
   is
      Result : Wide_Wide_String (1 .. Item'Length);

   begin
      for J in Item'Range loop
         Result (J - (Item'First - 1)) := To_Wide_Wide_Character (Item (J));
      end loop;

      return Result;
   end To_Wide_Wide_String;

   function To_Wide_Wide_String
     (Item : Wide_String) return Wide_Wide_String
   is
      Result : Wide_Wide_String (1 .. Item'Length);

   begin
      for J in Item'Range loop
         Result (J - (Item'First - 1)) := To_Wide_Wide_Character (Item (J));
      end loop;

      return Result;
   end To_Wide_Wide_String;

end Ada.Characters.Conversions;
