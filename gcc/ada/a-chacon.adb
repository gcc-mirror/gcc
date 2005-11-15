------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C H A R A C T E R S . H A N D L I N G               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005, Free Software Foundation, Inc.            --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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
