------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . S T R I N G S . W I D E _ B O U N D E D              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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

package body Ada.Strings.Wide_Bounded is

   package body Generic_Bounded_Length is

      ---------
      -- "*" --
      ---------

      function "*"
        (Left  : Natural;
         Right : Wide_Character) return Bounded_Wide_String
      is
      begin
         return Times (Left, Right, Max_Length);
      end "*";

      function "*"
        (Left  : Natural;
         Right : Wide_String) return Bounded_Wide_String
      is
      begin
         return Times (Left, Right, Max_Length);
      end "*";

      ---------------
      -- Replicate --
      ---------------

      function Replicate
        (Count : Natural;
         Item  : Wide_Character;
         Drop  : Strings.Truncation := Strings.Error)
         return Bounded_Wide_String
      is
      begin
         return Super_Replicate (Count, Item, Drop, Max_Length);
      end Replicate;

      function Replicate
        (Count : Natural;
         Item  : Wide_String;
         Drop  : Strings.Truncation := Strings.Error)
         return Bounded_Wide_String
      is
      begin
         return Super_Replicate (Count, Item, Drop, Max_Length);
      end Replicate;

      ----------------------------
      -- To_Bounded_Wide_String --
      ----------------------------

      function To_Bounded_Wide_String
        (Source : Wide_String;
         Drop   : Strings.Truncation := Strings.Error)
         return Bounded_Wide_String
      is
      begin
         return To_Super_String (Source, Max_Length, Drop);
      end To_Bounded_Wide_String;

   end Generic_Bounded_Length;
end Ada.Strings.Wide_Bounded;
