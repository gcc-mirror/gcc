------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  A D A . S T R I N G S . B O U N D E D                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

package body Ada.Strings.Bounded is

   package body Generic_Bounded_Length is

      --  The subprograms in this body are those for which there is no
      --  Bounded_String input, and hence no implicit information on the
      --  maximum size. This means that the maximum size has to be passed
      --  explicitly to the routine in Superbounded.

      ---------
      -- "*" --
      ---------

      function "*"
        (Left  : Natural;
         Right : Character) return Bounded_String
      is
      begin
         return Times (Left, Right, Max_Length);
      end "*";

      function "*"
        (Left  : Natural;
         Right : String) return Bounded_String
      is
      begin
         return Times (Left, Right, Max_Length);
      end "*";

      -----------------
      -- From_String --
      -----------------

      function From_String (Source : String) return Bounded_String is
      begin
         return To_Super_String (Source, Max_Length, Error);
      end From_String;

      ---------------
      -- Replicate --
      ---------------

      function Replicate
        (Count : Natural;
         Item  : Character;
         Drop  : Strings.Truncation := Strings.Error) return Bounded_String
      is
      begin
         return Super_Replicate (Count, Item, Drop, Max_Length);
      end Replicate;

      function Replicate
        (Count : Natural;
         Item  : String;
         Drop  : Strings.Truncation := Strings.Error) return Bounded_String
      is
      begin
         return Super_Replicate (Count, Item, Drop, Max_Length);
      end Replicate;

      -----------------------
      -- To_Bounded_String --
      -----------------------

      function To_Bounded_String
        (Source : String;
         Drop   : Strings.Truncation := Strings.Error) return Bounded_String
      is
      begin
         return To_Super_String (Source, Max_Length, Drop);
      end To_Bounded_String;

   end Generic_Bounded_Length;

end Ada.Strings.Bounded;
