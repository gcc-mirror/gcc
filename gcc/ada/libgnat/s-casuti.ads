------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . C A S E _ U T I L                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1995-2021, Free Software Foundation, Inc.         --
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

--  Simple casing functions

--  This package provides simple casing functions that do not require the
--  overhead of the full casing tables found in Ada.Characters.Handling.

--  Note that all the routines in this package are available to the user
--  via GNAT.Case_Util, which imports all the entities from this package.

pragma Compiler_Unit_Warning;

package System.Case_Util is
   pragma Pure;

   --  Note: all the following functions handle the full Latin-1 set

   function To_Upper (A : Character) return Character;
   --  Converts A to upper case if it is a lower case letter, otherwise
   --  returns the input argument unchanged.

   procedure To_Upper (A : in out String);
   function To_Upper (A : String) return String;
   --  Folds all characters of string A to upper case

   function To_Lower (A : Character) return Character;
   --  Converts A to lower case if it is an upper case letter, otherwise
   --  returns the input argument unchanged.

   procedure To_Lower (A : in out String);
   function To_Lower (A : String) return String;
   --  Folds all characters of string A to lower case

   procedure To_Mixed (A : in out String);
   function To_Mixed (A : String) return String;
   --  Converts A to mixed case (i.e. lower case, except for initial
   --  character and any character after an underscore, which are
   --  converted to upper case.

end System.Case_Util;
