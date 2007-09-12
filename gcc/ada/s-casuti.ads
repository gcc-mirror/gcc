------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . C A S E _ U T I L                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1995-2007, Free Software Foundation, Inc.         --
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

--  Simple casing functions

--  This package provides simple casing functions that do not require the
--  overhead of the full casing tables found in Ada.Characters.Handling.

--  Note that all the routines in this package are available to the user
--  via GNAT.Case_Util, which imports all the entities from this package.

pragma Warnings (Off);
pragma Compiler_Unit;
pragma Warnings (On);

package System.Case_Util is
   pragma Pure;

   --  Note: all the following functions handle the full Latin-1 set

   function To_Upper (A : Character) return Character;
   --  Converts A to upper case if it is a lower case letter, otherwise
   --  returns the input argument unchanged.

   procedure To_Upper (A : in out String);
   --  Folds all characters of string A to upper csae

   function To_Lower (A : Character) return Character;
   --  Converts A to lower case if it is an upper case letter, otherwise
   --  returns the input argument unchanged.

   procedure To_Lower (A : in out String);
   --  Folds all characters of string A to lower case

   procedure To_Mixed (A : in out String);
   --  Converts A to mixed case (i.e. lower case, except for initial
   --  character and any character after an underscore, which are
   --  converted to upper case.

end System.Case_Util;
