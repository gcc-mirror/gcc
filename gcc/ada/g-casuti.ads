------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       G N A T . C A S E _ U T I L                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 1995-2002 Ada Core Technologies, Inc.            --
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

--  Simple casing functions

--  This package provides simple casing functions that do not require the
--  overhead of the full casing tables found in Ada.Characters.Handling.

--  Note: actual code is found in System.Case_Util, which is used internally
--  by the GNAT run time. Applications programs should always use this package
--  rather than using System.Case_Util directly.

with System.Case_Util;

package GNAT.Case_Util is
pragma Pure (Case_Util);

pragma Elaborate_Body;
--  The elaborate body is because we have a dummy body to deal with bootstrap
--  path problems (we used to have a real body, and now we don't need it any
--  more, but the bootstrap requires that we have a dummy body, since otherwise
--  the old body gets picked up.

   --  Note: all the following functions handle the full Latin-1 set

   function To_Upper (A : Character) return Character
     renames System.Case_Util.To_Upper;
   --  Converts A to upper case if it is a lower case letter, otherwise
   --  returns the input argument unchanged.

   procedure To_Upper (A : in out String)
     renames System.Case_Util.To_Upper;
   --  Folds all characters of string A to upper csae

   function To_Lower (A : Character) return Character
     renames System.Case_Util.To_Lower;
   --  Converts A to lower case if it is an upper case letter, otherwise
   --  returns the input argument unchanged.

   procedure To_Lower (A : in out String)
     renames System.Case_Util.To_Lower;
   --  Folds all characters of string A to lower case

   procedure To_Mixed (A : in out String)
     renames System.Case_Util.To_Mixed;
   --  Converts A to mixed case (i.e. lower case, except for initial
   --  character and any character after an underscore, which are
   --  converted to upper case.

end GNAT.Case_Util;
