------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . C A S E _ U T I L                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1995-2026, Free Software Foundation, Inc.         --
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

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore. Postconditions and
--  contract cases should not be executed at runtime as well, in order not to
--  slow down the execution of these functions.

--  The portion of this package that does not require use of the secondary
--  stack (so all the subprograms except functions that return String)
--  has been moved into a sibling package, Case_Util_NSS. See comments there.
--  Clients who don't care about avoiding secondary stack usage can
--  continue to use this package and are unaffected by this reorganization.

pragma Assertion_Policy (Pre            => Ignore,
                         Post           => Ignore,
                         Contract_Cases => Ignore,
                         Ghost          => Ignore);

with System.Case_Util_NSS;

package System.Case_Util
  with Pure, SPARK_Mode
is
   --  Note: all the following functions handle the full Latin-1 set

   function To_Upper (A : Character) return Character
     renames Case_Util_NSS.To_Upper;
   --  Converts A to upper case if it is a lower case letter, otherwise
   --  returns the input argument unchanged.

   procedure To_Upper (A : in out String) renames Case_Util_NSS.To_Upper;

   function To_Upper (A : String) return String
   with
     Post => To_Upper'Result'First = A'First
       and then To_Upper'Result'Last = A'Last
       and then (for all J in A'Range =>
                   To_Upper'Result (J) = To_Upper (A (J)));
   --  Folds all characters of string A to upper case

   function To_Lower (A : Character) return Character
     renames Case_Util_NSS.To_Lower;
   --  Converts A to lower case if it is an upper case letter, otherwise
   --  returns the input argument unchanged.

   procedure To_Lower (A : in out String)
     renames Case_Util_NSS.To_Lower;

   function To_Lower (A : String) return String
   with
     Post => To_Lower'Result'First = A'First
       and then To_Lower'Result'Last = A'Last
       and then (for all J in A'Range =>
                   To_Lower'Result (J) = To_Lower (A (J)));
   --  Folds all characters of string A to lower case

   procedure To_Mixed (A : in out String)
     renames Case_Util_NSS.To_Mixed;

   function To_Mixed (A : String) return String
   with
     Post => To_Mixed'Result'First = A'First
       and then To_Mixed'Result'Last = A'Last
       and then (for all J in A'Range =>
                   (if J = A'First
                      or else A (J - 1) = '_'
                    then
                      To_Mixed'Result (J) = To_Upper (A (J))
                    else
                      To_Mixed'Result (J) = To_Lower (A (J))));
   --  Converts A to mixed case (i.e. lower case, except for initial
   --  character and any character after an underscore, which are
   --  converted to upper case.

end System.Case_Util;
