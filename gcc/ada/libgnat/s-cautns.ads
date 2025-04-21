------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . C A S E _ U T I L _ N S S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1995-2025, Free Software Foundation, Inc.         --
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

--  The No_Secondary_Stack portion of System.Case_Util. Some of the functions
--  provided in System.Case_Util make use of the secondary stack, and some
--  do not. Lumping them all together makes even the non-secondary-stack
--  portion of the package unusable in cases where references to
--  secondary-stack-related code must be avoided (for example, if linking with
--  a reduced version of the runtimes where that code is missing). That's a
--  problem in some cases, so Case_Util is split into two parts. The first
--  part (named Case_Util_NSS) is a subset of the original version which
--  does not use the secondary stack; the second part presents the same
--  complete interface to users as before, but avoids code duplication by
--  renaming entities out of the first part.
--
--  See comments in s-casuti.ads for further explanations (e.g., of
--  the Assertion_Policy specified here).

pragma Assertion_Policy (Pre            => Ignore,
                         Post           => Ignore,
                         Contract_Cases => Ignore,
                         Ghost          => Ignore);

package System.Case_Util_NSS
  with Pure, SPARK_Mode
is
   --  Note: all the following functions handle the full Latin-1 set

   function To_Upper (A : Character) return Character
   with
     Post => (declare
                A_Val : constant Natural := Character'Pos (A);
              begin
                (if A in 'a' .. 'z'
                   or else A_Val in 16#E0# .. 16#F6#
                   or else A_Val in 16#F8# .. 16#FE#
                 then
                   To_Upper'Result = Character'Val (A_Val - 16#20#)
                 else
                   To_Upper'Result = A));
   --  Converts A to upper case if it is a lower case letter, otherwise
   --  returns the input argument unchanged.

   procedure To_Upper (A : in out String)
   with
     Post => (for all J in A'Range => A (J) = To_Upper (A'Old (J)));

   function To_Lower (A : Character) return Character
   with
     Post => (declare
                A_Val : constant Natural := Character'Pos (A);
              begin
                (if A in 'A' .. 'Z'
                   or else A_Val in 16#C0# .. 16#D6#
                   or else A_Val in 16#D8# .. 16#DE#
                 then
                   To_Lower'Result = Character'Val (A_Val + 16#20#)
                 else
                   To_Lower'Result = A));
   --  Converts A to lower case if it is an upper case letter, otherwise
   --  returns the input argument unchanged.

   procedure To_Lower (A : in out String)
   with
     Post => (for all J in A'Range => A (J) = To_Lower (A'Old (J)));

   procedure To_Mixed (A : in out String)
   with
     Post =>
       (for all J in A'Range =>
          (if J = A'First
             or else A'Old (J - 1) = '_'
           then
             A (J) = To_Upper (A'Old (J))
           else
             A (J) = To_Lower (A'Old (J))));

end System.Case_Util_NSS;
