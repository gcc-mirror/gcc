------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       A D A . A S S E R T I O N S                        --
--                                                                          --
--            Copyright (C) 2015-2021, Free Software Foundation, Inc.       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the contracts that have been added.                      --
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
-- Extensive contributions were provided by Ada Core Technologies Inc. --
--                                                                          --
------------------------------------------------------------------------------

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised when calling Assert.
--  This is enforced by setting the corresponding assertion policy to Ignore.

pragma Assertion_Policy (Pre => Ignore);

pragma Compiler_Unit_Warning;

package Ada.Assertions with
  SPARK_Mode, Pure
is
   Assertion_Error : exception;

   procedure Assert (Check : Boolean) with
     Pre => Check;

   procedure Assert (Check : Boolean; Message : String) with
     Pre => Check;

end Ada.Assertions;
