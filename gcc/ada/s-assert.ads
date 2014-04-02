------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUN-TIME COMPONENTS                        --
--                                                                          --
--                    S Y S T E M . A S S E R T I O N S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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

--  This package provides support for assertions (including pragma Assert,
--  pragma Debug, and Precondition/Postcondition/Predicate/Invariant aspects
--  and their corresponding pragmas).

--  This unit may be used directly from an application program by providing
--  an appropriate WITH, and the interface can be expected to remain stable.

pragma Compiler_Unit_Warning;

package System.Assertions is

   Assert_Failure : exception;
   --  Exception raised when assertion fails

   procedure Raise_Assert_Failure (Msg : String);
   pragma No_Return (Raise_Assert_Failure);
   --  Called to raise Assert_Failure with given message

end System.Assertions;
