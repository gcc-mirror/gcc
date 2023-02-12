------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . E X C E P T I O N S . T R A C E B A C K             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2023, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  This package is part of the support for tracebacks on exceptions

with System.Traceback_Entries;

package Ada.Exceptions.Traceback is

   package STBE renames System.Traceback_Entries;

   subtype Code_Loc is System.Address;
   --  Code location in executing program

   subtype Tracebacks_Array is STBE.Tracebacks_Array;
   --  A traceback array is an array of traceback entries

   function Tracebacks (E : Exception_Occurrence) return Tracebacks_Array;
   --  This function extracts the traceback information from an exception
   --  occurrence, and returns it formatted in the manner required for
   --  processing in GNAT.Traceback. See g-traceb.ads for further details.

   function "=" (A, B : Tracebacks_Array) return Boolean renames STBE."=";
   --  Make "=" operator visible directly

   function Get_PC (TBE : STBE.Traceback_Entry) return Code_Loc
     renames STBE.PC_For;
   --  Returns the code address held by a given traceback entry, typically the
   --  address of a call instruction.

end Ada.Exceptions.Traceback;
