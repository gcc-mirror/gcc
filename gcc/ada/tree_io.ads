------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              T R E E _ I O                               --
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

--  This package contains the routines used to read and write the tree files
--  used by ASIS. Only the actual read and write routines are here. The open,
--  create and close routines are elsewhere (in Osint in the compiler, and in
--  the tree read driver for the tree read interface).

with Types;  use Types;
with System; use System;

pragma Warnings (Off);
--  This package is used also by gnatcoll
with System.OS_Lib; use System.OS_Lib;
pragma Warnings (On);

package Tree_IO is

   Tree_Format_Error : exception;
   --  Raised if a format error is detected in the input file

   ASIS_Version_Number : constant := 31;
   --  ASIS Version. This is used to check for consistency between the compiler
   --  used to generate trees and an ASIS application that is reading the
   --  trees. It must be incremented whenever a change is made to the tree
   --  format that would result in the compiler being incompatible with an
   --  older version of ASIS.
   --
   --  27  Changes in the tree structures for expression functions
   --  28  Changes in Snames
   --  29  Changes in Sem_Ch3 (tree copying in case of discriminant constraint
   --      for concurrent types).
   --  30  Add Check_Float_Overflow boolean to tree file
   --  31  Remove read/write of Debug_Pragmas_Disabled/Debug_Pragmas_Enabled

   procedure Tree_Read_Initialize (Desc : File_Descriptor);
   --  Called to initialize reading of a tree file. This call must be made
   --  before calls to Tree_Read_xx. No calls to Tree_Write_xx are permitted
   --  after this call.

   procedure Tree_Read_Data (Addr : Address; Length : Int);
   --  Checks that the Length provided is the same as what has been provided
   --  to the corresponding Tree_Write_Data from the current tree file,
   --  Tree_Format_Error is raised if it is not the case. If Length is
   --  correct and non zero, reads Length bytes of information into memory
   --  starting at Addr from the current tree file.

   procedure Tree_Read_Bool (B : out Boolean);
   --  Reads a single boolean value. The boolean value must have been written
   --  with a call to the Tree_Write_Bool procedure.

   procedure Tree_Read_Char (C : out Character);
   --  Reads a single character. The character must have been written with a
   --  call to the Tree_Write_Char procedure.

   procedure Tree_Read_Int (N : out Int);
   --  Reads a single integer value. The integer must have been written with
   --  a call to the Tree_Write_Int procedure.

   procedure Tree_Read_Str (S : out String_Ptr);
   --  Read string, allocate on heap, and return pointer to allocated string
   --  which always has a lower bound of 1.

   procedure Tree_Read_Terminate;
   --  Called after reading all data, checks that the buffer pointers is at
   --  the end of file, raising Tree_Format_Error if not.

   procedure Tree_Write_Initialize (Desc : File_Descriptor);
   --  Called to initialize writing of a tree file. This call must be made
   --  before calls to Tree_Write_xx. No calls to Tree_Read_xx are permitted
   --  after this call.

   procedure Tree_Write_Data (Addr : Address; Length : Int);
   --  Writes Length then, if Length is not null, Length bytes of data
   --  starting at Addr to current tree file

   procedure Tree_Write_Bool (B : Boolean);
   --  Writes a single boolean value to the current tree file

   procedure Tree_Write_Char (C : Character);
   --  Writes a single character to the current tree file

   procedure Tree_Write_Int (N : Int);
   --  Writes a single integer value to the current tree file

   procedure Tree_Write_Str (S : String_Ptr);
   --  Write out string value referenced by S (low bound of S must be 1)

   procedure Tree_Write_Terminate;
   --  Terminates writing of the file (flushing the buffer), but does not
   --  close the file (the caller is responsible for closing the file).

end Tree_IO;
