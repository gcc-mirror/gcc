------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     G N A T . M E M O R Y _ D U M P                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2003-2020, AdaCore                     --
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

--  A routine for dumping memory to either standard output or standard error.
--  Uses GNAT.IO for actual output (use the controls in GNAT.IO to specify
--  the destination of the output, which by default is Standard_Output).

with System;

package GNAT.Memory_Dump is
   pragma Preelaborate;

   type Prefix_Type is (Absolute_Address, Offset, None);

   procedure Dump
     (Addr   : System.Address;
      Count  : Natural);
   --  Dumps indicated number (Count) of bytes, starting at the address given
   --  by Addr. The coding of this routine in its current form assumes the case
   --  of a byte addressable machine (and is therefore inapplicable to machines
   --  like the AAMP, where the storage unit is not 8 bits). The output is one
   --  or more lines in the following format, which is for the case of 32-bit
   --  addresses (64-bit addresses are handled appropriately):
   --
   --    0234_3368: 66 67 68 . . .  73 74 75 "fghijklmnopqstuv"
   --
   --  All but the last line have 16 bytes. A question mark is used in the
   --  string data to indicate a non-printable character.

   procedure Dump
     (Addr   : System.Address;
      Count  : Natural;
      Prefix : Prefix_Type);
   --  Same as above, but allows the selection of different line formats.
   --  If Prefix is set to Absolute_Address, the output is identical to the
   --  above version, each line starting with the absolute address of the
   --  first dumped storage element.
   --
   --  If Prefix is set to Offset, then instead each line starts with the
   --  indication of the offset relative to Addr:
   --
   --    00: 66 67 68 . . .  73 74 75 "fghijklmnopqstuv"
   --
   --  Finally if Prefix is set to None, the prefix is suppressed altogether,
   --  and only the memory contents are displayed:
   --
   --    66 67 68 . . .  73 74 75 "fghijklmnopqstuv"

end GNAT.Memory_Dump;
