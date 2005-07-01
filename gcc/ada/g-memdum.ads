------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     G N A T . M E M O R Y _ D U M P                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2003-2005 AdaCore                      --
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

--  A routine for dumping memory to either standard output or standard error.
--  Uses GNAT.IO for actual output (use the controls in GNAT.IO to specify
--  the destination of the output, which by default is Standard_Output).

with System;

package GNAT.Memory_Dump is
pragma Preelaborate (Memory_Dump);

   procedure Dump (Addr : System.Address; Count : Natural);
   --  Dumps indicated number (Count) of bytes, starting at the address given
   --  by Addr. The coding of this routine in its current form assumes the
   --  case of a byte addressable machine (and is therefore inapplicable to
   --  machines like the AAMP, where the storage unit is not 8 bits). The
   --  output is one or more lines in the following format, which is for the
   --  case of 32-bit addresses (64-bit addressea are handled appropriately):
   --
   --    0234_3368: 66 67 68 . . .  73 74 75 "fghijklmnopqstuv"
   --
   --  All but the last line have 16 bytes. A question mark is used in the
   --  string data to indicate a non-printable character.

end GNAT.Memory_Dump;
