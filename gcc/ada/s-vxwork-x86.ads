------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                        S Y S T E M . V X W O R K S                       --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--            Copyright (C) 1998-2009 Free Software Foundation, Inc.        --
--                                                                          --
-- GNARL is free software;  you can  redistribute it and/or modify it under --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is the x86 VxWorks version of this package

package System.VxWorks is
   pragma Preelaborate;

   --  Floating point context record. x86 version

   --  There are two kinds of FP_CONTEXT for this architecture, corresponding
   --  to newer and older processors. The type is defined in fppI86lib.h as a
   --  union. The form used depends on the versions of the save and restore
   --  routines that are selected by the user (these versions are provided in
   --  vxwork.ads). Since we do not examine the contents of these objects, it
   --  is sufficient to declare the type as of the required size: 512 bytes.

   type FP_CONTEXT is array (1 .. 128) of Integer;
   for FP_CONTEXT'Alignment use 4;
   for FP_CONTEXT'Size use 512 * Storage_Unit;
   pragma Convention (C, FP_CONTEXT);

   Num_HW_Interrupts : constant := 256;
   --  Number of entries in hardware interrupt vector table

end System.VxWorks;
