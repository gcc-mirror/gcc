------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . M E M O R Y _ C O P Y                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001-2014, Free Software Foundation, Inc.         --
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

--  This package provides general block copy mechanisms analogous to those
--  provided by the C routines memcpy and memmove allowing for copies with
--  and without possible overlap of the operands.

--  The idea is to allow a configurable run-time to provide this capability
--  for use by the compiler without dragging in C-run time routines.

with System.CRTL;
--  The above with is contrary to the intent ???

package System.Memory_Copy is
   pragma Preelaborate;

   procedure memcpy (S1 : Address; S2 : Address; N : System.CRTL.size_t)
     renames System.CRTL.memcpy;
   --  Copies N storage units from area starting at S2 to area starting
   --  at S1 without any check for buffer overflow. The memory areas
   --  must not overlap, or the result of this call is undefined.

   procedure memmove (S1 : Address; S2 : Address; N : System.CRTL.size_t)
      renames System.CRTL.memmove;
   --  Copies N storage units from area starting at S2 to area starting
   --  at S1 without any check for buffer overflow. The difference between
   --  this memmove and memcpy is that with memmove, the storage areas may
   --  overlap (forwards or backwards) and the result is correct (i.e. it
   --  is as if S2 is first moved to a temporary area, and then this area
   --  is copied to S1 in a separate step).

   --  In the standard library, these are just interfaced to the C routines.
   --  But in the HI-E (high integrity version) they may be reprogrammed to
   --  meet certification requirements (and marked High_Integrity).

   --  Note that in high integrity mode these routines are by default not
   --  available, and the HI-E compiler will as a result generate implicit
   --  loops (which will violate the restriction No_Implicit_Loops).

end System.Memory_Copy;
