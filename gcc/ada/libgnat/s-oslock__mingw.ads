------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                       S Y S T E M . O S _ L O C K S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2024-2025, Free Software Foundation, Inc.          --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is a NT (native) version of this package

with Interfaces.C;
with System.Win32;

package System.OS_Locks is
   pragma Preelaborate;

   type CRITICAL_SECTION is limited private;

   subtype RTS_Lock is CRITICAL_SECTION;
   --  Should be used inside the runtime system. The difference between Lock
   --  and the RTS_Lock is that the later one serves only as a semaphore so
   --  that do not check for ceiling violations.

private

   type CRITICAL_SECTION is record
      DebugInfo : System.Address;

      LockCount      : Long_Integer;
      RecursionCount : Long_Integer;
      OwningThread   : Win32.HANDLE;
      --  The above three fields control entering and exiting the critical
      --  section for the resource.

      LockSemaphore : Win32.HANDLE;
      SpinCount     : Interfaces.C.size_t;
   end record;

end System.OS_Locks;
