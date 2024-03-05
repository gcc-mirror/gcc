------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                       S Y S T E M . O S _ L O C K S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2024, Free Software Foundation, Inc.            --
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

--  This is the HP-UX version of this package

with Interfaces.C;
with System.OS_Constants;

package System.OS_Locks is
   pragma Preelaborate;

   type pthread_mutex_t is limited private;

   subtype RTS_Lock is pthread_mutex_t;
   --  Should be used inside the runtime system. The difference between Lock
   --  and the RTS_Lock is that the latter serves only as a semaphore so that
   --  we do not check for ceiling violations.

private

   type cma_t_address is new System.Address;

   type cma_t_handle is record
      field1 : cma_t_address;
      field2 : Short_Integer;
      field3 : Short_Integer;
   end record;
   for cma_t_handle'Size use 64;

   type pthread_mutex_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_mutex_t);

end System.OS_Locks;
