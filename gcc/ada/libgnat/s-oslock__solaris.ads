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

--  This is a Solaris (native) version of this package

with Ada.Unchecked_Conversion;
with Interfaces.C;

package System.OS_Locks is
   pragma Preelaborate;

   type mutex_t is limited private;

   type Private_Task_Serial_Number is mod 2 ** Long_Long_Integer'Size;
   --  Used to give each task a unique serial number

   type Owner_ID is access all Integer;

   function To_Owner_ID is
     new Ada.Unchecked_Conversion (System.Address, Owner_ID);

   type RTS_Lock;
   type RTS_Lock_Ptr is access all RTS_Lock;

   type RTS_Lock is record
      L              : aliased mutex_t;
      Ceiling        : System.Any_Priority := System.Any_Priority'First;
      Saved_Priority : System.Any_Priority := System.Any_Priority'First;
      Owner          : Owner_ID;
      Next           : RTS_Lock_Ptr;
      Level          : Private_Task_Serial_Number := 0;
      Buddy          : Owner_ID;
      Frozen         : Boolean := False;
   end record;

private

   type array_type_9 is array (0 .. 3) of Interfaces.C.unsigned_char;
   type record_type_3 is record
      flag  : array_type_9;
      Xtype : Interfaces.C.unsigned_long;
   end record;
   pragma Convention (C, record_type_3);

   type upad64_t is new Interfaces.Unsigned_64;

   type mutex_t is record
      flags : record_type_3;
      lock  : upad64_t;
      data  : upad64_t;
   end record;
   pragma Convention (C, mutex_t);

end System.OS_Locks;
