------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               S Y S T E M . A T O M I C _ P R I M I T I V E S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--              Copyright (C) 2012, Free Software Foundation, Inc.          --
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

package body System.Atomic_Primitives is

   ---------------------------
   -- Lock_Free_Try_Write_8 --
   ---------------------------

   function Lock_Free_Try_Write_8
      (Ptr      : Address;
       Expected : in out uint8;
       Desired  : uint8) return Boolean
   is
      Actual : uint8;

   begin
      if Expected /= Desired then
         Actual := Atomic_Compare_Exchange_8 (Ptr, Expected, Desired);

         if Actual /= Expected then
            Expected := Actual;
            return False;
         end if;
      end if;

      return True;
   end Lock_Free_Try_Write_8;

   ----------------------------
   -- Lock_Free_Try_Write_16 --
   ----------------------------

   function Lock_Free_Try_Write_16
      (Ptr      : Address;
       Expected : in out uint16;
       Desired  : uint16) return Boolean
   is
      Actual : uint16;

   begin
      if Expected /= Desired then
         Actual := Atomic_Compare_Exchange_16 (Ptr, Expected, Desired);

         if Actual /= Expected then
            Expected := Actual;
            return False;
         end if;
      end if;

      return True;
   end Lock_Free_Try_Write_16;

   ----------------------------
   -- Lock_Free_Try_Write_32 --
   ----------------------------

   function Lock_Free_Try_Write_32
      (Ptr      : Address;
       Expected : in out uint32;
       Desired  : uint32) return Boolean
   is
      Actual : uint32;

   begin
      if Expected /= Desired then
         Actual := Atomic_Compare_Exchange_32 (Ptr, Expected, Desired);

         if Actual /= Expected then
            Expected := Actual;
            return False;
         end if;
      end if;

      return True;
   end Lock_Free_Try_Write_32;

   ----------------------------
   -- Lock_Free_Try_Write_64 --
   ----------------------------

   function Lock_Free_Try_Write_64
      (Ptr      : Address;
       Expected : in out uint64;
       Desired  : uint64) return Boolean
   is
      Actual : uint64;

   begin
      if Expected /= Desired then
         Actual := Atomic_Compare_Exchange_64 (Ptr, Expected, Desired);

         if Actual /= Expected then
            Expected := Actual;
            return False;
         end if;
      end if;

      return True;
   end Lock_Free_Try_Write_64;
end System.Atomic_Primitives;
