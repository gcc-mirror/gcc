------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               S Y S T E M . A T O M I C _ P R I M I T I V E S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--              Copyright (C) 2012-2025, Free Software Foundation, Inc.     --
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

   --------------------
   -- Lock_Free_Read --
   --------------------

   function Lock_Free_Read (Ptr : Address) return Atomic_Type is
      function My_Atomic_Load is new Atomic_Load (Atomic_Type);

   begin
      if Atomic_Type'Atomic_Always_Lock_Free then
         return My_Atomic_Load (Ptr, Acquire);
      else
         raise Program_Error;
      end if;
   end Lock_Free_Read;

   -------------------------
   -- Lock_Free_Try_Write --
   -------------------------

   function Lock_Free_Try_Write
      (Ptr      : Address;
       Expected : in out Atomic_Type;
       Desired  : Atomic_Type) return Boolean
   is
      function My_Atomic_Compare_Exchange is
        new Atomic_Compare_Exchange (Atomic_Type);

   begin
      pragma Annotate (Gnatcheck, Exempt_On, "Improper_Returns",
                       "early returns for performance");

      if Expected /= Desired then
         if Atomic_Type'Atomic_Always_Lock_Free then
            return My_Atomic_Compare_Exchange (Ptr, Expected'Address, Desired);
         else
            raise Program_Error;
         end if;
      end if;

      return True;

      pragma Annotate (Gnatcheck, Exempt_Off, "Improper_Returns");
   end Lock_Free_Try_Write;

end System.Atomic_Primitives;
