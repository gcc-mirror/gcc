------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--               A D A . C O N T A I N E R S . H E L P E R S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2015-2024, Free Software Foundation, Inc.        --
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
------------------------------------------------------------------------------

package body Ada.Containers.Helpers is

   Max_Count : constant := 2**31 - 1;
   --  Used in assertions below, to make sure the counts don't wrap around.
   --  This can help detect bugs in which Adjust and Finalize calls are
   --  improperly generated. An extra Decrement could otherwise cause
   --  wraparound from 0 to 2**32-1. The highest count seen so far is
   --  around 25, so this should be plenty.

   package body Generic_Implementation is

      ------------
      -- Adjust --
      ------------

      procedure Adjust (Control : in out Reference_Control_Type) is
      begin
         if Control.T_Counts /= null then
            Busy (Control.T_Counts.all);
         end if;
      end Adjust;

      ----------
      -- Busy --
      ----------

      procedure Busy (T_Counts : in out Tamper_Counts) is
      begin
         if T_Check then
            SAC.Increment (T_Counts.Busy);
            pragma Assert (T_Counts.Busy <= Max_Count);
         end if;
      end Busy;

      --------------
      -- Finalize --
      --------------

      procedure Finalize (Control : in out Reference_Control_Type) is
      begin
         if Control.T_Counts /= null then
            Unbusy (Control.T_Counts.all);
            Control.T_Counts := null;
         end if;
      end Finalize;

      --  No need to protect against double Finalize here, because these types
      --  are limited.

      procedure Finalize (Busy : in out With_Busy) is
         pragma Warnings (Off);
         pragma Assert (T_Check); -- not called if check suppressed
         pragma Warnings (On);
      begin
         Unbusy (Busy.T_Counts.all);
      end Finalize;

      procedure Finalize (Lock : in out With_Lock) is
         pragma Warnings (Off);
         pragma Assert (T_Check); -- not called if check suppressed
         pragma Warnings (On);
      begin
         Unlock (Lock.T_Counts.all);
      end Finalize;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (Busy : in out With_Busy) is
         pragma Warnings (Off);
         pragma Assert (T_Check); -- not called if check suppressed
         pragma Warnings (On);
      begin
         Generic_Implementation.Busy (Busy.T_Counts.all);
      end Initialize;

      procedure Initialize (Lock : in out With_Lock) is
         pragma Warnings (Off);
         pragma Assert (T_Check); -- not called if check suppressed
         pragma Warnings (On);
      begin
         Generic_Implementation.Lock (Lock.T_Counts.all);
      end Initialize;

      ----------
      -- Lock --
      ----------

      procedure Lock (T_Counts : in out Tamper_Counts) is
      begin
         if T_Check then
            SAC.Increment (T_Counts.Lock);
            pragma Assert (T_Counts.Lock <= Max_Count);
            SAC.Increment (T_Counts.Busy);
            pragma Assert (T_Counts.Busy <= Max_Count);
         end if;
      end Lock;

      --------------
      -- TC_Check --
      --------------

      procedure TC_Check (T_Counts : Tamper_Counts) is
      begin
         if T_Check then
            if Is_Busy (T_Counts) then
               raise Program_Error with
                 "attempt to tamper with cursors";
            end if;

            --  The lock status (which monitors "element tampering") always
            --  implies that the busy status (which monitors "cursor
            --  tampering") is set too; this is a representation invariant.
            --  Thus if the busy count is zero, then the lock count
            --  must also be zero.

            pragma Assert (not Is_Locked (T_Counts));
         end if;
      end TC_Check;

      --------------
      -- TE_Check --
      --------------

      procedure TE_Check (T_Counts : Tamper_Counts) is
      begin
         if T_Check and then Is_Locked (T_Counts) then
            raise Program_Error with
              "attempt to tamper with elements";
         end if;
      end TE_Check;

      ------------
      -- Unbusy --
      ------------

      procedure Unbusy (T_Counts : in out Tamper_Counts) is
      begin
         if T_Check then
            SAC.Decrement (T_Counts.Busy);
            pragma Assert (T_Counts.Busy <= Max_Count);
         end if;
      end Unbusy;

      ------------
      -- Unlock --
      ------------

      procedure Unlock (T_Counts : in out Tamper_Counts) is
      begin
         if T_Check then
            SAC.Decrement (T_Counts.Lock);
            pragma Assert (T_Counts.Lock <= Max_Count);
            SAC.Decrement (T_Counts.Busy);
            pragma Assert (T_Counts.Busy <= Max_Count);
         end if;
      end Unlock;

      -----------------
      -- Zero_Counts --
      -----------------

      procedure Zero_Counts (T_Counts : out Tamper_Counts) is
      begin
         if T_Check then
            T_Counts := (others => <>);
         end if;
      end Zero_Counts;

   end Generic_Implementation;

end Ada.Containers.Helpers;
