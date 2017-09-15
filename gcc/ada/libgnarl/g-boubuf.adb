------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  G N A T . B O U N D E D _ B U F F E R S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

package body GNAT.Bounded_Buffers is

   --------------------
   -- Bounded_Buffer --
   --------------------

   protected body Bounded_Buffer is

      ------------
      -- Insert --
      ------------

      entry Insert (Item : Element) when Count /= Capacity is
      begin
         Values (Next_In) := Item;
         Next_In := (Next_In mod Capacity) + 1;
         Count := Count + 1;
      end Insert;

      ------------
      -- Remove --
      ------------

      entry Remove (Item : out Element) when Count > 0 is
      begin
         Item := Values (Next_Out);
         Next_Out := (Next_Out mod Capacity) + 1;
         Count := Count - 1;
      end Remove;

      -----------
      -- Empty --
      -----------

      function Empty return Boolean is
      begin
         return Count = 0;
      end Empty;

      ----------
      -- Full --
      ----------

      function Full return Boolean is
      begin
         return Count = Capacity;
      end Full;

      ------------
      -- Extent --
      ------------

      function Extent return Natural is
      begin
         return Count;
      end Extent;

   end Bounded_Buffer;

end GNAT.Bounded_Buffers;
