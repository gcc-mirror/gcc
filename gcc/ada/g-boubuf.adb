------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  G N A T . B O U N D E D _ B U F F E R S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2003-2006, AdaCore                     --
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
