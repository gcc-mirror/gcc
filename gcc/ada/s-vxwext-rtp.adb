------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                     S Y S T E M . V X W O R K S . E X T                  --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--            Copyright (C) 2009, Free Software Foundation, Inc.            --
--                                                                          --
-- GNARL is free software;  you can redistribute it  and/or modify it under --
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
------------------------------------------------------------------------------

--  This is the VxWorks 6 rtp version of this package

package body System.VxWorks.Ext is

   function Task_Cont (tid : t_id) return int is
      pragma Unreferenced (tid);
   begin
      --  Operation not allowed in an RTP
      return 0;
   end Task_Cont;

   function Task_Stop (tid : t_id) return int is
      pragma Unreferenced (tid);
   begin
      --  Operation not allowed in an RTP
      return 0;
   end Task_Stop;

   function Int_Lock return int is
   begin
      --  Operation not allowed in an RTP
      return 0;
   end Int_Lock;

   function Int_Unlock return int is
   begin
      --  Operation not allowed in an RTP
      return 0;
   end Int_Unlock;

   function Set_Time_Slice (ticks : int) return int is
      pragma Unreferenced (ticks);
   begin
      return 0;
   end Set_Time_Slice;

end System.VxWorks.Ext;
