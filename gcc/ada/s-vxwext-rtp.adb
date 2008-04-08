------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                     S Y S T E M . V X W O R K S . E X T                  --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--            Copyright (C) 2008, Free Software Foundation, Inc.            --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
