------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 S Y S T E M . S T A C K _ C H E C K I N G                --
--                                                                          --
--                                  B o d y                                 --
--                              (Dummy version)                             --
--                                                                          --
--                             $Revision: 1.1 $
--                                                                          --
--               Copyright (C) 2000 Ada Core Technologies, Inc.             --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

package body System.Stack_Checking is

   -----------------
   -- Stack_Check --
   -----------------

   function Stack_Check (Stack_Address : System.Address) return Stack_Access is
   begin
      return null;
   end Stack_Check;

   ----------------------------
   -- Invalidate_Stack_Cache --
   ----------------------------

   procedure Invalidate_Stack_Cache (Any_Stack : Stack_Access) is
   begin
      null;
   end Invalidate_Stack_Cache;

   --------------------
   -- Set_Stack_Size --
   --------------------

   --  Specify the stack size for the current frame.

   procedure Set_Stack_Size
     (Stack_Size : System.Storage_Elements.Storage_Offset) is
   begin
      null;
   end Set_Stack_Size;

   ------------------------
   -- Update_Stack_Cache --
   ------------------------

   procedure Update_Stack_Cache (Stack : Stack_Access) is
   begin
      null;
   end Update_Stack_Cache;

end System.Stack_Checking;
