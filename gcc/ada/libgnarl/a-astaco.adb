------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--        A D A . A S Y N C H R O N O U S _ T A S K _ C O N T R O L         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

--  This is a dummy body, which will not normally be compiled when used with
--  standard versions of GNAT, which do not support this package. See comments
--  in spec for further details.

package body Ada.Asynchronous_Task_Control is

   --------------
   -- Continue --
   --------------

   procedure Continue (T : Ada.Task_Identification.Task_Id) is
   begin
      null;
   end Continue;

   ----------
   -- Hold --
   ----------

   procedure Hold (T : Ada.Task_Identification.Task_Id) is
   begin
      raise Program_Error;
   end Hold;

   -------------
   -- Is_Held --
   -------------

   function Is_Held (T : Ada.Task_Identification.Task_Id) return Boolean is
   begin
      return False;
   end Is_Held;

end Ada.Asynchronous_Task_Control;
