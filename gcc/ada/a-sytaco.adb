------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--         A D A . S Y N C H R O N O U S _ T A S K _ C O N T R O L          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--            Copyright (C) 1992-2001 Free Software Foundation, Inc.        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------


package body Ada.Synchronous_Task_Control is

   -------------------
   -- Suspension_PO --
   -------------------

   protected body Suspension_Object is

      --------------
      -- Get_Open --
      --------------

      function Get_Open return Boolean is
      begin
         return Open;
      end Get_Open;

      ---------------
      -- Set_False --
      ---------------

      procedure Set_False is
      begin
         Open := False;
      end Set_False;

      --------------
      -- Set_True --
      --------------

      procedure Set_True is
      begin
         Open := True;
      end Set_True;

      ----------
      -- Wait --
      ----------

      entry Wait when Open is
      begin
         Open := False;
      end Wait;

      --------------------
      -- Wait_Exception --
      --------------------

      entry Wait_Exception when True is
      begin
         if Wait'Count /= 0 then
            raise Program_Error;
         end if;

         requeue Wait;
      end Wait_Exception;

   end Suspension_Object;

   -------------------
   -- Current_State --
   -------------------

   function Current_State (S : Suspension_Object) return Boolean is
   begin
      return S.Get_Open;
   end Current_State;

   ---------------
   -- Set_False --
   ---------------

   procedure Set_False (S : in out Suspension_Object) is
   begin
      S.Set_False;
   end Set_False;

   --------------
   -- Set_True --
   --------------

   procedure Set_True (S : in out Suspension_Object) is
   begin
      S.Set_True;
   end Set_True;

   ------------------------
   -- Suspend_Until_True --
   ------------------------

   procedure Suspend_Until_True (S : in out Suspension_Object) is
   begin
      S.Wait_Exception;
   end Suspend_Until_True;

end Ada.Synchronous_Task_Control;
