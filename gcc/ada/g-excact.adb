------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              G N A T . E X C E P T I O N _ A C T I O N S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2003 Free Software Foundation, Inc.          --
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

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with System;
with System.Soft_Links;       use System.Soft_Links;
with System.Standard_Library; use System.Standard_Library;
with System.Exception_Table;  use System.Exception_Table;

package body GNAT.Exception_Actions is

   Global_Action : Exception_Action;
   pragma Import (C, Global_Action, "__gnat_exception_actions_global_action");
   --  Imported from Ada.Exceptions. Any change in the external name needs to
   --  be coordinated with a-except.adb

   Raise_Hook_Initialized : Boolean;
   pragma Import
     (Ada, Raise_Hook_Initialized, "__gnat_exception_actions_initialized");

   function To_Raise_Action is new Ada.Unchecked_Conversion
     (Exception_Action, Raise_Action);

   --  ??? Would be nice to have this in System.Standard_Library
   function To_Data is new Ada.Unchecked_Conversion
     (Exception_Id, Exception_Data_Ptr);
   function To_Id is new Ada.Unchecked_Conversion
     (Exception_Data_Ptr, Exception_Id);

   ----------------------------
   -- Register_Global_Action --
   ----------------------------

   procedure Register_Global_Action (Action : Exception_Action) is
   begin
      Lock_Task.all;
      Global_Action := Action;
      Unlock_Task.all;
   end Register_Global_Action;

   ------------------------
   -- Register_Id_Action --
   ------------------------

   procedure Register_Id_Action
     (Id     : Exception_Id;
      Action : Exception_Action)
   is
   begin
      if Id = Null_Id then
         raise Program_Error;
      end if;

      Lock_Task.all;
      To_Data (Id).Raise_Hook := To_Raise_Action (Action);
      Raise_Hook_Initialized := True;
      Unlock_Task.all;
   end Register_Id_Action;

   ---------------
   -- Core_Dump --
   ---------------

   procedure Core_Dump (Occurrence : Exception_Occurrence) is separate;

   ----------------
   -- Name_To_Id --
   ----------------

   function Name_To_Id (Name : String) return Exception_Id is
   begin
      return To_Id (Internal_Exception (Name, False));
   end Name_To_Id;

   ---------------------------------
   -- Registered_Exceptions_Count --
   ---------------------------------

   function Registered_Exceptions_Count return Natural renames
     System.Exception_Table.Registered_Exceptions_Count;

   -------------------------------
   -- Get_Registered_Exceptions --
   -------------------------------
   --  This subprogram isn't an iterator to avoid concurrency problems,
   --  since the exceptions are registered dynamically. Since we have to lock
   --  the runtime while computing this array, this means that any callback in
   --  an active iterator would be unable to access the runtime.

   procedure Get_Registered_Exceptions
     (List : out Exception_Id_Array;
      Last : out Integer)
   is
      Ids : Exception_Data_Array (List'Range);
   begin
      Get_Registered_Exceptions (Ids, Last);

      for L in List'First .. Last loop
         List (L) := To_Id (Ids (L));
      end loop;
   end Get_Registered_Exceptions;

end GNAT.Exception_Actions;
