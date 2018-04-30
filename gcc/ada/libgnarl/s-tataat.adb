------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--         S Y S T E M . T A S K I N G . T A S K _ A T T R I B U T E S      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 2014-2018, Free Software Foundation, Inc.         --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

with System.Parameters; use System.Parameters;
with System.Tasking.Initialization; use System.Tasking.Initialization;
with System.Task_Primitives.Operations;

package body System.Tasking.Task_Attributes is

   package STPO renames System.Task_Primitives.Operations;

   type Index_Info is record
      Used : Boolean;
      --  Used is True if a given index is used by an instantiation of
      --  Ada.Task_Attributes, False otherwise.

      Require_Finalization : Boolean;
      --  Require_Finalization is True if the attribute requires finalization
   end record;

   Index_Array : array (1 .. Max_Attribute_Count) of Index_Info :=
                   (others => (False, False));

   --  Note that this package will use an efficient implementation with no
   --  locks and no extra dynamic memory allocation if Attribute can fit in a
   --  System.Address type and Initial_Value is 0 (or null for an access type).

   function Next_Index (Require_Finalization : Boolean) return Integer is
      Self_Id : constant Task_Id := STPO.Self;

   begin
      Task_Lock (Self_Id);

      for J in Index_Array'Range loop
         if not Index_Array (J).Used then
            Index_Array (J).Used := True;
            Index_Array (J).Require_Finalization := Require_Finalization;
            Task_Unlock (Self_Id);
            return J;
         end if;
      end loop;

      Task_Unlock (Self_Id);
      raise Storage_Error with "Out of task attributes";
   end Next_Index;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Index : Integer) is
      Self_Id : constant Task_Id := STPO.Self;
   begin
      pragma Assert (Index in Index_Array'Range);
      Task_Lock (Self_Id);
      Index_Array (Index).Used := False;
      Task_Unlock (Self_Id);
   end Finalize;

   --------------------------
   -- Require_Finalization --
   --------------------------

   function Require_Finalization (Index : Integer) return Boolean is
   begin
      pragma Assert (Index in Index_Array'Range);
      return Index_Array (Index).Require_Finalization;
   end Require_Finalization;

end System.Tasking.Task_Attributes;
