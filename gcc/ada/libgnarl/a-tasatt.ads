------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  A D A . T A S K _ A T T R I B U T E S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2014-2017, Free Software Foundation, Inc.       --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

with Ada.Task_Identification;

generic
   type Attribute is private;
   Initial_Value : Attribute;

package Ada.Task_Attributes is

   --  Note that this package will use an efficient implementation with no
   --  locks and no extra dynamic memory allocation if Attribute is the size
   --  of either Integer or System.Address, and Initial_Value is 0 (null for
   --  an access type).

   --  Other types and initial values are supported, but will require
   --  the use of locking and a level of indirection (meaning extra dynamic
   --  memory allocation).

   --  The maximum number of task attributes supported by this implementation
   --  is determined by the constant System.Parameters.Max_Attribute_Count.
   --  If you exceed this number, Storage_Error will be raised during the
   --  elaboration of the instantiation of this package.

   type Attribute_Handle is access all Attribute;

   function Value
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task) return Attribute;
   --  Return the value of the corresponding attribute of T. Tasking_Error
   --  is raised if T is terminated and Program_Error will be raised if T
   --  is Null_Task_Id.

   function Reference
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task) return Attribute_Handle;
   --  Return an access value that designates the corresponding attribute of
   --  T. Tasking_Error is raised if T is terminated and Program_Error will be
   --  raised if T is Null_Task_Id.

   procedure Set_Value
     (Val : Attribute;
      T   : Ada.Task_Identification.Task_Id :=
              Ada.Task_Identification.Current_Task);
   --  Finalize the old value of the attribute of T and assign Val to that
   --  attribute. Tasking_Error is raised if T is terminated and Program_Error
   --  will be raised if T is Null_Task_Id.

   procedure Reinitialize
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task);
   --  Same as Set_Value (Initial_Value, T). Tasking_Error is raised if T is
   --  terminated and Program_Error will be raised if T is Null_Task_Id.

private
   pragma Inline (Value);
   pragma Inline (Reference);
   pragma Inline (Set_Value);
   pragma Inline (Reinitialize);
end Ada.Task_Attributes;
