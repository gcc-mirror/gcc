------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--         S Y S T E M . T A S K I N G . T A S K _ A T T R I B U T E S      --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--                     Copyright (C) 1995-2010, AdaCore                     --
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

--  This package provides support for the body of Ada.Task_Attributes

with Ada.Finalization;

with System.Storage_Elements;

package System.Tasking.Task_Attributes is

   type Attribute is new Integer;
   --  A stand-in for the generic formal type of Ada.Task_Attributes
   --  in the following declarations.

   type Node;
   type Access_Node is access all Node;
   --  This needs comments ???

   function To_Access_Node is new Ada.Unchecked_Conversion
     (Access_Address, Access_Node);
   --  Used to fetch pointer to indirect attribute list. Declaration is in
   --  spec to avoid any problems with aliasing assumptions.

   type Dummy_Wrapper;
   type Access_Dummy_Wrapper is access all Dummy_Wrapper;
   pragma No_Strict_Aliasing (Access_Dummy_Wrapper);
   --  Needed to avoid possible incorrect aliasing situations from
   --  instantiation of Unchecked_Conversion in body of Ada.Task_Attributes.

   for Access_Dummy_Wrapper'Storage_Size use 0;
   --  Access_Dummy_Wrapper is a stand-in for the generic type Wrapper defined
   --  in Ada.Task_Attributes. The real objects allocated are always
   --  of type Wrapper, no Dummy_Wrapper objects are ever created.

   type Deallocator is access procedure (P : in out Access_Node);
   --  Called to deallocate an Wrapper. P is a pointer to a Node within

   type Instance;

   type Access_Instance is access all Instance;

   type Instance is new Ada.Finalization.Limited_Controlled with record
      Deallocate    : Deallocator;
      Initial_Value : aliased System.Storage_Elements.Integer_Address;

      Index : Direct_Index;
      --  The index of the TCB location used by this instantiation, if it is
      --  stored in the TCB, otherwise zero.

      Next : Access_Instance;
      --  Next instance in All_Attributes list
   end record;

   procedure Finalize (X : in out Instance);

   type Node is record
      Wrapper  : Access_Dummy_Wrapper;
      Instance : Access_Instance;
      Next     : Access_Node;
   end record;

   --  The following type is a stand-in for the actual wrapper type, which is
   --  different for each instantiation of Ada.Task_Attributes.

   type Dummy_Wrapper is record
      Dummy_Node : aliased Node;

      Value : aliased Attribute;
      --  The generic formal type, may be controlled
   end record;

   for Dummy_Wrapper'Alignment use Standard'Maximum_Alignment;
   --  A number of unchecked conversions involving Dummy_Wrapper_Access
   --  sources are performed in other units (e.g. Ada.Task_Attributes).
   --  Ensure that the designated object is always strictly enough aligned.

   In_Use : Direct_Index_Vector := 0;
   --  Set True for direct indexes that are already used (True??? type???)

   All_Attributes : Access_Instance;
   --  A linked list of all indirectly access attributes, which includes all
   --  those that require finalization.

   procedure Initialize_Attributes (T : Task_Id);
   --  Initialize all attributes created via Ada.Task_Attributes for T. This
   --  must be called by the creator of the task, inside Create_Task, via
   --  soft-link Initialize_Attributes_Link. On entry, abort must be deferred
   --  and the caller must hold no locks

   procedure Finalize_Attributes (T : Task_Id);
   --  Finalize all attributes created via Ada.Task_Attributes for T.
   --  This is to be called by the task after it is marked as terminated
   --  (and before it actually dies), inside Vulnerable_Free_Task, via the
   --  soft-link Finalize_Attributes_Link. On entry, abort must be deferred
   --  and T.L must be write-locked.

end System.Tasking.Task_Attributes;
