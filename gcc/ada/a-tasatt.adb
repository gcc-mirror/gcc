------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                  A D A . T A S K _ A T T R I B U T E S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--                     Copyright (C) 1995-2014, AdaCore                     --
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

with System.Storage_Elements;
with System.Task_Primitives.Operations;
with System.Tasking;
with System.Tasking.Initialization;
with System.Tasking.Task_Attributes;

with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

pragma Elaborate_All (System.Tasking.Task_Attributes);
--  To ensure the initialization of object Local (below) will work

package body Ada.Task_Attributes is

   use System.Tasking.Initialization,
       System.Tasking,
       System.Tasking.Task_Attributes,
       Ada.Exceptions;

   package POP renames System.Task_Primitives.Operations;

   ---------------------------
   -- Unchecked Conversions --
   ---------------------------

   --  The following type corresponds to Dummy_Wrapper, declared in
   --  System.Tasking.Task_Attributes.

   type Wrapper;
   type Access_Wrapper is access all Wrapper;

   pragma Warnings (Off);
   --  We turn warnings off for the following To_Attribute_Handle conversions,
   --  since these are used only for small attributes where we know that there
   --  are no problems with alignment, but the compiler will generate warnings
   --  for the occurrences in the large attribute case, even though they will
   --  not actually be used.

   function To_Attribute_Handle is new Ada.Unchecked_Conversion
     (System.Address, Attribute_Handle);
   function To_Direct_Attribute_Element is new Ada.Unchecked_Conversion
     (System.Address, Direct_Attribute_Element);
   --  For reference to directly addressed task attributes

   type Access_Integer_Address is access all
     System.Storage_Elements.Integer_Address;

   function To_Attribute_Handle is new Ada.Unchecked_Conversion
     (Access_Integer_Address, Attribute_Handle);
   --  For reference to directly addressed task attributes

   pragma Warnings (On);
   --  End warnings off region for directly addressed attribute conversions

   function To_Access_Address is new Ada.Unchecked_Conversion
     (Access_Node, Access_Address);
   --  To store pointer to list of indirect attributes

   pragma Warnings (Off);
   function To_Access_Wrapper is new Ada.Unchecked_Conversion
     (Access_Dummy_Wrapper, Access_Wrapper);
   pragma Warnings (On);
   --  To fetch pointer to actual wrapper of attribute node. We turn off
   --  warnings since this may generate an alignment warning. The warning can
   --  be ignored since Dummy_Wrapper is only a non-generic standin for the
   --  real wrapper type (we never actually allocate objects of type
   --  Dummy_Wrapper).

   function To_Access_Dummy_Wrapper is new Ada.Unchecked_Conversion
     (Access_Wrapper, Access_Dummy_Wrapper);
   --  To store pointer to actual wrapper of attribute node

   function To_Task_Id is new Ada.Unchecked_Conversion
     (Task_Identification.Task_Id, Task_Id);
   --  To access TCB of identified task

   type Local_Deallocator is access procedure (P : in out Access_Node);

   function To_Lib_Level_Deallocator is new Ada.Unchecked_Conversion
     (Local_Deallocator, Deallocator);
   --  To defeat accessibility check

   ------------------------
   -- Storage Management --
   ------------------------

   procedure Deallocate (P : in out Access_Node);
   --  Passed to the RTS via unchecked conversion of a pointer to permit
   --  finalization and deallocation of attribute storage nodes.

   --------------------------
   -- Instantiation Record --
   --------------------------

   Local : aliased Instance;
   --  Initialized in package body

   type Wrapper is record
      Dummy_Node : aliased Node;

      Value : aliased Attribute := Initial_Value;
      --  The generic formal type, may be controlled
   end record;

   --  A number of unchecked conversions involving Wrapper_Access sources are
   --  performed in this unit. We have to ensure that the designated object is
   --  always strictly enough aligned.

   for Wrapper'Alignment use Standard'Maximum_Alignment;

   procedure Free is
      new Ada.Unchecked_Deallocation (Wrapper, Access_Wrapper);

   procedure Deallocate (P : in out Access_Node) is
      T : Access_Wrapper := To_Access_Wrapper (P.Wrapper);
   begin
      Free (T);
   end Deallocate;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (T    : Task_Identification.Task_Id := Task_Identification.Current_Task)
      return Attribute_Handle
   is
      TT            : constant Task_Id := To_Task_Id (T);
      Error_Message : constant String  := "Trying to get the reference of a ";

   begin
      if TT = null then
         Raise_Exception (Program_Error'Identity, Error_Message & "null task");
      end if;

      if TT.Common.State = Terminated then
         Raise_Exception (Tasking_Error'Identity,
           Error_Message & "terminated task");
      end if;

      --  Directly addressed case

      if Local.Index /= 0 then

         --  Return the attribute handle. Warnings off because this return
         --  statement generates alignment warnings for large attributes
         --  (but will never be executed in this case anyway).

         pragma Warnings (Off);
         return
           To_Attribute_Handle (TT.Direct_Attributes (Local.Index)'Address);
         pragma Warnings (On);

      --  Not directly addressed

      else
         declare
            P       : Access_Node := To_Access_Node (TT.Indirect_Attributes);
            W       : Access_Wrapper;
            Self_Id : constant Task_Id := POP.Self;

         begin
            Defer_Abort (Self_Id);
            POP.Lock_RTS;

            while P /= null loop
               if P.Instance = Access_Instance'(Local'Unchecked_Access) then
                  POP.Unlock_RTS;
                  Undefer_Abort (Self_Id);
                  return To_Access_Wrapper (P.Wrapper).Value'Access;
               end if;

               P := P.Next;
            end loop;

            --  Unlock the RTS here to follow the lock ordering rule that
            --  prevent us from using new (i.e the Global_Lock) while holding
            --  any other lock.

            POP.Unlock_RTS;
            W := new Wrapper'
                  ((null, Local'Unchecked_Access, null), Initial_Value);
            POP.Lock_RTS;

            P := W.Dummy_Node'Unchecked_Access;
            P.Wrapper := To_Access_Dummy_Wrapper (W);
            P.Next := To_Access_Node (TT.Indirect_Attributes);
            TT.Indirect_Attributes := To_Access_Address (P);
            POP.Unlock_RTS;
            Undefer_Abort (Self_Id);
            return W.Value'Access;

         exception
            when others =>
               POP.Unlock_RTS;
               Undefer_Abort (Self_Id);
               raise;
         end;
      end if;

   exception
      when Tasking_Error | Program_Error =>
         raise;

      when others =>
         raise Program_Error;
   end Reference;

   ------------------
   -- Reinitialize --
   ------------------

   procedure Reinitialize
     (T : Task_Identification.Task_Id := Task_Identification.Current_Task)
   is
      TT            : constant Task_Id := To_Task_Id (T);
      Error_Message : constant String  := "Trying to Reinitialize a ";

   begin
      if TT = null then
         Raise_Exception (Program_Error'Identity, Error_Message & "null task");
      end if;

      if TT.Common.State = Terminated then
         Raise_Exception (Tasking_Error'Identity,
           Error_Message & "terminated task");
      end if;

      if Local.Index /= 0 then
         Set_Value (Initial_Value, T);
      else
         declare
            P, Q    : Access_Node;
            W       : Access_Wrapper;
            Self_Id : constant Task_Id := POP.Self;

         begin
            Defer_Abort (Self_Id);
            POP.Lock_RTS;
            Q := To_Access_Node (TT.Indirect_Attributes);

            while Q /= null loop
               if Q.Instance = Access_Instance'(Local'Unchecked_Access) then
                  if P = null then
                     TT.Indirect_Attributes := To_Access_Address (Q.Next);
                  else
                     P.Next := Q.Next;
                  end if;

                  W := To_Access_Wrapper (Q.Wrapper);
                  Free (W);
                  POP.Unlock_RTS;
                  Undefer_Abort (Self_Id);
                  return;
               end if;

               P := Q;
               Q := Q.Next;
            end loop;

            POP.Unlock_RTS;
            Undefer_Abort (Self_Id);

         exception
            when others =>
               POP.Unlock_RTS;
               Undefer_Abort (Self_Id);
               raise;
         end;
      end if;

   exception
      when Tasking_Error | Program_Error =>
         raise;

      when others =>
         raise Program_Error;
   end Reinitialize;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Val : Attribute;
      T   : Task_Identification.Task_Id := Task_Identification.Current_Task)
   is
      TT            : constant Task_Id := To_Task_Id (T);
      Error_Message : constant String  := "Trying to Set the Value of a ";

   begin
      if TT = null then
         Raise_Exception (Program_Error'Identity, Error_Message & "null task");
      end if;

      if TT.Common.State = Terminated then
         Raise_Exception (Tasking_Error'Identity,
           Error_Message & "terminated task");
      end if;

      --  Directly addressed case

      if Local.Index /= 0 then

         --  Set attribute handle, warnings off, because this code can generate
         --  alignment warnings with large attributes (but of course will not
         --  be executed in this case, since we never have direct addressing in
         --  such cases).

         pragma Warnings (Off);
         To_Attribute_Handle
            (TT.Direct_Attributes (Local.Index)'Address).all := Val;
         pragma Warnings (On);
         return;
      end if;

      --  Not directly addressed

      declare
         P       : Access_Node := To_Access_Node (TT.Indirect_Attributes);
         W       : Access_Wrapper;
         Self_Id : constant Task_Id := POP.Self;

      begin
         Defer_Abort (Self_Id);
         POP.Lock_RTS;

         while P /= null loop

            if P.Instance = Access_Instance'(Local'Unchecked_Access) then
               To_Access_Wrapper (P.Wrapper).Value := Val;
               POP.Unlock_RTS;
               Undefer_Abort (Self_Id);
               return;
            end if;

            P := P.Next;
         end loop;

         --  Unlock RTS here to follow the lock ordering rule that prevent us
         --  from using new (i.e the Global_Lock) while holding any other lock.

         POP.Unlock_RTS;
         W := new Wrapper'((null, Local'Unchecked_Access, null), Val);
         POP.Lock_RTS;
         P := W.Dummy_Node'Unchecked_Access;
         P.Wrapper := To_Access_Dummy_Wrapper (W);
         P.Next := To_Access_Node (TT.Indirect_Attributes);
         TT.Indirect_Attributes := To_Access_Address (P);

         POP.Unlock_RTS;
         Undefer_Abort (Self_Id);

      exception
         when others =>
            POP.Unlock_RTS;
            Undefer_Abort (Self_Id);
            raise;
      end;

   exception
      when Tasking_Error | Program_Error =>
         raise;

      when others =>
         raise Program_Error;
   end Set_Value;

   -----------
   -- Value --
   -----------

   function Value
     (T : Task_Identification.Task_Id := Task_Identification.Current_Task)
      return Attribute
   is
      TT            : constant Task_Id := To_Task_Id (T);
      Error_Message : constant String  := "Trying to get the Value of a ";

   begin
      if TT = null then
         Raise_Exception (Program_Error'Identity, Error_Message & "null task");
      end if;

      if TT.Common.State = Terminated then
         Raise_Exception
           (Program_Error'Identity, Error_Message & "terminated task");
      end if;

      --  Directly addressed case

      if Local.Index /= 0 then

         --  Get value of attribute. We turn Warnings off, because for large
         --  attributes, this code can generate alignment warnings. But of
         --  course large attributes are never directly addressed so in fact
         --  we will never execute the code in this case.

         pragma Warnings (Off);
         return To_Attribute_Handle
           (TT.Direct_Attributes (Local.Index)'Address).all;
         pragma Warnings (On);
      end if;

      --  Not directly addressed

      declare
         P       : Access_Node;
         Result  : Attribute;
         Self_Id : constant Task_Id := POP.Self;

      begin
         Defer_Abort (Self_Id);
         POP.Lock_RTS;
         P := To_Access_Node (TT.Indirect_Attributes);

         while P /= null loop
            if P.Instance = Access_Instance'(Local'Unchecked_Access) then
               Result := To_Access_Wrapper (P.Wrapper).Value;
               POP.Unlock_RTS;
               Undefer_Abort (Self_Id);
               return Result;
            end if;

            P := P.Next;
         end loop;

         POP.Unlock_RTS;
         Undefer_Abort (Self_Id);
         return Initial_Value;

      exception
         when others =>
            POP.Unlock_RTS;
            Undefer_Abort (Self_Id);
            raise;
      end;

   exception
      when Tasking_Error | Program_Error =>
         raise;

      when others =>
         raise Program_Error;
   end Value;

--  Start of elaboration code for package Ada.Task_Attributes

begin
   --  This unchecked conversion can give warnings when alignments are
   --  incorrect, but they will not be used in such cases anyway, so the
   --  warnings can be safely ignored.

   pragma Warnings (Off);
   Local.Deallocate := To_Lib_Level_Deallocator (Deallocate'Access);
   pragma Warnings (On);

   declare
      Two_To_J : Direct_Index_Vector;
      Self_Id  : constant Task_Id := POP.Self;
   begin
      Defer_Abort (Self_Id);

      --  Need protection for updating links to per-task initialization and
      --  finalization routines, in case some task is being created or
      --  terminated concurrently.

      POP.Lock_RTS;

      --  Add this instantiation to the list of all instantiations

      Local.Next := System.Tasking.Task_Attributes.All_Attributes;
      System.Tasking.Task_Attributes.All_Attributes :=
        Local'Unchecked_Access;

      --  Try to find space for the attribute in the TCB

      Local.Index := 0;
      Two_To_J := 1;

      if Attribute'Size <= System.Address'Size then
         for J in Direct_Index_Range loop
            if (Two_To_J and In_Use) = 0 then

               --  Reserve location J for this attribute

               In_Use := In_Use or Two_To_J;
               Local.Index := J;

               --  This unchecked conversion can give a warning when the
               --  alignment is incorrect, but it will not be used in such
               --  a case anyway, so the warning can be safely ignored.

               pragma Warnings (Off);
               To_Attribute_Handle (Local.Initial_Value'Access).all :=
                 Initial_Value;
               pragma Warnings (On);

               exit;
            end if;

            Two_To_J := Two_To_J * 2;
         end loop;
      end if;

      --  Attribute goes directly in the TCB

      if Local.Index /= 0 then
         --  Replace stub for initialization routine that is called at task
         --  creation.

         Initialization.Initialize_Attributes_Link :=
           System.Tasking.Task_Attributes.Initialize_Attributes'Access;

         --  Initialize the attribute, for all tasks

         declare
            C : System.Tasking.Task_Id := System.Tasking.All_Tasks_List;
         begin
            while C /= null loop
               C.Direct_Attributes (Local.Index) :=
                 To_Direct_Attribute_Element
                   (System.Storage_Elements.To_Address (Local.Initial_Value));
               C := C.Common.All_Tasks_Link;
            end loop;
         end;

      --  Attribute goes into a node onto a linked list

      else
         --  Replace stub for finalization routine called at task termination

         Initialization.Finalize_Attributes_Link :=
           System.Tasking.Task_Attributes.Finalize_Attributes'Access;
      end if;

      POP.Unlock_RTS;
      Undefer_Abort (Self_Id);
   end;
end Ada.Task_Attributes;
