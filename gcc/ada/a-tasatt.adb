------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                  A D A . T A S K _ A T T R I B U T E S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--             Copyright (C) 1995-2004, Ada Core Technologies               --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  The following notes are provided in case someone decides the
--  implementation of this package is too complicated, or too slow.
--  Please read this before making any "simplifications".

--  Correct implementation of this package is more difficult than one
--  might expect. After considering (and coding) several alternatives,
--  we settled on the present compromise. Things we do not like about
--  this implementation include:

--  -  It is vulnerable to bad Task_Id values, to the extent of
--     possibly trashing memory and crashing the runtime system.

--  -  It requires dynamic storage allocation for each new attribute value,
--     except for types that happen to be the same size as System.Address,
--     or shorter.

--  -  Instantiations at other than the library level rely on being able to
--     do down-level calls to a procedure declared in the generic package body.
--     This makes it potentially vulnerable to compiler changes.

--  The main implementation issue here is that the connection from
--  task to attribute is a potential source of dangling references.

--  When a task goes away, we want to be able to recover all the storage
--  associated with its attributes. The Ada mechanism for this is
--  finalization, via controlled attribute types. For this reason,
--  the ARM requires finalization of attribute values when the
--  associated task terminates.

--  This finalization must be triggered by the tasking runtime system,
--  during termination of the task. Given the active set of instantiations
--  of Ada.Task_Attributes is dynamic, the number and types of attributes
--  belonging to a task will not be known until the task actually terminates.
--  Some of these types may be controlled and some may not. The RTS must find
--  some way to determine which of these attributes need finalization, and
--  invoke the appropriate finalization on them.

--  One way this might be done is to create a special finalization chain
--  for each task, similar to the finalization chain that is used for
--  controlled objects within the task. This would differ from the usual
--  finalization chain in that it would not have a LIFO structure, since
--  attributes may be added to a task at any time during its lifetime.
--  This might be the right way to go for the longer term, but at present
--  this approach is not open, since GNAT does not provide such special
--  finalization support.

--  Lacking special compiler support, the RTS is limited to the
--  normal ways an application invokes finalization, i.e.

--  a) Explicit call to the procedure Finalize, if we know the type
--     has this operation defined on it. This is not sufficient, since
--     we have no way of determining whether a given generic formal
--     Attribute type is controlled, and no visibility of the associated
--     Finalize procedure, in the generic body.

--  b) Leaving the scope of a local object of a controlled type.
--     This does not help, since the lifetime of an instantiation of
--     Ada.Task_Attributes does not correspond to the lifetimes of the
--     various tasks which may have that attribute.

--  c) Assignment of another value to the object. This would not help,
--     since we then have to finalize the new value of the object.

--  d) Unchecked deallocation of an object of a controlled type.
--     This seems to be the only mechanism available to the runtime
--     system for finalization of task attributes.

--  We considered two ways of using unchecked deallocation, both based
--  on a linked list of that would hang from the task control block.

--  In the first approach the objects on the attribute list are all derived
--  from one controlled type, say T, and are linked using an access type to
--  T'Class. The runtime system has an Unchecked_Deallocation for T'Class
--  with access type T'Class, and uses this to deallocate and finalize all
--  the items in the list. The limitation of this approach is that each
--  instantiation of the package Ada.Task_Attributes derives a new record
--  extension of T, and since T is controlled (RM 3.9.1 (3)), instantiation
--  is only allowed at the library level.

--  In the second approach the objects on the attribute list are of
--  unrelated but structurally similar types. Unchecked conversion is
--  used to circument Ada type checking. Each attribute-storage node
--  contains not only the attribute value and a link for chaining, but
--  also a pointer to a descriptor for the corresponding instantiation
--  of Task_Attributes. The instantiation-descriptor contains a
--  pointer to a procedure that can do the correct deallocation and
--  finalization for that type of attribute. On task termination, the
--  runtime system uses the pointer to call the appropriate deallocator.

--  While this gets around the limitation that instantations be at
--  the library level, it relies on an implementation feature that
--  may not always be safe, i.e. that it is safe to call the
--  Deallocate procedure for an instantiation of Ada.Task_Attributes
--  that no longer exists. In general, it seems this might result in
--  dangling references.

--  Another problem with instantiations deeper than the library level
--  is that there is risk of storage leakage, or dangling references
--  to reused storage. That is, if an instantiation of Ada.Task_Attributes
--  is made within a procedure, what happens to the storage allocated for
--  attributes, when the procedure call returns?  Apparently (RM 7.6.1 (4))
--  any such objects must be finalized, since they will no longer be
--  accessible, and in general one would expect that the storage they occupy
--  would be recovered for later reuse. (If not, we would have a case of
--  storage leakage.)  Assuming the storage is recovered and later reused,
--  we have potentially dangerous dangling references. When the procedure
--  containing the instantiation of Ada.Task_Attributes returns, there
--  may still be unterminated tasks with associated attribute values for
--  that instantiation. When such tasks eventually terminate, the RTS
--  will attempt to call the Deallocate procedure on them. If the
--  corresponding storage has already been deallocated, when the master
--  of the access type was left, we have a potential disaster. This
--  disaster is compounded since the pointer to Deallocate is probably
--  through a "trampoline" which will also have been destroyed.

--  For this reason, we arrange to remove all dangling references
--  before leaving the scope of an instantiation. This is ugly, since
--  it requires traversing the list of all tasks, but it is no more ugly
--  than a similar traversal that we must do at the point of instantiation
--  in order to initialize the attributes of all tasks. At least we only
--  need to do these traversals if the type is controlled.

--  We chose to defer allocation of storage for attributes until the
--  Reference function is called or the attribute is first set to a value
--  different from the default initial one. This allows a potential
--  savings in allocation, for attributes that are not used by all tasks.

--  For efficiency, we reserve space in the TCB for a fixed number of
--  direct-access attributes. These are required to be of a size that
--  fits in the space of an object of type System.Address. Because
--  we must use unchecked bitwise copy operations on these values, they
--  cannot be of a controlled type, but that is covered automatically
--  since controlled objects are too large to fit in the spaces.

--  We originally deferred the initialization of these direct-access
--  attributes, just as we do for the indirect-access attributes, and
--  used a per-task bit vector to keep track of which attributes were
--  currently defined for that task. We found that the overhead of
--  maintaining this bit-vector seriously slowed down access to the
--  attributes, and made the fetch operation non-atomic, so that even
--  to read an attribute value required locking the TCB. Therefore,
--  we now initialize such attributes for all existing tasks at the time
--  of the attribute instantiation, and initialize existing attributes
--  for each new task at the time it is created.

--  The latter initialization requires a list of all the instantiation
--  descriptors. Updates to this list, as well as the bit-vector that
--  is used to reserve slots for attributes in the TCB, require mutual
--  exclusion. That is provided by the Lock/Unlock_RTS.

--  One special problem that added complexity to the design is that
--  the per-task list of indirect attributes contains objects of
--  different types. We use unchecked pointer conversion to link
--  these nodes together and access them, but the records may not have
--  identical internal structure. Initially, we thought it would be
--  enough to allocate all the common components of the records at the
--  front of each record, so that their positions would correspond.
--  Unfortunately, GNAT adds "dope" information at the front of a record,
--  if the record contains any controlled-type components.
--
--  This means that the offset of the fields we use to link the nodes is
--  at different positions on nodes of different types. To get around this,
--  each attribute storage record consists of a core node and wrapper.
--  The core nodes are all of the same type, and it is these that are
--  linked together and generally "seen" by the RTS. Each core node
--  contains a pointer to its own wrapper, which is a record that contains
--  the core node along with an attribute value, approximately
--  as follows:

--    type Node;
--    type Node_Access is access all Node;
--    type Node_Access;
--    type Access_Wrapper is access all Wrapper;
--    type Node is record
--       Next    : Node_Access;
--       ...
--       Wrapper : Access_Wrapper;
--    end record;
--    type Wrapper is record
--       Noed    : aliased Node;
--       Value   : aliased Attribute;  --  the generic formal type
--    end record;

--  Another interesting problem is with the initialization of
--  the instantiation descriptors. Originally, we did this all via
--  the Initialize procedure of the descriptor type and code in the
--  package body. It turned out that the Initialize procedure needed
--  quite a bit of information, including the size of the attribute
--  type, the initial value of the attribute (if it fits in the TCB),
--  and a pointer to the deallocator procedure. These needed to be
--  "passed" in via access discriminants. GNAT was having trouble
--  with access discriminants, so all this work was moved to the
--  package body.

with Ada.Task_Identification;
--  used for Task_Id
--           Null_Task_Id
--           Current_Task

with System.Error_Reporting;
--  used for Shutdown;

with System.Storage_Elements;
--  used for Integer_Address

with System.Task_Primitives.Operations;
--  used for Write_Lock
--           Unlock
--           Lock/Unlock_RTS

with System.Tasking;
--  used for Access_Address
--           Task_Id
--           Direct_Index_Vector
--           Direct_Index

with System.Tasking.Initialization;
--  used for Defer_Abortion
--           Undefer_Abortion
--           Initialize_Attributes_Link
--           Finalize_Attributes_Link

with System.Tasking.Task_Attributes;
--  used for Access_Node
--           Access_Dummy_Wrapper
--           Deallocator
--           Instance
--           Node
--           Access_Instance

with Ada.Exceptions;
--  used for Raise_Exception

with Unchecked_Conversion;
with Unchecked_Deallocation;

pragma Elaborate_All (System.Tasking.Task_Attributes);
--  to ensure the initialization of object Local (below) will work

package body Ada.Task_Attributes is

   use System.Error_Reporting,
       System.Tasking.Initialization,
       System.Tasking,
       System.Tasking.Task_Attributes,
       Ada.Exceptions;

   use type System.Tasking.Access_Address;

   package POP renames System.Task_Primitives.Operations;

   ---------------------------
   -- Unchecked Conversions --
   ---------------------------

   --  The following type corresponds to Dummy_Wrapper,
   --  declared in System.Tasking.Task_Attributes.

   type Wrapper;
   type Access_Wrapper is access all Wrapper;

   pragma Warnings (Off);
   --  We turn warnings off for the following declarations of the
   --  To_Attribute_Handle conversions, since these are used only
   --  for small attributes where we know that there are no problems
   --  with alignment, but the compiler will generate warnings for
   --  the occurrences in the large attribute case, even though
   --  they will not actually be used.

   function To_Attribute_Handle is new Unchecked_Conversion
     (System.Address, Attribute_Handle);
   function To_Direct_Attribute_Element is new Unchecked_Conversion
     (System.Address, Direct_Attribute_Element);
   --  For reference to directly addressed task attributes

   type Access_Integer_Address is access all
     System.Storage_Elements.Integer_Address;

   function To_Attribute_Handle is new Unchecked_Conversion
     (Access_Integer_Address, Attribute_Handle);
   --  For reference to directly addressed task attributes

   pragma Warnings (On);
   --  End of warnings off region for directly addressed
   --  attribute conversion functions.

   function To_Access_Address is new Unchecked_Conversion
     (Access_Node, Access_Address);
   --  To store pointer to list of indirect attributes

   pragma Warnings (Off);
   function To_Access_Wrapper is new Unchecked_Conversion
     (Access_Dummy_Wrapper, Access_Wrapper);
   pragma Warnings (On);
   --  To fetch pointer to actual wrapper of attribute node. We turn off
   --  warnings since this may generate an alignment warning. The warning
   --  can be ignored since Dummy_Wrapper is only a non-generic standin
   --  for the real wrapper type (we never actually allocate objects of
   --  type Dummy_Wrapper).

   function To_Access_Dummy_Wrapper is new Unchecked_Conversion
     (Access_Wrapper, Access_Dummy_Wrapper);
   --  To store pointer to actual wrapper of attribute node

   function To_Task_Id is new Unchecked_Conversion
     (Task_Identification.Task_Id, Task_Id);
   --  To access TCB of identified task

   type Local_Deallocator is access procedure (P : in out Access_Node);

   function To_Lib_Level_Deallocator is new Unchecked_Conversion
     (Local_Deallocator, Deallocator);
   --  To defeat accessibility check

   pragma Warnings (On);

   ------------------------
   -- Storage Management --
   ------------------------

   procedure Deallocate (P : in out Access_Node);
   --  Passed to the RTS via unchecked conversion of a pointer to
   --  permit finalization and deallocation of attribute storage nodes

   --------------------------
   -- Instantiation Record --
   --------------------------

   Local : aliased Instance;
   --  Initialized in package body

   type Wrapper is record
      Noed : aliased Node;

      Value : aliased Attribute := Initial_Value;
      --  The generic formal type, may be controlled
   end record;

   --  A number of unchecked conversions involving Wrapper_Access sources
   --  are performed in this unit. We have to ensure that the designated
   --  object is always strictly enough aligned.

   for Wrapper'Alignment use Standard'Maximum_Alignment;

   procedure Free is
      new Unchecked_Deallocation (Wrapper, Access_Wrapper);

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
            P : Access_Node := To_Access_Node (TT.Indirect_Attributes);
            W : Access_Wrapper;

         begin
            Defer_Abortion;
            POP.Lock_RTS;

            while P /= null loop
               if P.Instance = Access_Instance'(Local'Unchecked_Access) then
                  POP.Unlock_RTS;
                  Undefer_Abortion;
                  return To_Access_Wrapper (P.Wrapper).Value'Access;
               end if;

               P := P.Next;
            end loop;

            --  Unlock the RTS here to follow the lock ordering rule
            --  that prevent us from using new (i.e the Global_Lock) while
            --  holding any other lock.

            POP.Unlock_RTS;
            W := new Wrapper'
                  ((null, Local'Unchecked_Access, null), Initial_Value);
            POP.Lock_RTS;

            P := W.Noed'Unchecked_Access;
            P.Wrapper := To_Access_Dummy_Wrapper (W);
            P.Next := To_Access_Node (TT.Indirect_Attributes);
            TT.Indirect_Attributes := To_Access_Address (P);
            POP.Unlock_RTS;
            Undefer_Abortion;
            return W.Value'Access;

         exception
            when others =>
               POP.Unlock_RTS;
               Undefer_Abortion;
               raise;
         end;
      end if;

      pragma Assert (Shutdown ("Should never get here in Reference"));
      return null;

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
            P, Q : Access_Node;
            W    : Access_Wrapper;
         begin
            Defer_Abortion;
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
                  Undefer_Abortion;
                  return;
               end if;

               P := Q;
               Q := Q.Next;
            end loop;

            POP.Unlock_RTS;
            Undefer_Abortion;

         exception
            when others =>
               POP.Unlock_RTS;
               Undefer_Abortion;
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
         P : Access_Node := To_Access_Node (TT.Indirect_Attributes);
         W : Access_Wrapper;

      begin
         Defer_Abortion;
         POP.Lock_RTS;

         while P /= null loop

            if P.Instance = Access_Instance'(Local'Unchecked_Access) then
               To_Access_Wrapper (P.Wrapper).Value := Val;
               POP.Unlock_RTS;
               Undefer_Abortion;
               return;
            end if;

            P := P.Next;
         end loop;

         --  Unlock RTS here to follow the lock ordering rule that
         --  prevent us from using new (i.e the Global_Lock) while
         --  holding any other lock.

         POP.Unlock_RTS;
         W := new Wrapper'((null, Local'Unchecked_Access, null), Val);
         POP.Lock_RTS;
         P := W.Noed'Unchecked_Access;
         P.Wrapper := To_Access_Dummy_Wrapper (W);
         P.Next := To_Access_Node (TT.Indirect_Attributes);
         TT.Indirect_Attributes := To_Access_Address (P);

         POP.Unlock_RTS;
         Undefer_Abortion;

      exception
         when others =>
            POP.Unlock_RTS;
            Undefer_Abortion;
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

         --  Get value of attribute. Warnings off, because for large
         --  attributes, this code can generate alignment warnings.
         --  But of course large attributes are never directly addressed
         --  so in fact we will never execute the code in this case.

         pragma Warnings (Off);
         return To_Attribute_Handle
           (TT.Direct_Attributes (Local.Index)'Address).all;
         pragma Warnings (On);
      end if;

      --  Not directly addressed

      declare
         P      : Access_Node;
         Result : Attribute;

      begin
         Defer_Abortion;
         POP.Lock_RTS;
         P := To_Access_Node (TT.Indirect_Attributes);

         while P /= null loop
            if P.Instance = Access_Instance'(Local'Unchecked_Access) then
               Result := To_Access_Wrapper (P.Wrapper).Value;
               POP.Unlock_RTS;
               Undefer_Abortion;
               return Result;
            end if;

            P := P.Next;
         end loop;

         POP.Unlock_RTS;
         Undefer_Abortion;
         return Initial_Value;

      exception
         when others =>
            POP.Unlock_RTS;
            Undefer_Abortion;
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
   --  This unchecked conversion can give warnings when alignments
   --  are incorrect, but they will not be used in such cases anyway,
   --  so the warnings can be safely ignored.

   pragma Warnings (Off);
   Local.Deallocate := To_Lib_Level_Deallocator (Deallocate'Access);
   pragma Warnings (On);

   declare
      Two_To_J : Direct_Index_Vector;
   begin
      Defer_Abortion;

      --  Need protection for updating links to per-task initialization and
      --  finalization routines, in case some task is being created or
      --  terminated concurrently.

      POP.Lock_RTS;

      --  Add this instantiation to the list of all instantiations.

      Local.Next := System.Tasking.Task_Attributes.All_Attributes;
      System.Tasking.Task_Attributes.All_Attributes :=
        Local'Unchecked_Access;

      --  Try to find space for the attribute in the TCB.

      Local.Index := 0;
      Two_To_J := 1;

      if Attribute'Size <= System.Address'Size then
         for J in Direct_Index_Range loop
            if (Two_To_J and In_Use) = 0 then

               --  Reserve location J for this attribute

               In_Use := In_Use or Two_To_J;
               Local.Index := J;

               --  This unchecked conversions can give a warning when the
               --  the alignment is incorrect, but it will not be used in
               --  such a case anyway, so the warning can be safely ignored.

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
         --  Replace stub for initialization routine
         --  that is called at task creation.

         Initialization.Initialize_Attributes_Link :=
           System.Tasking.Task_Attributes.Initialize_Attributes'Access;

         --  Initialize the attribute, for all tasks.

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
         --  Replace stub for finalization routine
         --  that is called at task termination.

         Initialization.Finalize_Attributes_Link :=
           System.Tasking.Task_Attributes.Finalize_Attributes'Access;
      end if;

      POP.Unlock_RTS;
      Undefer_Abortion;
   end;
end Ada.Task_Attributes;
