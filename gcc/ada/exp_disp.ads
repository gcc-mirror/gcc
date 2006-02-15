------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ D I S P                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains routines involved in tagged types and dynamic
--  dispatching expansion.

with Types; use Types;

package Exp_Disp is

   -------------------------------------
   -- Predefined primitive operations --
   -------------------------------------

   --  The predefined primitive operations (PPOs) are subprograms generated
   --  by GNAT for a particular tagged type. Their role is to provide support
   --  for different Ada language features such as the attribute 'Size or
   --  handling of dispatching triggers in select statements. PPOs are created
   --  when a tagged type is expanded or frozen. These subprograms are later
   --  collected and inserted into the dispatch table of a tagged type at
   --  fixed positions. Some of the PPOs that manipulate data in tagged objects
   --  require the generation of thunks.

   --  List of predefined primitive operations

   --    Leading underscores designate reserved names. Bracketed numerical
   --    values represent dispatch table slot numbers.

   --      _Size (1) - implementation of the attribute 'Size for any tagged
   --      type. Constructs of the form Prefix'Size are converted into
   --      Prefix._Size.

   --      _Alignment (2) - implementation of the attribute 'Alignment for
   --      any tagged type. Constructs of the form Prefix'Alignment are
   --      converted into Prefix._Alignment.

   --      TSS_Stream_Read (3) - implementation of the stream attribute Read
   --      for any tagged type.

   --      TSS_Stream_Write (4) - implementation of the stream attribute Write
   --      for any tagged type.

   --      TSS_Stream_Input (5) - implementation of the stream attribute Input
   --      for any tagged type.

   --      TSS_Stream_Output (6) - implementation of the stream attribute
   --      Output for any tagged type.

   --      Op_Eq (7) - implementation of the equality operator for any non-
   --      limited tagged type.

   --      _Assign (8) - implementation of the assignment operator for any
   --      non-limited tagged type.

   --      TSS_Deep_Adjust (9) - implementation of the finalization operation
   --      Adjust for any non-limited tagged type.

   --      TSS_Deep_Finalize (10) - implementation of the finalization
   --      operation Finalize for any non-limited tagged type.

   --      _Disp_Asynchronous_Select (11) - used in the expansion of ATC with
   --      dispatching triggers. Null implementation for limited interfaces,
   --      full body generation for types that implement limited interfaces,
   --      not generated for the rest of the cases. See Expand_N_Asynchronous_
   --      Select in Exp_Ch9 for more information.

   --      _Disp_Conditional_Select (12) - used in the expansion of conditional
   --      selects with dispatching triggers. Null implementation for limited
   --      interfaces, full body generation for types that implement limited
   --      interfaces, not generated for the rest of the cases. See Expand_N_
   --      Conditional_Entry_Call in Exp_Ch9 for more information.

   --      _Disp_Get_Prim_Op_Kind (13) - helper routine used in the expansion
   --      of ATC with dispatching triggers. Null implementation for limited
   --      interfaces, full body generation for types that implement limited
   --      interfaces, not generated for the rest of the cases.

   --      _Disp_Get_Task_Id (14) - helper routine used in the expansion of
   --      Abort, attributes 'Callable and 'Terminated for task interface
   --      class-wide types. Full body generation for task types, null
   --      implementation for limited interfaces, not generated for the rest
   --      of the cases. See Expand_N_Attribute_Reference in Exp_Attr and
   --      Expand_N_Abort_Statement in Exp_Ch9 for more information.

   --      _Disp_Timed_Select (15) - used in the expansion of timed selects
   --      with dispatching triggers. Null implementation for limited
   --      interfaces, full body generation for types that implement limited
   --      interfaces, not generated for the rest of the cases. See Expand_N_
   --      Timed_Entry_Call for more information.

   --  Lifecycle of predefined primitive operations

   --      The specifications and bodies of the PPOs are created by
   --      Make_Predefined_Primitive_Specs and Predefined_Primitive_Bodies
   --      in Exp_Ch3. The generated specifications are immediately analyzed,
   --      while the bodies are left as freeze actions to the tagged type for
   --      which they are created.

   --      PPOs are collected and added to the Primitive_Operations list of
   --      a type by the regular analysis mechanism.

   --      PPOs are frozen in Predefined_Primitive_Freeze in Exp_Ch3.

   --      Thunks for PPOs are created in Freeze_Subprogram in Exp_Ch6, by a
   --      call to Register_Predefined_DT_Entry, also in Exp_Ch6.

   --      Dispatch table positions of PPOs are set in Set_All_DT_Position in
   --      Exp_Disp.

   --      Calls to PPOs procede as regular dispatching calls. If the PPO
   --      has a thunk, a call procedes as a regular dispatching call with
   --      a thunk.

   --  Guidelines for addition of new predefined primitive operations

   --      Update the value of constant Default_Prim_Op_Count in A-Tags.ads
   --      to reflect the new number of PPOs.

   --      Introduce a new predefined name for the new PPO in Snames.ads and
   --      Snames.adb.

   --      Categorize the new PPO name as predefined by adding an entry in
   --      Is_Predefined_Dispatching_Operation in Exp_Util.adb.

   --      Generate the specification of the new PPO in Make_Predefined_
   --      Primitive_Spec in Exp_Ch3.adb. The Is_Internal flag of the defining
   --      identifier of the specification must be set to True.

   --      Generate the body of the new PPO in Predefined_Primitive_Bodies in
   --      Exp_Ch3.adb. The Is_Internal flag of the defining identifier of the
   --      specification must be set to True.

   --      If the new PPO requires a thunk, add an entry in Freeze_Subprogram
   --      in Exp_Ch6.adb.

   --      When generating calls to a PPO, use Find_Prim_Op from Exp_Util.ads
   --      to retrieve the entity of the operation directly.

   --  Number of predefined primitive operations added by the Expander
   --  for a tagged type. If more predefined primitive operations are
   --  added, the following items must be changed:

   --    Ada.Tags.Defailt_Prim_Op_Count    - indirect use
   --    Exp_Disp.Default_Prim_Op_Position - indirect use
   --    Exp_Disp.Set_All_DT_Position      - direct   use

   type DT_Access_Action is
      (CW_Membership,
       IW_Membership,
       DT_Entry_Size,
       DT_Prologue_Size,
       Get_Access_Level,
       Get_Entry_Index,
       Get_External_Tag,
       Get_Predefined_Prim_Op_Address,
       Get_Prim_Op_Address,
       Get_Prim_Op_Kind,
       Get_RC_Offset,
       Get_Remotely_Callable,
       Get_Tagged_Kind,
       Inherit_DT,
       Inherit_TSD,
       Register_Interface_Tag,
       Register_Tag,
       Set_Access_Level,
       Set_Entry_Index,
       Set_Expanded_Name,
       Set_External_Tag,
       Set_Interface_Table,
       Set_Offset_Index,
       Set_OSD,
       Set_Predefined_Prim_Op_Address,
       Set_Prim_Op_Address,
       Set_Prim_Op_Kind,
       Set_RC_Offset,
       Set_Remotely_Callable,
       Set_Signature,
       Set_SSD,
       Set_TSD,
       Set_Tagged_Kind,
       TSD_Entry_Size,
       TSD_Prologue_Size);

   procedure Expand_Dispatching_Call (Call_Node : Node_Id);
   --  Expand the call to the operation through the dispatch table and perform
   --  the required tag checks when appropriate. For CPP types the call is
   --  done through the Vtable (tag checks are not relevant)

   procedure Expand_Interface_Actuals    (Call_Node : Node_Id);
   --  Ada 2005 (AI-251): Displace all the actuals corresponding to class-wide
   --  interfaces to reference the interface tag of the actual object

   procedure Expand_Interface_Conversion
     (N         : Node_Id;
      Is_Static : Boolean := True);
   --  Ada 2005 (AI-251): N is a type-conversion node. Reference the base of
   --  the object to give access to the interface tag associated with the
   --  secondary dispatch table.

   function Expand_Interface_Thunk
     (N           : Node_Id;
      Thunk_Alias : Node_Id;
      Thunk_Id    : Entity_Id) return Node_Id;
   --  Ada 2005 (AI-251): When a tagged type implements abstract interfaces we
   --  generate additional subprograms (thunks) to have a layout compatible
   --  with the C++ ABI. The thunk modifies the value of the first actual of
   --  the call (that is, the pointer to the object) before transferring
   --  control to the target function.

   function Fill_DT_Entry
     (Loc          : Source_Ptr;
      Prim         : Entity_Id) return Node_Id;
   --  Generate the code necessary to fill the appropriate entry of the
   --  dispatch table of Prim's controlling type with Prim's address.

   function Fill_Secondary_DT_Entry
     (Loc          : Source_Ptr;
      Prim         : Entity_Id;
      Thunk_Id     : Entity_Id;
      Iface_DT_Ptr : Entity_Id) return Node_Id;
   --  (Ada 2005): Generate the code necessary to fill the appropriate entry of
   --  the secondary dispatch table of Prim's controlling type with Thunk_Id's
   --  address.

   function Get_Remotely_Callable (Obj : Node_Id) return Node_Id;
   --  Return an expression that holds True if the object can be transmitted
   --  onto another partition according to E.4 (18)

   function Init_Predefined_Interface_Primitives
     (Typ : Entity_Id) return List_Id;
   --  Ada 2005 (AI-251): Initialize the entries associated with predefined
   --  primitives in all the secondary dispatch tables of Typ.

   function Make_DT_Access_Action
     (Typ    : Entity_Id;
      Action : DT_Access_Action;
      Args   : List_Id) return Node_Id;
   --  Generate a call to one of the Dispatch Table Access Subprograms defined
   --  in Ada.Tags or in Interfaces.Cpp

   function Make_DT (Typ : Entity_Id) return List_Id;
   --  Expand the declarations for the Dispatch Table (or the Vtable in
   --  the case of type whose ancestor is a CPP_Class)

   function Make_Disp_Asynchronous_Select_Body
     (Typ : Entity_Id) return Node_Id;
   --  Ada 2005 (AI-345): Generate the body of the primitive operation of type
   --  Typ used for dispatching in asynchronous selects. Generate a null body
   --  if Typ is an interface type.

   function Make_Disp_Asynchronous_Select_Spec
     (Typ : Entity_Id) return Node_Id;
   --  Ada 2005 (AI-345): Generate the specification of the primitive operation
   --  of type Typ used for dispatching in asynchronous selects.

   function Make_Disp_Conditional_Select_Body
     (Typ : Entity_Id) return Node_Id;
   --  Ada 2005 (AI-345): Generate the body of the primitive operation of type
   --  Typ used for dispatching in conditional selects. Generate a null body
   --  if Typ is an interface type.

   function Make_Disp_Conditional_Select_Spec
     (Typ : Entity_Id) return Node_Id;
   --  Ada 2005 (AI-345): Generate the specification of the primitive operation
   --  of type Typ used for dispatching in conditional selects.

   function Make_Disp_Get_Prim_Op_Kind_Body
     (Typ : Entity_Id) return Node_Id;
   --  Ada 2005 (AI-345): Generate the body of the primitive operation of type
   --  Typ used for retrieving the callable entity kind during dispatching in
   --  asynchronous selects. Generate a null body if Typ is an interface type.

   function Make_Disp_Get_Prim_Op_Kind_Spec
     (Typ : Entity_Id) return Node_Id;
   --  Ada 2005 (AI-345): Generate the specification of the primitive operation
   --  of the type Typ use for retrieving the callable entity kind during
   --  dispatching in asynchronous selects.

   function Make_Disp_Get_Task_Id_Body
     (Typ : Entity_Id) return Node_Id;
   --  Ada 2005 (AI-345): Generate the body of the primitive operation of type
   --  Typ used for retrieving the _task_id field of a task interface class-
   --  wide type. Generate a null body if Typ is an interface or a non-task
   --  type.

   function Make_Disp_Get_Task_Id_Spec
     (Typ : Entity_Id) return Node_Id;
   --  Ada 2005 (AI-345): Generate the specification of the primitive operation
   --  of type Typ used for retrieving the _task_id field of a task interface
   --  class-wide type.

   function Make_Disp_Timed_Select_Body
     (Typ : Entity_Id) return Node_Id;
   --  Ada 2005 (AI-345): Generate the body of the primitive operation of type
   --  Typ used for dispatching in timed selects. Generate a null body if Nul
   --  is an interface type.

   function Make_Disp_Timed_Select_Spec
     (Typ : Entity_Id) return Node_Id;
   --  Ada 2005 (AI-345): Generate the specification of the primitive operation
   --  of type Typ used for dispatching in timed selects.

   function Make_Select_Specific_Data_Table
     (Typ : Entity_Id) return List_Id;
   --  Ada 2005 (AI-345): Create and populate the auxiliary table in the TSD
   --  of Typ used for dispatching in asynchronous, conditional and timed
   --  selects. Generate code to set the primitive operation kinds and entry
   --  indices of primitive operations and primitive wrappers.

   procedure Make_Secondary_DT
     (Typ             : Entity_Id;
      Ancestor_Typ    : Entity_Id;
      Suffix_Index    : Int;
      Iface           : Entity_Id;
      AI_Tag          : Entity_Id;
      Acc_Disp_Tables : in out Elist_Id;
      Result          : out List_Id);
   --  Ada 2005 (AI-251): Expand the declarations for the Secondary Dispatch
   --  Table of Typ associated with Iface (each abstract interface implemented
   --  by Typ has a secondary dispatch table). The arguments Typ, Ancestor_Typ
   --  and Suffix_Index are used to generate an unique external name which
   --  is added at the end of Acc_Disp_Tables; this external name will be
   --  used later by the subprogram Exp_Ch3.Build_Init_Procedure.

   procedure Set_All_DT_Position (Typ : Entity_Id);
   --  Set the DT_Position field for each primitive operation. In the CPP
   --  Class case check that no pragma CPP_Virtual is missing and that the
   --  DT_Position are coherent

   procedure Set_Default_Constructor (Typ : Entity_Id);
   --  Typ is a CPP_Class type. Create the Init procedure of that type to
   --  be the default constructor (i.e. the function returning this type,
   --  having a pragma CPP_Constructor and no parameter)

   procedure Write_DT (Typ : Entity_Id);
   pragma Export (Ada, Write_DT);
   --  Debugging procedure (to be called within gdb)

end Exp_Disp;
