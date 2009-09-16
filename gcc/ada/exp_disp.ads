------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ D I S P                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
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

   --      _Disp_Requeue (15) - used in the expansion of dispatching requeue
   --      statements. Null implementation is provided for protected, task
   --      and synchronized interfaces. Protected and task types implementing
   --      concurrent interfaces receive full bodies. See Expand_N_Requeue_
   --      Statement in Exp_Ch9 for more information.

   --      _Disp_Timed_Select (16) - used in the expansion of timed selects
   --      with dispatching triggers. Null implementation for limited
   --      interfaces, full body generation for types that implement limited
   --      interfaces, not generated for the rest of the cases. See Expand_N_
   --      Timed_Entry_Call for more information.

   --  Life cycle of predefined primitive operations

   --      The specifications and bodies of the PPOs are created by
   --      Make_Predefined_Primitive_Specs and Predefined_Primitive_Bodies
   --      in Exp_Ch3. The generated specifications are immediately analyzed,
   --      while the bodies are left as freeze actions to the tagged type for
   --      which they are created.

   --      PPOs are collected and added to the Primitive_Operations list of
   --      a type by the regular analysis mechanism.

   --      PPOs are frozen by Exp_Ch3.Predefined_Primitive_Freeze

   --      Thunks for PPOs are created by Make_DT

   --      Dispatch table positions of PPOs are set by Set_All_DT_Position

   --      Calls to PPOs proceed as regular dispatching calls. If the PPO
   --      has a thunk, a call proceeds as a regular dispatching call with
   --      a thunk.

   --  Guidelines for addition of new predefined primitive operations

   --      Update the value of constant Max_Predef_Prims in a-tags.ads to
   --      indicate the new number of PPOs.

   --      Introduce a new predefined name for the new PPO in Snames.ads and
   --      Snames.adb.

   --      Categorize the new PPO name as predefined by adding an entry in
   --      Is_Predefined_Dispatching_Operation in Exp_Disp.

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

   --    Ada.Tags.Max_Predef_Prims         - indirect use
   --    Exp_Disp.Default_Prim_Op_Position - indirect use
   --    Exp_Disp.Set_All_DT_Position      - direct   use

   procedure Apply_Tag_Checks (Call_Node : Node_Id);
   --  Generate checks required on dispatching calls

   function Building_Static_DT (Typ : Entity_Id) return Boolean;
   pragma Inline (Building_Static_DT);
   --  Returns true when building statically allocated dispatch tables

   procedure Build_Static_Dispatch_Tables (N : Node_Id);
   --  N is a library level package declaration or package body. Build the
   --  static dispatch table of the tagged types defined at library level. In
   --  case of package declarations with private part the generated nodes are
   --  added at the end of the list of private declarations. Otherwise they are
   --  added to the end of the list of public declarations. In case of package
   --  bodies they are added to the end of the list of declarations of the
   --  package body.

   procedure Expand_Dispatching_Call (Call_Node : Node_Id);
   --  Expand the call to the operation through the dispatch table and perform
   --  the required tag checks when appropriate. For CPP types tag checks are
   --  not relevant.

   procedure Expand_Interface_Actuals (Call_Node : Node_Id);
   --  Ada 2005 (AI-251): Displace all the actuals corresponding to class-wide
   --  interfaces to reference the interface tag of the actual object

   procedure Expand_Interface_Conversion
     (N         : Node_Id;
      Is_Static : Boolean := True);
   --  Ada 2005 (AI-251): N is a type-conversion node. Reference the base of
   --  the object to give access to the interface tag associated with the
   --  secondary dispatch table.

   procedure Expand_Interface_Thunk
     (Prim       : Node_Id;
      Thunk_Id   : out Entity_Id;
      Thunk_Code : out Node_Id);
   --  Ada 2005 (AI-251): When a tagged type implements abstract interfaces we
   --  generate additional subprograms (thunks) associated with each primitive
   --  Prim to have a layout compatible with the C++ ABI. The thunk displaces
   --  the pointers to the actuals that depend on the controlling type before
   --  transferring control to the target subprogram. If there is no need to
   --  generate the thunk then Thunk_Id and Thunk_Code are set to Empty.
   --  Otherwise they are set to the defining identifier and the subprogram
   --  body of the generated thunk.

   function Is_Predefined_Dispatching_Operation (E : Entity_Id) return Boolean;
   --  Ada 2005 (AI-251): Determines if E is a predefined primitive operation

   function Is_Predefined_Internal_Operation (E : Entity_Id) return Boolean;
   --  Similar to the previous one, but excludes stream operations, because
   --  these may be overridden, and need extra formals, like user-defined
   --  operations.

   function Is_Predefined_Interface_Primitive (E : Entity_Id) return Boolean;
   --  Ada 2005 (AI-345): Returns True if E is one of the predefined primitives
   --  required to implement interfaces.

   function Make_DT (Typ : Entity_Id; N : Node_Id := Empty) return List_Id;
   --  Expand the declarations for the Dispatch Table. The node N is the
   --  declaration that forces the generation of the table. It is used to place
   --  error messages when the declaration leads to the freezing of a given
   --  primitive operation that has an incomplete non- tagged formal.

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
   --  Ada 2005 (AI-345): Generate body of the primitive operation of type Typ
   --  used for retrieving the _task_id field of a task interface class- wide
   --  type. Generate a null body if Typ is an interface or a non-task type.

   function Make_Disp_Get_Task_Id_Spec
     (Typ : Entity_Id) return Node_Id;
   --  Ada 2005 (AI-345): Generate the specification of the primitive operation
   --  of type Typ used for retrieving the _task_id field of a task interface
   --  class-wide type.

   function Make_Disp_Requeue_Body
     (Typ : Entity_Id) return Node_Id;
   --  Ada 2005 (AI05-0030): Generate the body of the primitive operation of
   --  type Typ used for dispatching on requeue statements. Generate a body
   --  containing a single null-statement if Typ is an interface type.

   function Make_Disp_Requeue_Spec
     (Typ : Entity_Id) return Node_Id;
   --  Ada 2005 (AI05-0030): Generate the specification of the primitive
   --  operation of type Typ used for dispatching requeue statements.

   function Make_Disp_Timed_Select_Body
     (Typ : Entity_Id) return Node_Id;
   --  Ada 2005 (AI-345): Generate the body of the primitive operation of type
   --  Typ used for dispatching in timed selects. Generate a body containing
   --  a single null-statement if Typ is an interface type.

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

   function Make_Tags (Typ : Entity_Id) return List_Id;
   --  Generate the entities associated with the primary and secondary tags of
   --  Typ and fill the contents of Access_Disp_Table. In case of library level
   --  tagged types this routine imports the forward declaration of the tag
   --  entity, that will be declared and exported by Make_DT.

   function Register_Primitive
     (Loc     : Source_Ptr;
      Prim    : Entity_Id) return List_Id;
   --  Build code to register Prim in the primary or secondary dispatch table.
   --  If Prim is associated with a secondary dispatch table then generate also
   --  its thunk and register it in the associated secondary dispatch table.
   --  In general the dispatch tables are always generated by Make_DT and
   --  Make_Secondary_DT; this routine is only used in two corner cases:
   --
   --    1) To construct the dispatch table of a tagged type whose parent
   --       is a CPP_Class (see Build_Init_Procedure).
   --    2) To handle late overriding of dispatching operations (see
   --       Check_Dispatching_Operation and Make_DT).
   --
   --  The caller is responsible for inserting the generated code in the
   --  proper place.

   procedure Set_All_DT_Position (Typ : Entity_Id);
   --  Set the DT_Position field for each primitive operation. In the CPP
   --  Class case check that no pragma CPP_Virtual is missing and that the
   --  DT_Position are coherent

   procedure Set_CPP_Constructors (Typ : Entity_Id);
   --  Typ is a CPP_Class type. Create the Init procedures of that type
   --  required to handle its default and non-default constructors. The
   --  functions to which pragma CPP_Constructor is applied in the sources
   --  are functions returning this type, and having an implicit access to the
   --  target object in its first argument; such implicit argument is explicit
   --  in the IP procedures built here.

   procedure Set_DTC_Entity_Value
     (Tagged_Type : Entity_Id;
      Prim        : Entity_Id);
   --  Set the definite value of the DTC_Entity value associated with a given
   --  primitive of a tagged type.

   procedure Write_DT (Typ : Entity_Id);
   pragma Export (Ada, Write_DT);
   --  Debugging procedure (to be called within gdb)

end Exp_Disp;
