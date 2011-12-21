------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              A S P E C T S                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2010-2011, Free Software Foundation, Inc.          --
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

--  This package defines the aspects that are recognized by GNAT in aspect
--  specifications. It also contains the subprograms for storing/retrieving
--  aspect specifications from the tree. The semantic processing for aspect
--  specifications is found in Sem_Ch13.Analyze_Aspect_Specifications.

with Namet;  use Namet;
with Snames; use Snames;
with Types;  use Types;

package Aspects is

   --  Type defining recognized aspects

   type Aspect_Id is
     (No_Aspect,                            -- Dummy entry for no aspect
      Aspect_Address,
      Aspect_Alignment,
      Aspect_Attach_Handler,
      Aspect_Bit_Order,
      Aspect_Component_Size,
      Aspect_Constant_Indexing,
      Aspect_CPU,
      Aspect_Default_Component_Value,
      Aspect_Default_Iterator,
      Aspect_Default_Value,
      Aspect_Dimension,                     -- GNAT
      Aspect_Dimension_System,              -- GNAT
      Aspect_Dispatching_Domain,
      Aspect_Dynamic_Predicate,
      Aspect_External_Tag,
      Aspect_Implicit_Dereference,
      Aspect_Input,
      Aspect_Interrupt_Priority,
      Aspect_Invariant,
      Aspect_Iterator_Element,
      Aspect_Machine_Radix,
      Aspect_Object_Size,                   -- GNAT
      Aspect_Output,
      Aspect_Post,
      Aspect_Postcondition,
      Aspect_Pre,
      Aspect_Precondition,
      Aspect_Predicate,                     -- GNAT
      Aspect_Priority,
      Aspect_Read,
      Aspect_Size,
      Aspect_Small,
      Aspect_Static_Predicate,
      Aspect_Storage_Pool,
      Aspect_Storage_Size,
      Aspect_Stream_Size,
      Aspect_Suppress,
      Aspect_Test_Case,                     -- GNAT
      Aspect_Type_Invariant,
      Aspect_Unsuppress,
      Aspect_Value_Size,                    -- GNAT
      Aspect_Variable_Indexing,
      Aspect_Warnings,
      Aspect_Write,

      --  The following aspects correspond to library unit pragmas

      Aspect_All_Calls_Remote,
      Aspect_Compiler_Unit,                 -- GNAT
      Aspect_Elaborate_Body,
      Aspect_Preelaborate,
      Aspect_Preelaborate_05,               -- GNAT
      Aspect_Pure,
      Aspect_Pure_05,                       -- GNAT
      Aspect_Pure_12,                       -- GNAT
      Aspect_Remote_Call_Interface,
      Aspect_Remote_Types,
      Aspect_Shared_Passive,
      Aspect_Universal_Data,                -- GNAT

      --  Remaining aspects have a static boolean value that turns the aspect
      --  on or off. They all correspond to pragmas, but are only converted to
      --  the pragmas where the value is True. A value of False normally means
      --  that the aspect is ignored, except in the case of derived types where
      --  the aspect value is inherited from the parent, in which case, we do
      --  not allow False if we inherit a True value from the parent.

      Aspect_Ada_2005,                      -- GNAT
      Aspect_Ada_2012,                      -- GNAT
      Aspect_Asynchronous,
      Aspect_Atomic,
      Aspect_Atomic_Components,
      Aspect_Discard_Names,
      Aspect_Favor_Top_Level,               -- GNAT
      Aspect_Independent,
      Aspect_Independent_Components,
      Aspect_Inline,
      Aspect_Inline_Always,                 -- GNAT
      Aspect_Interrupt_Handler,
      Aspect_No_Return,
      Aspect_Pack,
      Aspect_Persistent_BSS,                -- GNAT
      Aspect_Preelaborable_Initialization,
      Aspect_Pure_Function,                 -- GNAT
      Aspect_Shared,                        -- GNAT (equivalent to Atomic)
      Aspect_Suppress_Debug_Info,           -- GNAT
      Aspect_Unchecked_Union,
      Aspect_Universal_Aliasing,            -- GNAT
      Aspect_Unmodified,                    -- GNAT
      Aspect_Unreferenced,                  -- GNAT
      Aspect_Unreferenced_Objects,          -- GNAT
      Aspect_Volatile,
      Aspect_Volatile_Components);

   --  The following array indicates aspects that accept 'Class

   Class_Aspect_OK : constant array (Aspect_Id) of Boolean :=
                       (Aspect_Invariant     => True,
                        Aspect_Pre           => True,
                        Aspect_Predicate     => True,
                        Aspect_Post          => True,
                        others               => False);

   --  The following array indicates aspects that a subtype inherits from
   --  its base type. True means that the subtype inherits the aspect from
   --  its base type. False means it is not inherited.

   Base_Aspect : constant array (Aspect_Id) of Boolean :=
                   (Aspect_Atomic                  => True,
                    Aspect_Atomic_Components       => True,
                    Aspect_Discard_Names           => True,
                    Aspect_Independent_Components  => True,
                    Aspect_Iterator_Element        => True,
                    Aspect_Constant_Indexing       => True,
                    Aspect_Default_Iterator        => True,
                    Aspect_Type_Invariant          => True,
                    Aspect_Unchecked_Union         => True,
                    Aspect_Variable_Indexing       => True,
                    Aspect_Volatile                => True,
                    others                         => False);

   --  The following array identifies all implementation defined aspects

   Impl_Defined_Aspects : constant array (Aspect_Id) of Boolean :=
                            (Aspect_Ada_2005             => True,
                             Aspect_Ada_2012             => True,
                             Aspect_Compiler_Unit        => True,
                             Aspect_Dimension            => True,
                             Aspect_Dimension_System     => True,
                             Aspect_Favor_Top_Level      => True,
                             Aspect_Inline_Always        => True,
                             Aspect_Object_Size          => True,
                             Aspect_Persistent_BSS       => True,
                             Aspect_Predicate            => True,
                             Aspect_Preelaborate_05      => True,
                             Aspect_Pure_05              => True,
                             Aspect_Pure_12              => True,
                             Aspect_Pure_Function        => True,
                             Aspect_Shared               => True,
                             Aspect_Suppress_Debug_Info  => True,
                             Aspect_Test_Case            => True,
                             Aspect_Universal_Data       => True,
                             Aspect_Universal_Aliasing   => True,
                             Aspect_Unmodified           => True,
                             Aspect_Unreferenced         => True,
                             Aspect_Unreferenced_Objects => True,
                             Aspect_Value_Size           => True,
                             others                      => False);

   --  The following array indicates aspects for which multiple occurrences of
   --  the same aspect attached to the same declaration are allowed.

   No_Duplicates_Allowed : constant array (Aspect_Id) of Boolean :=
                             (Aspect_Test_Case => False,
                              others           => True);

   --  The following array indicates type aspects that are inherited and apply
   --  to the class-wide type as well.

   Inherited_Aspect : constant array (Aspect_Id) of Boolean :=
                        (Aspect_Constant_Indexing    => True,
                         Aspect_Default_Iterator     => True,
                         Aspect_Implicit_Dereference => True,
                         Aspect_Iterator_Element     => True,
                         Aspect_Remote_Types         => True,
                         Aspect_Variable_Indexing    => True,
                         others                      => False);

   --  The following subtype defines aspects corresponding to library unit
   --  pragmas, these can only validly appear as aspects for library units,
   --  and result in a corresponding pragma being inserted immediately after
   --  the occurrence of the aspect.

   subtype Library_Unit_Aspects is
     Aspect_Id range Aspect_All_Calls_Remote .. Aspect_Universal_Data;

   --  The following subtype defines aspects accepting an optional static
   --  boolean parameter indicating if the aspect should be active or
   --  cancelling. If the parameter is missing the effective value is True,
   --  enabling the aspect. If the parameter is present it must be a static
   --  expression of type Standard.Boolean. If the value is True, then the
   --  aspect is enabled. If it is False, the aspect is disabled.

   subtype Boolean_Aspects is
     Aspect_Id range Aspect_Ada_2005 .. Aspect_Id'Last;

   subtype Pre_Post_Aspects is
     Aspect_Id range Aspect_Post .. Aspect_Precondition;

   --  The following type is used for indicating allowed expression forms

   type Aspect_Expression is
     (Optional,               -- Optional boolean expression
      Expression,             -- Required expression
      Name);                  -- Required name

   --  The following array indicates what argument type is required

   Aspect_Argument : constant array (Aspect_Id) of Aspect_Expression :=
                       (No_Aspect                      => Optional,
                        Aspect_Address                 => Expression,
                        Aspect_Alignment               => Expression,
                        Aspect_Attach_Handler          => Expression,
                        Aspect_Bit_Order               => Expression,
                        Aspect_Component_Size          => Expression,
                        Aspect_Constant_Indexing       => Name,
                        Aspect_CPU                     => Expression,
                        Aspect_Default_Component_Value => Expression,
                        Aspect_Default_Iterator        => Name,
                        Aspect_Default_Value           => Expression,
                        Aspect_Dimension               => Expression,
                        Aspect_Dimension_System        => Expression,
                        Aspect_Dispatching_Domain      => Expression,
                        Aspect_Dynamic_Predicate       => Expression,
                        Aspect_External_Tag            => Expression,
                        Aspect_Implicit_Dereference    => Name,
                        Aspect_Input                   => Name,
                        Aspect_Interrupt_Priority      => Expression,
                        Aspect_Invariant               => Expression,
                        Aspect_Iterator_Element        => Name,
                        Aspect_Machine_Radix           => Expression,
                        Aspect_Object_Size             => Expression,
                        Aspect_Output                  => Name,
                        Aspect_Post                    => Expression,
                        Aspect_Postcondition           => Expression,
                        Aspect_Pre                     => Expression,
                        Aspect_Precondition            => Expression,
                        Aspect_Predicate               => Expression,
                        Aspect_Priority                => Expression,
                        Aspect_Read                    => Name,
                        Aspect_Size                    => Expression,
                        Aspect_Small                   => Expression,
                        Aspect_Static_Predicate        => Expression,
                        Aspect_Storage_Pool            => Name,
                        Aspect_Storage_Size            => Expression,
                        Aspect_Stream_Size             => Expression,
                        Aspect_Suppress                => Name,
                        Aspect_Test_Case               => Expression,
                        Aspect_Type_Invariant          => Expression,
                        Aspect_Unsuppress              => Name,
                        Aspect_Value_Size              => Expression,
                        Aspect_Variable_Indexing       => Name,
                        Aspect_Warnings                => Name,
                        Aspect_Write                   => Name,

                        Library_Unit_Aspects           => Optional,
                        Boolean_Aspects                => Optional);

   -----------------------------------------
   -- Table Linking Names and Aspect_Id's --
   -----------------------------------------

   --  Table linking aspect names and id's

   Aspect_Names : constant array (Aspect_Id) of Name_Id := (
     No_Aspect                           => No_Name,
     Aspect_Ada_2005                     => Name_Ada_2005,
     Aspect_Ada_2012                     => Name_Ada_2012,
     Aspect_Address                      => Name_Address,
     Aspect_Alignment                    => Name_Alignment,
     Aspect_All_Calls_Remote             => Name_All_Calls_Remote,
     Aspect_Asynchronous                 => Name_Asynchronous,
     Aspect_Atomic                       => Name_Atomic,
     Aspect_Atomic_Components            => Name_Atomic_Components,
     Aspect_Attach_Handler               => Name_Attach_Handler,
     Aspect_Bit_Order                    => Name_Bit_Order,
     Aspect_Compiler_Unit                => Name_Compiler_Unit,
     Aspect_Component_Size               => Name_Component_Size,
     Aspect_Constant_Indexing            => Name_Constant_Indexing,
     Aspect_CPU                          => Name_CPU,
     Aspect_Default_Iterator             => Name_Default_Iterator,
     Aspect_Default_Value                => Name_Default_Value,
     Aspect_Default_Component_Value      => Name_Default_Component_Value,
     Aspect_Dimension                    => Name_Dimension,
     Aspect_Dimension_System             => Name_Dimension_System,
     Aspect_Discard_Names                => Name_Discard_Names,
     Aspect_Dispatching_Domain           => Name_Dispatching_Domain,
     Aspect_Dynamic_Predicate            => Name_Dynamic_Predicate,
     Aspect_Elaborate_Body               => Name_Elaborate_Body,
     Aspect_External_Tag                 => Name_External_Tag,
     Aspect_Favor_Top_Level              => Name_Favor_Top_Level,
     Aspect_Implicit_Dereference         => Name_Implicit_Dereference,
     Aspect_Independent                  => Name_Independent,
     Aspect_Independent_Components       => Name_Independent_Components,
     Aspect_Inline                       => Name_Inline,
     Aspect_Inline_Always                => Name_Inline_Always,
     Aspect_Input                        => Name_Input,
     Aspect_Interrupt_Handler            => Name_Interrupt_Handler,
     Aspect_Interrupt_Priority           => Name_Interrupt_Priority,
     Aspect_Invariant                    => Name_Invariant,
     Aspect_Iterator_Element             => Name_Iterator_Element,
     Aspect_Machine_Radix                => Name_Machine_Radix,
     Aspect_No_Return                    => Name_No_Return,
     Aspect_Object_Size                  => Name_Object_Size,
     Aspect_Output                       => Name_Output,
     Aspect_Pack                         => Name_Pack,
     Aspect_Persistent_BSS               => Name_Persistent_BSS,
     Aspect_Post                         => Name_Post,
     Aspect_Postcondition                => Name_Postcondition,
     Aspect_Pre                          => Name_Pre,
     Aspect_Precondition                 => Name_Precondition,
     Aspect_Predicate                    => Name_Predicate,
     Aspect_Preelaborable_Initialization => Name_Preelaborable_Initialization,
     Aspect_Preelaborate                 => Name_Preelaborate,
     Aspect_Preelaborate_05              => Name_Preelaborate_05,
     Aspect_Priority                     => Name_Priority,
     Aspect_Pure                         => Name_Pure,
     Aspect_Pure_05                      => Name_Pure_05,
     Aspect_Pure_12                      => Name_Pure_12,
     Aspect_Pure_Function                => Name_Pure_Function,
     Aspect_Read                         => Name_Read,
     Aspect_Remote_Call_Interface        => Name_Remote_Call_Interface,
     Aspect_Remote_Types                 => Name_Remote_Types,
     Aspect_Shared                       => Name_Shared,
     Aspect_Shared_Passive               => Name_Shared_Passive,
     Aspect_Size                         => Name_Size,
     Aspect_Small                        => Name_Small,
     Aspect_Static_Predicate             => Name_Static_Predicate,
     Aspect_Storage_Pool                 => Name_Storage_Pool,
     Aspect_Storage_Size                 => Name_Storage_Size,
     Aspect_Stream_Size                  => Name_Stream_Size,
     Aspect_Suppress                     => Name_Suppress,
     Aspect_Suppress_Debug_Info          => Name_Suppress_Debug_Info,
     Aspect_Test_Case                    => Name_Test_Case,
     Aspect_Type_Invariant               => Name_Type_Invariant,
     Aspect_Unchecked_Union              => Name_Unchecked_Union,
     Aspect_Universal_Aliasing           => Name_Universal_Aliasing,
     Aspect_Universal_Data               => Name_Universal_Data,
     Aspect_Unmodified                   => Name_Unmodified,
     Aspect_Unreferenced                 => Name_Unreferenced,
     Aspect_Unreferenced_Objects         => Name_Unreferenced_Objects,
     Aspect_Unsuppress                   => Name_Unsuppress,
     Aspect_Value_Size                   => Name_Value_Size,
     Aspect_Variable_Indexing            => Name_Variable_Indexing,
     Aspect_Volatile                     => Name_Volatile,
     Aspect_Volatile_Components          => Name_Volatile_Components,
     Aspect_Warnings                     => Name_Warnings,
     Aspect_Write                        => Name_Write);

   function Get_Aspect_Id (Name : Name_Id) return Aspect_Id;
   pragma Inline (Get_Aspect_Id);
   --  Given a name Nam, returns the corresponding aspect id value. If the name
   --  does not match any aspect, then No_Aspect is returned as the result.

   ---------------------------------------------------
   -- Handling of Aspect Specifications in the Tree --
   ---------------------------------------------------

   --  Several kinds of declaration node permit aspect specifications in Ada
   --  2012 mode. If there was room in all the corresponding declaration nodes,
   --  we could just have a field Aspect_Specifications pointing to a list of
   --  nodes for the aspects (N_Aspect_Specification nodes). But there isn't
   --  room, so we adopt a different approach.

   --  The following subprograms provide access to a specialized interface
   --  implemented internally with a hash table in the body, that provides
   --  access to aspect specifications.

   function Permits_Aspect_Specifications (N : Node_Id) return Boolean;
   --  Returns True if the node N is a declaration node that permits aspect
   --  specifications in the grammar. It is possible for other nodes to have
   --  aspect specifications as a result of Rewrite or Replace calls.

   function Aspect_Specifications (N : Node_Id) return List_Id;
   --  Given a node N, returns the list of N_Aspect_Specification nodes that
   --  are attached to this declaration node. If the node is in the class of
   --  declaration nodes that permit aspect specifications, as defined by the
   --  predicate above, and if their Has_Aspects flag is set to True, then this
   --  will always be a non-empty list. If this flag is set to False, then
   --  No_List is returned. Normally, the only nodes that have Has_Aspects set
   --  True are the nodes for which Permits_Aspect_Specifications would return
   --  True (i.e. the declaration nodes defined in the RM as permitting the
   --  presence of Aspect_Specifications). However, it is possible for the
   --  flag Has_Aspects to be set on other nodes as a result of Rewrite and
   --  Replace calls, and this function may be used to retrieve the aspect
   --  specifications for the original rewritten node in such cases.

   procedure Set_Aspect_Specifications (N : Node_Id; L : List_Id);
   --  The node N must be in the class of declaration nodes that permit aspect
   --  specifications and the Has_Aspects flag must be False on entry. L must
   --  be a non-empty list of N_Aspect_Specification nodes. This procedure sets
   --  the Has_Aspects flag to True, and makes an entry that can be retrieved
   --  by a subsequent Aspect_Specifications call. It is an error to call this
   --  procedure with a node that does not permit aspect specifications, or a
   --  node that has its Has_Aspects flag set True on entry, or with L being an
   --  empty list or No_List.

   function Find_Aspect (Ent : Entity_Id; A : Aspect_Id) return Node_Id;
   --  Find value of a given aspect from aspect list of entity

   procedure Move_Aspects (From : Node_Id; To : Node_Id);
   --  Moves aspects from 'From' node to 'To' node. Has_Aspects (To) must be
   --  False on entry. If Has_Aspects (From) is False, the call has no effect.
   --  Otherwise the aspects are moved and on return Has_Aspects (To) is True,
   --  and Has_Aspects (From) is False.

   function Same_Aspect (A1 : Aspect_Id; A2 : Aspect_Id) return Boolean;
   --  Returns True if A1 and A2 are (essentially) the same aspect. This is not
   --  a simple equality test because e.g. Post and Postcondition are the same.
   --  This is used for detecting duplicate aspects.

   procedure Tree_Write;
   --  Writes contents of Aspect_Specifications hash table to the tree file

   procedure Tree_Read;
   --  Reads contents of Aspect_Specifications hash table from the tree file

end Aspects;
