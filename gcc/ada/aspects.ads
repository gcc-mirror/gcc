------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              A S P E C T S                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2010-2013, Free Software Foundation, Inc.         --
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

------------------------
-- Adding New Aspects --
------------------------

--  In general, each aspect should have a corresponding pragma, so that the
--  newly developed functionality is available for Ada versions < Ada 2012.
--  When both are defined, it is convenient to first transform the aspect into
--  an equivalent pragma in Sem_Ch13.Analyze_Aspect_Specifications, and then
--  analyze the pragma in Sem_Prag.Analyze_Pragma.

--  To add a new aspect, you need to do the following

--    1. Create a name in snames.ads-tmpl

--    2. Create a value in type Aspect_Id in this unit

--    3. Add a value for the aspect in the global arrays defined in this unit

--    4. Add code for the aspect in Sem_Ch13.Analyze_Aspect_Specifications.
--       This may involve adding some nodes to the tree to perform additional
--       treatments later.

--    5. If the semantic analysis of expressions/names in the aspect should not
--       occur at the point the aspect is defined, add code in the adequate
--       semantic analysis procedure for the aspect. For example, this is the
--       case for aspects Pre and Post on subprograms, which are pre-analyzed
--       at the end of the declaration list to which the subprogram belongs,
--       and fully analyzed (possibly with expansion) during the semantic
--       analysis of subprogram bodies.

with Namet;  use Namet;
with Snames; use Snames;
with Types;  use Types;

package Aspects is

   --  Type defining recognized aspects

   type Aspect_Id is
     (No_Aspect,                            -- Dummy entry for no aspect
      Aspect_Abstract_State,                -- GNAT
      Aspect_Address,
      Aspect_Alignment,
      Aspect_Attach_Handler,
      Aspect_Bit_Order,
      Aspect_Component_Size,
      Aspect_Constant_Indexing,
      Aspect_Contract_Cases,                -- GNAT
      Aspect_Convention,
      Aspect_CPU,
      Aspect_Default_Component_Value,
      Aspect_Default_Iterator,
      Aspect_Default_Value,
      Aspect_Depends,                       -- GNAT
      Aspect_Dimension,                     -- GNAT
      Aspect_Dimension_System,              -- GNAT
      Aspect_Dispatching_Domain,
      Aspect_Dynamic_Predicate,
      Aspect_External_Name,
      Aspect_External_Tag,
      Aspect_Global,                        -- GNAT
      Aspect_Implicit_Dereference,
      Aspect_Initial_Condition,             -- GNAT
      Aspect_Initializes,                   -- GNAT
      Aspect_Input,
      Aspect_Interrupt_Priority,
      Aspect_Invariant,                     -- GNAT
      Aspect_Iterator_Element,
      Aspect_Link_Name,
      Aspect_Linker_Section,                -- GNAT
      Aspect_Machine_Radix,
      Aspect_Object_Size,                   -- GNAT
      Aspect_Output,
      Aspect_Part_Of,                       -- GNAT
      Aspect_Post,
      Aspect_Postcondition,
      Aspect_Pre,
      Aspect_Precondition,
      Aspect_Predicate,                     -- GNAT
      Aspect_Priority,
      Aspect_Read,
      Aspect_Refined_Depends,               -- GNAT
      Aspect_Refined_Global,                -- GNAT
      Aspect_Refined_Post,                  -- GNAT
      Aspect_Refined_State,                 -- GNAT
      Aspect_Relative_Deadline,
      Aspect_Scalar_Storage_Order,          -- GNAT
      Aspect_Simple_Storage_Pool,           -- GNAT
      Aspect_Size,
      Aspect_Small,
      Aspect_SPARK_Mode,                    -- GNAT
      Aspect_Static_Predicate,
      Aspect_Storage_Pool,
      Aspect_Storage_Size,
      Aspect_Stream_Size,
      Aspect_Suppress,
      Aspect_Synchronization,
      Aspect_Test_Case,                     -- GNAT
      Aspect_Type_Invariant,
      Aspect_Unsuppress,
      Aspect_Value_Size,                    -- GNAT
      Aspect_Variable_Indexing,
      Aspect_Warnings,                      -- GNAT
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
      Aspect_Async_Readers,                 -- GNAT
      Aspect_Async_Writers,                 -- GNAT
      Aspect_Asynchronous,
      Aspect_Atomic,
      Aspect_Atomic_Components,
      Aspect_Discard_Names,
      Aspect_Effective_Reads,               -- GNAT
      Aspect_Effective_Writes,              -- GNAT
      Aspect_Export,
      Aspect_Favor_Top_Level,               -- GNAT
      Aspect_Independent,
      Aspect_Independent_Components,
      Aspect_Import,
      Aspect_Inline,
      Aspect_Inline_Always,                 -- GNAT
      Aspect_Interrupt_Handler,
      Aspect_No_Return,
      Aspect_Pack,
      Aspect_Persistent_BSS,                -- GNAT
      Aspect_Preelaborable_Initialization,
      Aspect_Pure_Function,                 -- GNAT
      Aspect_Remote_Access_Type,            -- GNAT
      Aspect_Shared,                        -- GNAT (equivalent to Atomic)
      Aspect_Simple_Storage_Pool_Type,      -- GNAT
      Aspect_Suppress_Debug_Info,           -- GNAT
      Aspect_Unchecked_Union,
      Aspect_Universal_Aliasing,            -- GNAT
      Aspect_Unmodified,                    -- GNAT
      Aspect_Unreferenced,                  -- GNAT
      Aspect_Unreferenced_Objects,          -- GNAT
      Aspect_Volatile,
      Aspect_Volatile_Components,

      --  Aspects that have a static boolean value but don't correspond to
      --  pragmas

      Aspect_Lock_Free);                    -- GNAT

   subtype Aspect_Id_Exclude_No_Aspect is
     Aspect_Id range Aspect_Id'Succ (No_Aspect) .. Aspect_Id'Last;
   --  Aspect_Id's excluding No_Aspect

   --  The following array indicates aspects that accept 'Class

   Class_Aspect_OK : constant array (Aspect_Id) of Boolean :=
     (Aspect_Invariant      => True,
      Aspect_Pre            => True,
      Aspect_Predicate      => True,
      Aspect_Post           => True,
      Aspect_Type_Invariant => True,
      others                => False);

   --  The following array identifies all implementation defined aspects

   Implementation_Defined_Aspect : constant array (Aspect_Id) of Boolean :=
     (Aspect_Abstract_State           => True,
      Aspect_Ada_2005                 => True,
      Aspect_Ada_2012                 => True,
      Aspect_Async_Readers            => True,
      Aspect_Async_Writers            => True,
      Aspect_Compiler_Unit            => True,
      Aspect_Contract_Cases           => True,
      Aspect_Depends                  => True,
      Aspect_Dimension                => True,
      Aspect_Dimension_System         => True,
      Aspect_Effective_Reads          => True,
      Aspect_Effective_Writes         => True,
      Aspect_Favor_Top_Level          => True,
      Aspect_Global                   => True,
      Aspect_Inline_Always            => True,
      Aspect_Invariant                => True,
      Aspect_Lock_Free                => True,
      Aspect_Object_Size              => True,
      Aspect_Persistent_BSS           => True,
      Aspect_Predicate                => True,
      Aspect_Preelaborate_05          => True,
      Aspect_Pure_05                  => True,
      Aspect_Pure_12                  => True,
      Aspect_Pure_Function            => True,
      Aspect_Remote_Access_Type       => True,
      Aspect_Scalar_Storage_Order     => True,
      Aspect_Shared                   => True,
      Aspect_Simple_Storage_Pool      => True,
      Aspect_Simple_Storage_Pool_Type => True,
      Aspect_Suppress_Debug_Info      => True,
      Aspect_Test_Case                => True,
      Aspect_Universal_Aliasing       => True,
      Aspect_Universal_Data           => True,
      Aspect_Unmodified               => True,
      Aspect_Unreferenced             => True,
      Aspect_Unreferenced_Objects     => True,
      Aspect_Value_Size               => True,
      Aspect_Warnings                 => True,
      others                          => False);

   --  The following array indicates aspects for which multiple occurrences of
   --  the same aspect attached to the same declaration are allowed.

   No_Duplicates_Allowed : constant array (Aspect_Id) of Boolean :=
     (Aspect_Test_Case => False,
      others           => True);

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
     (Expression,             -- Required expression
      Name,                   -- Required name
      Optional_Expression,    -- Optional boolean expression
      Optional_Name);         -- Optional name

   --  The following array indicates what argument type is required

   Aspect_Argument : constant array (Aspect_Id) of Aspect_Expression :=
     (No_Aspect                      => Optional_Expression,
      Aspect_Abstract_State          => Expression,
      Aspect_Address                 => Expression,
      Aspect_Alignment               => Expression,
      Aspect_Attach_Handler          => Expression,
      Aspect_Bit_Order               => Expression,
      Aspect_Component_Size          => Expression,
      Aspect_Constant_Indexing       => Name,
      Aspect_Contract_Cases          => Expression,
      Aspect_Convention              => Name,
      Aspect_CPU                     => Expression,
      Aspect_Default_Component_Value => Expression,
      Aspect_Default_Iterator        => Name,
      Aspect_Default_Value           => Expression,
      Aspect_Depends                 => Expression,
      Aspect_Dimension               => Expression,
      Aspect_Dimension_System        => Expression,
      Aspect_Dispatching_Domain      => Expression,
      Aspect_Dynamic_Predicate       => Expression,
      Aspect_External_Name           => Expression,
      Aspect_External_Tag            => Expression,
      Aspect_Global                  => Expression,
      Aspect_Implicit_Dereference    => Name,
      Aspect_Initial_Condition       => Expression,
      Aspect_Initializes             => Expression,
      Aspect_Input                   => Name,
      Aspect_Interrupt_Priority      => Expression,
      Aspect_Invariant               => Expression,
      Aspect_Iterator_Element        => Name,
      Aspect_Link_Name               => Expression,
      Aspect_Linker_Section          => Expression,
      Aspect_Machine_Radix           => Expression,
      Aspect_Object_Size             => Expression,
      Aspect_Output                  => Name,
      Aspect_Part_Of                 => Expression,
      Aspect_Post                    => Expression,
      Aspect_Postcondition           => Expression,
      Aspect_Pre                     => Expression,
      Aspect_Precondition            => Expression,
      Aspect_Predicate               => Expression,
      Aspect_Priority                => Expression,
      Aspect_Read                    => Name,
      Aspect_Refined_Depends         => Expression,
      Aspect_Refined_Global          => Expression,
      Aspect_Refined_Post            => Expression,
      Aspect_Refined_State           => Expression,
      Aspect_Relative_Deadline       => Expression,
      Aspect_Scalar_Storage_Order    => Expression,
      Aspect_Simple_Storage_Pool     => Name,
      Aspect_Size                    => Expression,
      Aspect_Small                   => Expression,
      Aspect_SPARK_Mode              => Optional_Name,
      Aspect_Static_Predicate        => Expression,
      Aspect_Storage_Pool            => Name,
      Aspect_Storage_Size            => Expression,
      Aspect_Stream_Size             => Expression,
      Aspect_Suppress                => Name,
      Aspect_Synchronization         => Name,
      Aspect_Test_Case               => Expression,
      Aspect_Type_Invariant          => Expression,
      Aspect_Unsuppress              => Name,
      Aspect_Value_Size              => Expression,
      Aspect_Variable_Indexing       => Name,
      Aspect_Warnings                => Name,
      Aspect_Write                   => Name,

      Boolean_Aspects                => Optional_Expression,
      Library_Unit_Aspects           => Optional_Expression);

   -----------------------------------------
   -- Table Linking Names and Aspect_Id's --
   -----------------------------------------

   --  Table linking aspect names and id's

   Aspect_Names : constant array (Aspect_Id) of Name_Id :=
     (No_Aspect                           => No_Name,
      Aspect_Abstract_State               => Name_Abstract_State,
      Aspect_Ada_2005                     => Name_Ada_2005,
      Aspect_Ada_2012                     => Name_Ada_2012,
      Aspect_Address                      => Name_Address,
      Aspect_Alignment                    => Name_Alignment,
      Aspect_All_Calls_Remote             => Name_All_Calls_Remote,
      Aspect_Async_Readers                => Name_Async_Readers,
      Aspect_Async_Writers                => Name_Async_Writers,
      Aspect_Asynchronous                 => Name_Asynchronous,
      Aspect_Atomic                       => Name_Atomic,
      Aspect_Atomic_Components            => Name_Atomic_Components,
      Aspect_Attach_Handler               => Name_Attach_Handler,
      Aspect_Bit_Order                    => Name_Bit_Order,
      Aspect_Compiler_Unit                => Name_Compiler_Unit,
      Aspect_Component_Size               => Name_Component_Size,
      Aspect_Constant_Indexing            => Name_Constant_Indexing,
      Aspect_Contract_Cases               => Name_Contract_Cases,
      Aspect_Convention                   => Name_Convention,
      Aspect_CPU                          => Name_CPU,
      Aspect_Default_Iterator             => Name_Default_Iterator,
      Aspect_Default_Value                => Name_Default_Value,
      Aspect_Default_Component_Value      => Name_Default_Component_Value,
      Aspect_Depends                      => Name_Depends,
      Aspect_Dimension                    => Name_Dimension,
      Aspect_Dimension_System             => Name_Dimension_System,
      Aspect_Discard_Names                => Name_Discard_Names,
      Aspect_Dispatching_Domain           => Name_Dispatching_Domain,
      Aspect_Dynamic_Predicate            => Name_Dynamic_Predicate,
      Aspect_Effective_Reads              => Name_Effective_Reads,
      Aspect_Effective_Writes             => Name_Effective_Writes,
      Aspect_Elaborate_Body               => Name_Elaborate_Body,
      Aspect_External_Name                => Name_External_Name,
      Aspect_External_Tag                 => Name_External_Tag,
      Aspect_Export                       => Name_Export,
      Aspect_Favor_Top_Level              => Name_Favor_Top_Level,
      Aspect_Global                       => Name_Global,
      Aspect_Implicit_Dereference         => Name_Implicit_Dereference,
      Aspect_Import                       => Name_Import,
      Aspect_Independent                  => Name_Independent,
      Aspect_Independent_Components       => Name_Independent_Components,
      Aspect_Inline                       => Name_Inline,
      Aspect_Inline_Always                => Name_Inline_Always,
      Aspect_Initial_Condition            => Name_Initial_Condition,
      Aspect_Initializes                  => Name_Initializes,
      Aspect_Input                        => Name_Input,
      Aspect_Interrupt_Handler            => Name_Interrupt_Handler,
      Aspect_Interrupt_Priority           => Name_Interrupt_Priority,
      Aspect_Invariant                    => Name_Invariant,
      Aspect_Iterator_Element             => Name_Iterator_Element,
      Aspect_Link_Name                    => Name_Link_Name,
      Aspect_Linker_Section               => Name_Linker_Section,
      Aspect_Lock_Free                    => Name_Lock_Free,
      Aspect_Machine_Radix                => Name_Machine_Radix,
      Aspect_No_Return                    => Name_No_Return,
      Aspect_Object_Size                  => Name_Object_Size,
      Aspect_Output                       => Name_Output,
      Aspect_Pack                         => Name_Pack,
      Aspect_Part_Of                      => Name_Part_Of,
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
      Aspect_Refined_Depends              => Name_Refined_Depends,
      Aspect_Refined_Global               => Name_Refined_Global,
      Aspect_Refined_Post                 => Name_Refined_Post,
      Aspect_Refined_State                => Name_Refined_State,
      Aspect_Relative_Deadline            => Name_Relative_Deadline,
      Aspect_Remote_Access_Type           => Name_Remote_Access_Type,
      Aspect_Remote_Call_Interface        => Name_Remote_Call_Interface,
      Aspect_Remote_Types                 => Name_Remote_Types,
      Aspect_Scalar_Storage_Order         => Name_Scalar_Storage_Order,
      Aspect_Shared                       => Name_Shared,
      Aspect_Shared_Passive               => Name_Shared_Passive,
      Aspect_Simple_Storage_Pool          => Name_Simple_Storage_Pool,
      Aspect_Simple_Storage_Pool_Type     => Name_Simple_Storage_Pool_Type,
      Aspect_Size                         => Name_Size,
      Aspect_Small                        => Name_Small,
      Aspect_SPARK_Mode                   => Name_SPARK_Mode,
      Aspect_Static_Predicate             => Name_Static_Predicate,
      Aspect_Storage_Pool                 => Name_Storage_Pool,
      Aspect_Storage_Size                 => Name_Storage_Size,
      Aspect_Stream_Size                  => Name_Stream_Size,
      Aspect_Suppress                     => Name_Suppress,
      Aspect_Suppress_Debug_Info          => Name_Suppress_Debug_Info,
      Aspect_Synchronization              => Name_Synchronization,
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

   function Get_Aspect_Id (Aspect : Node_Id) return Aspect_Id;
   pragma Inline (Get_Aspect_Id);
   --  Given an aspect specification, return the corresponding aspect_id value.
   --  If the name does not match any aspect, return No_Aspect.

   ------------------------------------
   -- Delaying Evaluation of Aspects --
   ------------------------------------

   --  The RM requires that all language defined aspects taking an expression
   --  delay evaluation of the expression till the freeze point of the entity
   --  to which the aspect applies. This allows forward references, and is of
   --  use for example in connection with preconditions and postconditions
   --  where the requirement of making all references in contracts to local
   --  functions be backwards references would be onerous.

   --  For consistency, even attributes like Size are delayed, so we can do:

   --    type A is range 1 .. 10
   --      with Size => Not_Defined_Yet;
   --    ..
   --    Not_Defined_Yet : constant := 64;

   --  Resulting in A having a size of 64, which gets set when A is frozen.
   --  Furthermore, we can have a situation like

   --    type A is range 1 .. 10
   --      with Size => Not_Defined_Yet;
   --    ..
   --    type B is new A;
   --    ..
   --    Not_Defined_Yet : constant := 64;

   --  where the Size of A is considered to have been previously specified at
   --  the point of derivation, even though the actual value of the size is
   --  not known yet, and in this example B inherits the size value of 64.

   --  Our normal implementation model (prior to Ada 2012) was simply to copy
   --  inheritable attributes at the point of derivation. Then any subsequent
   --  representation items apply either to the parent type, not affecting the
   --  derived type, or to the derived type, not affecting the parent type.

   --  To deal with the delayed aspect case, we use two flags. The first is
   --  set on the parent type if it has delayed representation aspects. This
   --  flag Has_Delayed_Rep_Aspects indicates that if we derive from this type
   --  we have to worry about making sure we inherit any delayed aspects. The
   --  second flag is set on a derived type: May_Have_Inherited_Rep_Aspects
   --  is set if the parent type has Has_Delayed_Rep_Aspects set.

   --  When we freeze a derived type, if the May_Have_Inherited_Rep_Aspects
   --  flag is set, then we call Freeze.Inherit_Delayed_Rep_Aspects when
   --  the derived type is frozen, which deals with the necessary copying of
   --  information from the parent type, which must be frozen at that point
   --  (since freezing the derived type first freezes the parent type).

   --  The following shows which aspects are delayed. There are three cases:

   type Delay_Type is
     (Always_Delay,
      --  This aspect is not a representation aspect that can be inherited and
      --  is always delayed, as required by the language definition.

      Never_Delay,
      --  There are two cases. There are language defined aspects like
      --  Convention where the "expression" is simply an uninterpreted
      --  identifier, and there is no issue of evaluating it and thus no
      --  issue of delaying the evaluation. The second case is implementation
      --  defined aspects where we have decided that we don't want to allow
      --  delays (and for our own aspects we can do what we like!).

      Rep_Aspect);
      --  These are the cases of representation aspects that are in general
      --  delayed, and where there is a potential issue of derived types that
      --  inherit delayed representation values.

   --  Note: even if this table indicates that an aspect is delayed, we never
   --  delay Boolean aspects that have a missing expression (taken as True),
   --  or expressions for delayed rep items that consist of an integer literal
   --  (most cases of Size etc. in practice), since in these cases we know we
   --  can get the value of the expression without delay. Note that we still
   --  need to delay Boolean aspects that are specifically set to True:

   --     type R is array (0 .. 31) of Boolean
   --       with Pack => True;
   --     True : constant Boolean := False;

   --  This is nonsense, but we need to make it work and result in R not
   --  being packed, and if we have something like:

   --     type R is array (0 .. 31) of Boolean
   --       with Pack => True;
   --     RR : R;
   --     True : constant Boolean := False;

   --  This is illegal because the visibility of True changes after the freeze
   --  point, which is not allowed, and we need the delay mechanism to properly
   --  diagnose this error.

   Aspect_Delay : constant array (Aspect_Id) of Delay_Type :=
     (No_Aspect                           => Always_Delay,
      Aspect_Address                      => Always_Delay,
      Aspect_All_Calls_Remote             => Always_Delay,
      Aspect_Async_Readers                => Always_Delay,
      Aspect_Async_Writers                => Always_Delay,
      Aspect_Asynchronous                 => Always_Delay,
      Aspect_Attach_Handler               => Always_Delay,
      Aspect_Compiler_Unit                => Always_Delay,
      Aspect_Constant_Indexing            => Always_Delay,
      Aspect_Contract_Cases               => Always_Delay,
      Aspect_CPU                          => Always_Delay,
      Aspect_Default_Iterator             => Always_Delay,
      Aspect_Default_Value                => Always_Delay,
      Aspect_Default_Component_Value      => Always_Delay,
      Aspect_Depends                      => Always_Delay,
      Aspect_Discard_Names                => Always_Delay,
      Aspect_Dispatching_Domain           => Always_Delay,
      Aspect_Dynamic_Predicate            => Always_Delay,
      Aspect_Effective_Reads              => Always_Delay,
      Aspect_Effective_Writes             => Always_Delay,
      Aspect_Elaborate_Body               => Always_Delay,
      Aspect_External_Name                => Always_Delay,
      Aspect_External_Tag                 => Always_Delay,
      Aspect_Export                       => Always_Delay,
      Aspect_Favor_Top_Level              => Always_Delay,
      Aspect_Global                       => Always_Delay,
      Aspect_Implicit_Dereference         => Always_Delay,
      Aspect_Import                       => Always_Delay,
      Aspect_Independent                  => Always_Delay,
      Aspect_Independent_Components       => Always_Delay,
      Aspect_Inline                       => Always_Delay,
      Aspect_Inline_Always                => Always_Delay,
      Aspect_Initial_Condition            => Always_Delay,
      Aspect_Initializes                  => Always_Delay,
      Aspect_Input                        => Always_Delay,
      Aspect_Interrupt_Handler            => Always_Delay,
      Aspect_Interrupt_Priority           => Always_Delay,
      Aspect_Invariant                    => Always_Delay,
      Aspect_Iterator_Element             => Always_Delay,
      Aspect_Link_Name                    => Always_Delay,
      Aspect_Linker_Section               => Always_Delay,
      Aspect_Lock_Free                    => Always_Delay,
      Aspect_No_Return                    => Always_Delay,
      Aspect_Output                       => Always_Delay,
      Aspect_Persistent_BSS               => Always_Delay,
      Aspect_Post                         => Always_Delay,
      Aspect_Postcondition                => Always_Delay,
      Aspect_Pre                          => Always_Delay,
      Aspect_Precondition                 => Always_Delay,
      Aspect_Predicate                    => Always_Delay,
      Aspect_Preelaborable_Initialization => Always_Delay,
      Aspect_Preelaborate                 => Always_Delay,
      Aspect_Preelaborate_05              => Always_Delay,
      Aspect_Priority                     => Always_Delay,
      Aspect_Pure                         => Always_Delay,
      Aspect_Pure_05                      => Always_Delay,
      Aspect_Pure_12                      => Always_Delay,
      Aspect_Pure_Function                => Always_Delay,
      Aspect_Read                         => Always_Delay,
      Aspect_Refined_Depends              => Always_Delay,
      Aspect_Refined_Global               => Always_Delay,
      Aspect_Refined_State                => Always_Delay,
      Aspect_Relative_Deadline            => Always_Delay,
      Aspect_Remote_Access_Type           => Always_Delay,
      Aspect_Remote_Call_Interface        => Always_Delay,
      Aspect_Remote_Types                 => Always_Delay,
      Aspect_Shared                       => Always_Delay,
      Aspect_Shared_Passive               => Always_Delay,
      Aspect_Simple_Storage_Pool          => Always_Delay,
      Aspect_Simple_Storage_Pool_Type     => Always_Delay,
      Aspect_Static_Predicate             => Always_Delay,
      Aspect_Storage_Pool                 => Always_Delay,
      Aspect_Stream_Size                  => Always_Delay,
      Aspect_Suppress                     => Always_Delay,
      Aspect_Suppress_Debug_Info          => Always_Delay,
      Aspect_Type_Invariant               => Always_Delay,
      Aspect_Unchecked_Union              => Always_Delay,
      Aspect_Universal_Aliasing           => Always_Delay,
      Aspect_Universal_Data               => Always_Delay,
      Aspect_Unmodified                   => Always_Delay,
      Aspect_Unreferenced                 => Always_Delay,
      Aspect_Unreferenced_Objects         => Always_Delay,
      Aspect_Unsuppress                   => Always_Delay,
      Aspect_Variable_Indexing            => Always_Delay,
      Aspect_Write                        => Always_Delay,

      Aspect_Abstract_State               => Never_Delay,
      Aspect_Ada_2005                     => Never_Delay,
      Aspect_Ada_2012                     => Never_Delay,
      Aspect_Convention                   => Never_Delay,
      Aspect_Dimension                    => Never_Delay,
      Aspect_Dimension_System             => Never_Delay,
      Aspect_Part_Of                      => Never_Delay,
      Aspect_Refined_Post                 => Never_Delay,
      Aspect_SPARK_Mode                   => Never_Delay,
      Aspect_Synchronization              => Never_Delay,
      Aspect_Test_Case                    => Never_Delay,
      Aspect_Warnings                     => Never_Delay,

      Aspect_Alignment                    => Rep_Aspect,
      Aspect_Atomic                       => Rep_Aspect,
      Aspect_Atomic_Components            => Rep_Aspect,
      Aspect_Bit_Order                    => Rep_Aspect,
      Aspect_Component_Size               => Rep_Aspect,
      Aspect_Machine_Radix                => Rep_Aspect,
      Aspect_Object_Size                  => Rep_Aspect,
      Aspect_Pack                         => Rep_Aspect,
      Aspect_Scalar_Storage_Order         => Rep_Aspect,
      Aspect_Size                         => Rep_Aspect,
      Aspect_Small                        => Rep_Aspect,
      Aspect_Storage_Size                 => Rep_Aspect,
      Aspect_Value_Size                   => Rep_Aspect,
      Aspect_Volatile                     => Rep_Aspect,
      Aspect_Volatile_Components          => Rep_Aspect);

   ------------------------------------------------
   -- Handling of Aspect Specifications on Stubs --
   ------------------------------------------------

   --  Aspects that appear on the following stub nodes

   --    N_Package_Body_Stub
   --    N_Protected_Body_Stub
   --    N_Subprogram_Body_Stub
   --    N_Task_Body_Stub

   --  are treated as if they apply to the corresponding proper body. Their
   --  analysis is postponed until the analysis of the proper body takes place
   --  (see Analyze_Proper_Body). The delay is required because the analysis
   --  may generate extra code which would be harder to relocate to the body.
   --  If the proper body is present, the aspect specifications are relocated
   --  to the corresponding body node:

   --    N_Package_Body
   --    N_Protected_Body
   --    N_Subprogram_Body
   --    N_Task_Body

   --  The subsequent analysis takes care of the aspect-to-pragma conversions
   --  and verification of pragma legality. In the case where the proper body
   --  is not available, the aspect specifications are analyzed on the spot
   --  (see Analyze_Proper_Body) to catch potential errors.

   --  The following table lists all aspects that can apply to a subprogram
   --  body [stub]. For instance, the following example is legal:

   --    package P with SPARK_Mode ...;
   --    package body P with SPARK_Mode is ...;

   --  The table should be synchronized with Pragma_On_Body_Or_Stub_OK in unit
   --  Sem_Prag if the aspects below are implemented by a pragma.

   Aspect_On_Body_Or_Stub_OK : constant array (Aspect_Id) of Boolean :=
     (Aspect_Refined_Depends              => True,
      Aspect_Refined_Global               => True,
      Aspect_Refined_Post                 => True,
      Aspect_SPARK_Mode                   => True,
      Aspect_Warnings                     => True,
      others                              => False);

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

   function Aspects_On_Body_Or_Stub_OK (N : Node_Id) return Boolean;
   --  N denotes a body [stub] with aspects. Determine whether all aspects of N
   --  are allowed to appear on a body [stub].

   function Find_Aspect (Id : Entity_Id; A : Aspect_Id) return Node_Id;
   --  Find the aspect specification of aspect A associated with entity I.
   --  Return Empty if Id does not have the requested aspect.

   function Find_Value_Of_Aspect
     (Id : Entity_Id;
      A  : Aspect_Id) return Node_Id;
   --  Find the value of aspect A associated with entity Id. Return Empty if
   --  Id does not have the requested aspect.

   function Has_Aspect (Id : Entity_Id; A : Aspect_Id) return Boolean;
   --  Determine whether entity Id has aspect A

   procedure Move_Aspects (From : Node_Id; To : Node_Id);
   --  Relocate the aspect specifications of node From to node To. On entry it
   --  is assumed that To does not have aspect specifications. If From has no
   --  aspects, the routine has no effect.

   procedure Move_Or_Merge_Aspects (From : Node_Id; To : Node_Id);
   --  Relocate the aspect specifications of node From to node To. If To has
   --  aspects, the aspects of From are added to the aspects of To. If From has
   --  no aspects, the routine has no effect. When From denotes a subprogram
   --  body stub that also acts as a spec, the only aspects relocated to node
   --  To are those from table Aspect_On_Body_Or_Stub_OK and preconditions.

   function Permits_Aspect_Specifications (N : Node_Id) return Boolean;
   --  Returns True if the node N is a declaration node that permits aspect
   --  specifications in the grammar. It is possible for other nodes to have
   --  aspect specifications as a result of Rewrite or Replace calls.

   procedure Remove_Aspects (N : Node_Id);
   --  Delete the aspect specifications associated with node N. If the node has
   --  no aspects, the routine has no effect.

   function Same_Aspect (A1 : Aspect_Id; A2 : Aspect_Id) return Boolean;
   --  Returns True if A1 and A2 are (essentially) the same aspect. This is not
   --  a simple equality test because e.g. Post and Postcondition are the same.
   --  This is used for detecting duplicate aspects.

   procedure Set_Aspect_Specifications (N : Node_Id; L : List_Id);
   --  The node N must be in the class of declaration nodes that permit aspect
   --  specifications and the Has_Aspects flag must be False on entry. L must
   --  be a non-empty list of N_Aspect_Specification nodes. This procedure sets
   --  the Has_Aspects flag to True, and makes an entry that can be retrieved
   --  by a subsequent Aspect_Specifications call. It is an error to call this
   --  procedure with a node that does not permit aspect specifications, or a
   --  node that has its Has_Aspects flag set True on entry, or with L being an
   --  empty list or No_List.

   procedure Tree_Read;
   --  Reads contents of Aspect_Specifications hash table from the tree file

   procedure Tree_Write;
   --  Writes contents of Aspect_Specifications hash table to the tree file

end Aspects;
