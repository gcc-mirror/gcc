------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              A S P E C T S                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2010-2024, Free Software Foundation, Inc.         --
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

--  This package defines the aspects that are recognized by GNAT in aspect
--  specifications. It also contains the subprograms for storing/retrieving
--  aspect specifications from the tree. The semantic processing for aspect
--  specifications is found in Sem_Ch13.Analyze_Aspect_Specifications.

------------------------
-- Adding New Aspects --
------------------------

--  In general, each aspect should have a corresponding pragma or attribute, so
--  that the newly developed functionality is available for old Ada versions.
--  When both are defined, it is convenient to first transform the aspect into
--  an equivalent pragma or attribute in Sem_Ch13.Analyze_Aspect_Specifications
--  and then analyze that.

--  To add a new aspect, you need to do the following

--    1. Create a name in snames.ads-tmpl

--    2. Create a value in type Aspect_Id in this unit

--    3. Add a value for the aspect in the global arrays defined in this unit

--    4. Add code for the aspect in Sem_Ch13.Analyze_Aspect_Specifications.
--       This may involve adding some nodes to the tree to perform additional
--       treatments later.

--    5. If the semantic analysis of expressions/names in the aspect should not
--       occur at the point the aspect is defined, add code in the appropriate
--       semantic analysis procedure for the aspect. For example, this is the
--       case for aspects Pre and Post on subprograms, which are preanalyzed
--       at the end of the declaration list to which the subprogram belongs,
--       and fully analyzed (possibly with expansion) during the semantic
--       analysis of subprogram bodies.

with Namet;   use Namet;
with Snames;  use Snames;
with Types;   use Types;

package Aspects is

   --  Type defining recognized aspects

   type Aspect_Id is
     (No_Aspect,                            -- Dummy entry for no aspect
      Aspect_Abstract_State,                -- GNAT
      Aspect_Address,
      Aspect_Aggregate,
      Aspect_Alignment,
      Aspect_Always_Terminates,             -- GNAT
      Aspect_Annotate,                      -- GNAT
      Aspect_Async_Readers,                 -- GNAT
      Aspect_Async_Writers,                 -- GNAT
      Aspect_Attach_Handler,
      Aspect_Bit_Order,
      Aspect_Component_Size,
      Aspect_Constant_After_Elaboration,    -- GNAT
      Aspect_Constant_Indexing,
      Aspect_Contract_Cases,                -- GNAT
      Aspect_Convention,
      Aspect_CPU,
      Aspect_Default_Component_Value,
      Aspect_Default_Initial_Condition,     -- GNAT
      Aspect_Default_Iterator,
      Aspect_Default_Storage_Pool,
      Aspect_Default_Value,
      Aspect_Depends,                       -- GNAT
      Aspect_Designated_Storage_Model,      -- GNAT
      Aspect_Dimension,                     -- GNAT
      Aspect_Dimension_System,              -- GNAT
      Aspect_Dispatching_Domain,
      Aspect_Dynamic_Predicate,
      Aspect_Effective_Reads,               -- GNAT
      Aspect_Effective_Writes,              -- GNAT
      Aspect_Exceptional_Cases,             -- GNAT
      Aspect_Extensions_Visible,            -- GNAT
      Aspect_External_Name,
      Aspect_External_Tag,
      Aspect_Ghost,                         -- GNAT
      Aspect_Ghost_Predicate,               -- GNAT
      Aspect_Global,                        -- GNAT
      Aspect_GNAT_Annotate,                 -- GNAT
      Aspect_Implicit_Dereference,
      Aspect_Initial_Condition,             -- GNAT
      Aspect_Initializes,                   -- GNAT
      Aspect_Input,
      Aspect_Integer_Literal,
      Aspect_Interrupt_Priority,
      Aspect_Invariant,                     -- GNAT
      Aspect_Iterator_Element,
      Aspect_Iterable,                      -- GNAT
      Aspect_Link_Name,
      Aspect_Linker_Section,                -- GNAT
      Aspect_Local_Restrictions,            -- GNAT
      Aspect_Machine_Radix,
      Aspect_Max_Entry_Queue_Depth,         -- GNAT
      Aspect_Max_Entry_Queue_Length,
      Aspect_Max_Queue_Length,              -- GNAT
      Aspect_No_Caching,                    -- GNAT
      Aspect_No_Controlled_Parts,
      Aspect_No_Task_Parts,                 -- GNAT
      Aspect_Object_Size,                   -- GNAT
      Aspect_Obsolescent,                   -- GNAT
      Aspect_Output,
      Aspect_Part_Of,                       -- GNAT
      Aspect_Post,
      Aspect_Postcondition,
      Aspect_Pre,
      Aspect_Precondition,
      Aspect_Predicate,                     -- GNAT
      Aspect_Predicate_Failure,
      Aspect_Priority,
      Aspect_Put_Image,
      Aspect_Read,
      Aspect_Real_Literal,
      Aspect_Refined_Depends,               -- GNAT
      Aspect_Refined_Global,                -- GNAT
      Aspect_Refined_Post,                  -- GNAT
      Aspect_Refined_State,                 -- GNAT
      Aspect_Relative_Deadline,
      Aspect_Relaxed_Initialization,        -- GNAT
      Aspect_Scalar_Storage_Order,          -- GNAT
      Aspect_Secondary_Stack_Size,          -- GNAT
      Aspect_Side_Effects,                  -- GNAT
      Aspect_Simple_Storage_Pool,           -- GNAT
      Aspect_Size,
      Aspect_Small,
      Aspect_SPARK_Mode,                    -- GNAT
      Aspect_Stable_Properties,
      Aspect_Static_Predicate,
      Aspect_Storage_Model_Type,            -- GNAT
      Aspect_Storage_Pool,
      Aspect_Storage_Size,
      Aspect_Stream_Size,
      Aspect_String_Literal,
      Aspect_Subprogram_Variant,            -- GNAT
      Aspect_Suppress,
      Aspect_Synchronization,
      Aspect_Test_Case,                     -- GNAT
      Aspect_Type_Invariant,
      Aspect_Unimplemented,                 -- GNAT
      Aspect_Unsuppress,
      Aspect_User_Aspect,                   -- GNAT
      Aspect_Value_Size,                    -- GNAT
      Aspect_Variable_Indexing,
      Aspect_Volatile_Function,             -- GNAT
      Aspect_Warnings,                      -- GNAT
      Aspect_Write,

      --  The following aspects correspond to library unit pragmas

      Aspect_All_Calls_Remote,
      Aspect_Elaborate_Body,
      Aspect_No_Elaboration_Code_All,       -- GNAT
      Aspect_Preelaborate,
      Aspect_Pure,
      Aspect_Remote_Call_Interface,
      Aspect_Remote_Types,
      Aspect_Shared_Passive,

      --  Remaining aspects have a static boolean value that turns the aspect
      --  on or off. They all correspond to pragmas, but are only converted to
      --  the pragmas where the value is True. A value of False normally means
      --  that the aspect is ignored, except in the case of derived types where
      --  the aspect value is inherited from the parent, in which case, we do
      --  not allow False if we inherit a True value from the parent.

      Aspect_Asynchronous,
      Aspect_Atomic,
      Aspect_Atomic_Components,
      Aspect_Disable_Controlled,            -- GNAT
      Aspect_Discard_Names,
      Aspect_CUDA_Device,                   -- GNAT
      Aspect_CUDA_Global,                   -- GNAT
      Aspect_Exclusive_Functions,
      Aspect_Export,
      Aspect_Favor_Top_Level,               -- GNAT
      Aspect_Full_Access_Only,
      Aspect_Independent,
      Aspect_Independent_Components,
      Aspect_Import,
      Aspect_Inline,
      Aspect_Inline_Always,                 -- GNAT
      Aspect_Interrupt_Handler,
      Aspect_Lock_Free,                     -- GNAT
      Aspect_No_Inline,                     -- GNAT
      Aspect_No_Return,
      Aspect_No_Tagged_Streams,             -- GNAT
      Aspect_Pack,
      Aspect_Persistent_BSS,                -- GNAT
      Aspect_Preelaborable_Initialization,
      Aspect_Pure_Function,                 -- GNAT
      Aspect_Remote_Access_Type,            -- GNAT
      Aspect_Shared,                        -- GNAT (equivalent to Atomic)
      Aspect_Simple_Storage_Pool_Type,      -- GNAT
      Aspect_Static,
      Aspect_Suppress_Debug_Info,           -- GNAT
      Aspect_Suppress_Initialization,       -- GNAT
      Aspect_Thread_Local_Storage,          -- GNAT
      Aspect_Unchecked_Union,
      Aspect_Universal_Aliasing,            -- GNAT
      Aspect_Unmodified,                    -- GNAT
      Aspect_Unreferenced,                  -- GNAT
      Aspect_Unreferenced_Objects,          -- GNAT
      Aspect_Volatile,
      Aspect_Volatile_Components,
      Aspect_Volatile_Full_Access,          -- GNAT
      Aspect_Yield);

   subtype Aspect_Id_Exclude_No_Aspect is
     Aspect_Id range Aspect_Id'Succ (No_Aspect) .. Aspect_Id'Last;
   --  Aspect_Id's excluding No_Aspect

   subtype Nonoverridable_Aspect_Id is Aspect_Id with
     Static_Predicate => Nonoverridable_Aspect_Id in
       Aspect_Default_Iterator | Aspect_Iterator_Element |
       Aspect_Implicit_Dereference | Aspect_Constant_Indexing |
       Aspect_Variable_Indexing | Aspect_Aggregate |
       Aspect_Max_Entry_Queue_Length
        | Aspect_No_Controlled_Parts
       --  ??? No_Controlled_Parts not yet in Aspect_Id enumeration
       ;  --  see RM 13.1.1(18.7)

   --  The following array indicates aspects that accept 'Class

   Class_Aspect_OK : constant array (Aspect_Id) of Boolean :=
     (Aspect_Input             => True,
      Aspect_Invariant         => True,
      Aspect_Output            => True,
      Aspect_Pre               => True,
      Aspect_Predicate         => True,
      Aspect_Post              => True,
      Aspect_Read              => True,
      Aspect_Write             => True,
      Aspect_Stable_Properties => True,
      Aspect_Type_Invariant    => True,
      others                   => False);

   --  The following array identifies all implementation defined aspects

   Implementation_Defined_Aspect : constant array (Aspect_Id) of Boolean :=
     (Aspect_Abstract_State             => True,
      Aspect_Always_Terminates          => True,
      Aspect_Annotate                   => True,
      Aspect_Async_Readers              => True,
      Aspect_Async_Writers              => True,
      Aspect_Constant_After_Elaboration => True,
      Aspect_Contract_Cases             => True,
      Aspect_Depends                    => True,
      Aspect_Dimension                  => True,
      Aspect_Dimension_System           => True,
      Aspect_Effective_Reads            => True,
      Aspect_Effective_Writes           => True,
      Aspect_Exceptional_Cases          => True,
      Aspect_Extensions_Visible         => True,
      Aspect_Favor_Top_Level            => True,
      Aspect_Ghost                      => True,
      Aspect_Ghost_Predicate            => True,
      Aspect_Global                     => True,
      Aspect_GNAT_Annotate              => True,
      Aspect_Inline_Always              => True,
      Aspect_Invariant                  => True,
      Aspect_Lock_Free                  => True,
      Aspect_Max_Entry_Queue_Depth      => True,
      Aspect_Max_Entry_Queue_Length     => True,
      Aspect_Max_Queue_Length           => True,
      Aspect_Object_Size                => True,
      Aspect_Persistent_BSS             => True,
      Aspect_Predicate                  => True,
      Aspect_Pure_Function              => True,
      Aspect_Relaxed_Initialization     => True,
      Aspect_Remote_Access_Type         => True,
      Aspect_Scalar_Storage_Order       => True,
      Aspect_Secondary_Stack_Size       => True,
      Aspect_Shared                     => True,
      Aspect_Side_Effects               => True,
      Aspect_Simple_Storage_Pool        => True,
      Aspect_Simple_Storage_Pool_Type   => True,
      Aspect_Subprogram_Variant         => True,
      Aspect_Suppress_Debug_Info        => True,
      Aspect_Suppress_Initialization    => True,
      Aspect_Thread_Local_Storage       => True,
      Aspect_Test_Case                  => True,
      Aspect_Universal_Aliasing         => True,
      Aspect_Unmodified                 => True,
      Aspect_Unreferenced               => True,
      Aspect_Unreferenced_Objects       => True,
      Aspect_Value_Size                 => True,
      Aspect_Volatile_Function          => True,
      Aspect_Warnings                   => True,
      others                            => False);

   --  The following array indicates aspects that specify operational
   --  characteristics, and thus are view-specific. Representation
   --  aspects break privacy, as they are needed during expansion and
   --  code generation.
   --  List is currently incomplete ???

   Operational_Aspect : constant array (Aspect_Id) of Boolean :=
     (Aspect_Constant_Indexing          => True,
      Aspect_Default_Iterator           => True,
      Aspect_Iterator_Element           => True,
      Aspect_Iterable                   => True,
      Aspect_Variable_Indexing          => True,
      Aspect_Aggregate                  => True,
      others                            => False);

   --  The following array indicates aspects for which multiple occurrences of
   --  the same aspect attached to the same declaration are allowed.

   No_Duplicates_Allowed : constant array (Aspect_Id) of Boolean :=
     (Aspect_Annotate      => False,
      Aspect_GNAT_Annotate => False,
      Aspect_Test_Case     => False,
      others               => True);

   --  The following subtype defines aspects corresponding to library unit
   --  pragmas, these can only validly appear as aspects for library units,
   --  and result in a corresponding pragma being inserted immediately after
   --  the occurrence of the aspect.

   subtype Library_Unit_Aspects is
     Aspect_Id range Aspect_All_Calls_Remote .. Aspect_Shared_Passive;

   --  The following subtype defines aspects accepting an optional static
   --  boolean parameter indicating if the aspect should be active or
   --  cancelling. If the parameter is missing the effective value is True,
   --  enabling the aspect. If the parameter is present it must be a static
   --  expression of type Standard.Boolean. If the value is True, then the
   --  aspect is enabled. If it is False, the aspect is disabled.

   subtype Boolean_Aspects is
     Aspect_Id range Aspect_Asynchronous .. Aspect_Id'Last;

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
     (No_Aspect                         => Optional_Expression,
      Aspect_Abstract_State             => Expression,
      Aspect_Address                    => Expression,
      Aspect_Aggregate                  => Expression,
      Aspect_Alignment                  => Expression,
      Aspect_Always_Terminates          => Optional_Expression,
      Aspect_Annotate                   => Expression,
      Aspect_Async_Readers              => Optional_Expression,
      Aspect_Async_Writers              => Optional_Expression,
      Aspect_Attach_Handler             => Expression,
      Aspect_Bit_Order                  => Expression,
      Aspect_Component_Size             => Expression,
      Aspect_Constant_After_Elaboration => Optional_Expression,
      Aspect_Constant_Indexing          => Name,
      Aspect_Contract_Cases             => Expression,
      Aspect_Convention                 => Name,
      Aspect_CPU                        => Expression,
      Aspect_Default_Component_Value    => Expression,
      Aspect_Default_Initial_Condition  => Optional_Expression,
      Aspect_Default_Iterator           => Name,
      Aspect_Default_Storage_Pool       => Expression,
      Aspect_Default_Value              => Expression,
      Aspect_Depends                    => Expression,
      Aspect_Designated_Storage_Model   => Name,
      Aspect_Dimension                  => Expression,
      Aspect_Dimension_System           => Expression,
      Aspect_Dispatching_Domain         => Expression,
      Aspect_Dynamic_Predicate          => Expression,
      Aspect_Effective_Reads            => Optional_Expression,
      Aspect_Effective_Writes           => Optional_Expression,
      Aspect_Exceptional_Cases          => Expression,
      Aspect_Extensions_Visible         => Optional_Expression,
      Aspect_External_Name              => Expression,
      Aspect_External_Tag               => Expression,
      Aspect_Ghost                      => Optional_Expression,
      Aspect_Ghost_Predicate            => Expression,
      Aspect_Global                     => Expression,
      Aspect_GNAT_Annotate              => Expression,
      Aspect_Implicit_Dereference       => Name,
      Aspect_Initial_Condition          => Expression,
      Aspect_Initializes                => Expression,
      Aspect_Input                      => Name,
      Aspect_Integer_Literal            => Name,
      Aspect_Interrupt_Priority         => Expression,
      Aspect_Invariant                  => Expression,
      Aspect_Iterable                   => Expression,
      Aspect_Iterator_Element           => Name,
      Aspect_Link_Name                  => Expression,
      Aspect_Linker_Section             => Expression,
      Aspect_Local_Restrictions         => Expression,
      Aspect_Machine_Radix              => Expression,
      Aspect_Max_Entry_Queue_Depth      => Expression,
      Aspect_Max_Entry_Queue_Length     => Expression,
      Aspect_Max_Queue_Length           => Expression,
      Aspect_No_Caching                 => Optional_Expression,
      Aspect_No_Controlled_Parts        => Optional_Expression,
      Aspect_No_Task_Parts              => Optional_Expression,
      Aspect_Object_Size                => Expression,
      Aspect_Obsolescent                => Optional_Expression,
      Aspect_Output                     => Name,
      Aspect_Part_Of                    => Expression,
      Aspect_Post                       => Expression,
      Aspect_Postcondition              => Expression,
      Aspect_Pre                        => Expression,
      Aspect_Precondition               => Expression,
      Aspect_Predicate                  => Expression,
      Aspect_Predicate_Failure          => Expression,
      Aspect_Priority                   => Expression,
      Aspect_Put_Image                  => Name,
      Aspect_Read                       => Name,
      Aspect_Real_Literal               => Name,
      Aspect_Refined_Depends            => Expression,
      Aspect_Refined_Global             => Expression,
      Aspect_Refined_Post               => Expression,
      Aspect_Refined_State              => Expression,
      Aspect_Relative_Deadline          => Expression,
      Aspect_Relaxed_Initialization     => Optional_Expression,
      Aspect_Scalar_Storage_Order       => Expression,
      Aspect_Secondary_Stack_Size       => Expression,
      Aspect_Side_Effects               => Optional_Expression,
      Aspect_Simple_Storage_Pool        => Name,
      Aspect_Size                       => Expression,
      Aspect_Small                      => Expression,
      Aspect_SPARK_Mode                 => Optional_Name,
      Aspect_Stable_Properties          => Expression,
      Aspect_Static_Predicate           => Expression,
      Aspect_Storage_Model_Type         => Optional_Expression,
      Aspect_Storage_Pool               => Name,
      Aspect_Storage_Size               => Expression,
      Aspect_Stream_Size                => Expression,
      Aspect_String_Literal             => Name,
      Aspect_Subprogram_Variant         => Expression,
      Aspect_Suppress                   => Name,
      Aspect_Synchronization            => Name,
      Aspect_Test_Case                  => Expression,
      Aspect_Type_Invariant             => Expression,
      Aspect_Unimplemented              => Optional_Expression,
      Aspect_Unsuppress                 => Name,
      Aspect_User_Aspect                => Expression,
      Aspect_Value_Size                 => Expression,
      Aspect_Variable_Indexing          => Name,
      Aspect_Volatile_Function          => Optional_Expression,
      Aspect_Warnings                   => Name,
      Aspect_Write                      => Name,

      Boolean_Aspects                   => Optional_Expression,
      Library_Unit_Aspects              => Optional_Expression);

   --  The following array indicates what aspects are representation aspects

   Is_Representation_Aspect : constant array (Aspect_Id) of Boolean :=
     (No_Aspect                           => False,
      Aspect_Abstract_State               => False,
      Aspect_Address                      => True,
      Aspect_Aggregate                    => False,
      Aspect_Alignment                    => True,
      Aspect_Always_Terminates            => False,
      Aspect_Annotate                     => False,
      Aspect_Async_Readers                => False,
      Aspect_Async_Writers                => False,
      Aspect_Attach_Handler               => False,
      Aspect_Bit_Order                    => True,
      Aspect_Component_Size               => True,
      Aspect_Constant_After_Elaboration   => False,
      Aspect_Constant_Indexing            => False,
      Aspect_Contract_Cases               => False,
      Aspect_Convention                   => True,
      Aspect_CPU                          => False,
      Aspect_CUDA_Device                  => False,
      Aspect_CUDA_Global                  => False,
      Aspect_Default_Component_Value      => True,
      Aspect_Default_Initial_Condition    => False,
      Aspect_Default_Iterator             => False,
      Aspect_Default_Storage_Pool         => True,
      Aspect_Default_Value                => True,
      Aspect_Depends                      => False,
      Aspect_Designated_Storage_Model     => True,
      Aspect_Dimension                    => False,
      Aspect_Dimension_System             => False,
      Aspect_Dispatching_Domain           => False,
      Aspect_Dynamic_Predicate            => False,
      Aspect_Effective_Reads              => False,
      Aspect_Effective_Writes             => False,
      Aspect_Exceptional_Cases            => False,
      Aspect_Exclusive_Functions          => False,
      Aspect_Extensions_Visible           => False,
      Aspect_External_Name                => False,
      Aspect_External_Tag                 => False,
      Aspect_Ghost                        => False,
      Aspect_Ghost_Predicate              => False,
      Aspect_Global                       => False,
      Aspect_GNAT_Annotate                => False,
      Aspect_Implicit_Dereference         => False,
      Aspect_Initial_Condition            => False,
      Aspect_Initializes                  => False,
      Aspect_Input                        => False,
      Aspect_Integer_Literal              => False,
      Aspect_Interrupt_Priority           => False,
      Aspect_Invariant                    => False,
      Aspect_Iterable                     => False,
      Aspect_Iterator_Element             => False,
      Aspect_Link_Name                    => True,
      Aspect_Linker_Section               => True,
      Aspect_Local_Restrictions           => False,
      Aspect_Machine_Radix                => True,
      Aspect_Max_Entry_Queue_Depth        => False,
      Aspect_Max_Entry_Queue_Length       => False,
      Aspect_Max_Queue_Length             => False,
      Aspect_No_Caching                   => False,
      Aspect_No_Controlled_Parts          => False,
      Aspect_No_Task_Parts                => False,
      Aspect_Object_Size                  => True,
      Aspect_Obsolescent                  => False,
      Aspect_Output                       => False,
      Aspect_Part_Of                      => False,
      Aspect_Post                         => False,
      Aspect_Postcondition                => False,
      Aspect_Pre                          => False,
      Aspect_Precondition                 => False,
      Aspect_Predicate                    => False,
      Aspect_Predicate_Failure            => False,
      Aspect_Priority                     => False,
      Aspect_Put_Image                    => False,
      Aspect_Read                         => False,
      Aspect_Real_Literal                 => False,
      Aspect_Refined_Depends              => False,
      Aspect_Refined_Global               => False,
      Aspect_Refined_Post                 => False,
      Aspect_Refined_State                => False,
      Aspect_Relative_Deadline            => False,
      Aspect_Relaxed_Initialization       => False,
      Aspect_Scalar_Storage_Order         => True,
      Aspect_Secondary_Stack_Size         => True,
      Aspect_Side_Effects                 => False,
      Aspect_Simple_Storage_Pool          => True,
      Aspect_Size                         => True,
      Aspect_Small                        => True,
      Aspect_SPARK_Mode                   => False,
      Aspect_Stable_Properties            => False,
      Aspect_Static_Predicate             => False,
      Aspect_Storage_Model_Type           => False,
      Aspect_Storage_Pool                 => True,
      Aspect_Storage_Size                 => True,
      Aspect_Stream_Size                  => True,
      Aspect_String_Literal               => False,
      Aspect_Subprogram_Variant           => False,
      Aspect_Suppress                     => False,
      Aspect_Synchronization              => False,
      Aspect_Test_Case                    => False,
      Aspect_Type_Invariant               => False,
      Aspect_Unimplemented                => False,
      Aspect_Unsuppress                   => False,
      Aspect_User_Aspect                  => False,
      Aspect_Value_Size                   => True,
      Aspect_Variable_Indexing            => False,
      Aspect_Volatile_Function            => False,
      Aspect_Warnings                     => False,
      Aspect_Write                        => False,

      Library_Unit_Aspects                => False,

      Aspect_Asynchronous                 => True,
      Aspect_Atomic                       => True,
      Aspect_Atomic_Components            => True,
      Aspect_Disable_Controlled           => False,
      Aspect_Discard_Names                => True,
      Aspect_Export                       => True,
      Aspect_Favor_Top_Level              => False,
      Aspect_Full_Access_Only             => True,
      Aspect_Independent                  => True,
      Aspect_Independent_Components       => True,
      Aspect_Import                       => True,
      Aspect_Inline                       => False,
      Aspect_Inline_Always                => False,
      Aspect_Interrupt_Handler            => False,
      Aspect_Lock_Free                    => False,
      Aspect_No_Inline                    => False,
      Aspect_No_Return                    => False,
      Aspect_No_Tagged_Streams            => False,
      Aspect_Pack                         => True,
      Aspect_Persistent_BSS               => True,
      Aspect_Preelaborable_Initialization => False,
      Aspect_Pure_Function                => False,
      Aspect_Remote_Access_Type           => False,
      Aspect_Shared                       => True,
      Aspect_Simple_Storage_Pool_Type     => True,
      Aspect_Static                       => False,
      Aspect_Suppress_Debug_Info          => False,
      Aspect_Suppress_Initialization      => False,
      Aspect_Thread_Local_Storage         => True,
      Aspect_Unchecked_Union              => True,
      Aspect_Universal_Aliasing           => False,
      Aspect_Unmodified                   => False,
      Aspect_Unreferenced                 => False,
      Aspect_Unreferenced_Objects         => False,
      Aspect_Volatile                     => True,
      Aspect_Volatile_Components          => True,
      Aspect_Volatile_Full_Access         => True,
      Aspect_Yield                        => False);

   -----------------------------------------
   -- Table Linking Names and Aspect_Id's --
   -----------------------------------------

   --  Table linking aspect names and id's

   Aspect_Names : constant array (Aspect_Id) of Name_Id :=
     (No_Aspect                           => No_Name,
      Aspect_Abstract_State               => Name_Abstract_State,
      Aspect_Address                      => Name_Address,
      Aspect_Aggregate                    => Name_Aggregate,
      Aspect_Alignment                    => Name_Alignment,
      Aspect_All_Calls_Remote             => Name_All_Calls_Remote,
      Aspect_Always_Terminates            => Name_Always_Terminates,
      Aspect_Annotate                     => Name_Annotate,
      Aspect_Async_Readers                => Name_Async_Readers,
      Aspect_Async_Writers                => Name_Async_Writers,
      Aspect_Asynchronous                 => Name_Asynchronous,
      Aspect_Atomic                       => Name_Atomic,
      Aspect_Atomic_Components            => Name_Atomic_Components,
      Aspect_Attach_Handler               => Name_Attach_Handler,
      Aspect_Bit_Order                    => Name_Bit_Order,
      Aspect_Component_Size               => Name_Component_Size,
      Aspect_Constant_After_Elaboration   => Name_Constant_After_Elaboration,
      Aspect_Constant_Indexing            => Name_Constant_Indexing,
      Aspect_Contract_Cases               => Name_Contract_Cases,
      Aspect_Convention                   => Name_Convention,
      Aspect_CPU                          => Name_CPU,
      Aspect_CUDA_Device                  => Name_CUDA_Device,
      Aspect_CUDA_Global                  => Name_CUDA_Global,
      Aspect_Default_Component_Value      => Name_Default_Component_Value,
      Aspect_Default_Initial_Condition    => Name_Default_Initial_Condition,
      Aspect_Default_Iterator             => Name_Default_Iterator,
      Aspect_Default_Storage_Pool         => Name_Default_Storage_Pool,
      Aspect_Default_Value                => Name_Default_Value,
      Aspect_Depends                      => Name_Depends,
      Aspect_Designated_Storage_Model     => Name_Designated_Storage_Model,
      Aspect_Dimension                    => Name_Dimension,
      Aspect_Dimension_System             => Name_Dimension_System,
      Aspect_Disable_Controlled           => Name_Disable_Controlled,
      Aspect_Discard_Names                => Name_Discard_Names,
      Aspect_Dispatching_Domain           => Name_Dispatching_Domain,
      Aspect_Dynamic_Predicate            => Name_Dynamic_Predicate,
      Aspect_Effective_Reads              => Name_Effective_Reads,
      Aspect_Effective_Writes             => Name_Effective_Writes,
      Aspect_Elaborate_Body               => Name_Elaborate_Body,
      Aspect_Exceptional_Cases            => Name_Exceptional_Cases,
      Aspect_Exclusive_Functions          => Name_Exclusive_Functions,
      Aspect_Export                       => Name_Export,
      Aspect_Extensions_Visible           => Name_Extensions_Visible,
      Aspect_External_Name                => Name_External_Name,
      Aspect_External_Tag                 => Name_External_Tag,
      Aspect_Favor_Top_Level              => Name_Favor_Top_Level,
      Aspect_Full_Access_Only             => Name_Full_Access_Only,
      Aspect_Ghost                        => Name_Ghost,
      Aspect_Ghost_Predicate              => Name_Ghost_Predicate,
      Aspect_Global                       => Name_Global,
      Aspect_GNAT_Annotate                => Name_GNAT_Annotate,
      Aspect_Implicit_Dereference         => Name_Implicit_Dereference,
      Aspect_Import                       => Name_Import,
      Aspect_Independent                  => Name_Independent,
      Aspect_Independent_Components       => Name_Independent_Components,
      Aspect_Inline                       => Name_Inline,
      Aspect_Inline_Always                => Name_Inline_Always,
      Aspect_Initial_Condition            => Name_Initial_Condition,
      Aspect_Initializes                  => Name_Initializes,
      Aspect_Input                        => Name_Input,
      Aspect_Integer_Literal              => Name_Integer_Literal,
      Aspect_Interrupt_Handler            => Name_Interrupt_Handler,
      Aspect_Interrupt_Priority           => Name_Interrupt_Priority,
      Aspect_Invariant                    => Name_Invariant,
      Aspect_Iterator_Element             => Name_Iterator_Element,
      Aspect_Iterable                     => Name_Iterable,
      Aspect_Link_Name                    => Name_Link_Name,
      Aspect_Linker_Section               => Name_Linker_Section,
      Aspect_Lock_Free                    => Name_Lock_Free,
      Aspect_Local_Restrictions           => Name_Local_Restrictions,
      Aspect_Machine_Radix                => Name_Machine_Radix,
      Aspect_Max_Entry_Queue_Depth        => Name_Max_Entry_Queue_Depth,
      Aspect_Max_Entry_Queue_Length       => Name_Max_Entry_Queue_Length,
      Aspect_Max_Queue_Length             => Name_Max_Queue_Length,
      Aspect_No_Caching                   => Name_No_Caching,
      Aspect_No_Controlled_Parts          => Name_No_Controlled_Parts,
      Aspect_No_Task_Parts                => Name_No_Task_Parts,
      Aspect_No_Elaboration_Code_All      => Name_No_Elaboration_Code_All,
      Aspect_No_Inline                    => Name_No_Inline,
      Aspect_No_Return                    => Name_No_Return,
      Aspect_No_Tagged_Streams            => Name_No_Tagged_Streams,
      Aspect_Object_Size                  => Name_Object_Size,
      Aspect_Obsolescent                  => Name_Obsolescent,
      Aspect_Output                       => Name_Output,
      Aspect_Pack                         => Name_Pack,
      Aspect_Part_Of                      => Name_Part_Of,
      Aspect_Persistent_BSS               => Name_Persistent_BSS,
      Aspect_Post                         => Name_Post,
      Aspect_Postcondition                => Name_Postcondition,
      Aspect_Pre                          => Name_Pre,
      Aspect_Precondition                 => Name_Precondition,
      Aspect_Predicate                    => Name_Predicate,
      Aspect_Predicate_Failure            => Name_Predicate_Failure,
      Aspect_Preelaborable_Initialization => Name_Preelaborable_Initialization,
      Aspect_Preelaborate                 => Name_Preelaborate,
      Aspect_Priority                     => Name_Priority,
      Aspect_Pure                         => Name_Pure,
      Aspect_Pure_Function                => Name_Pure_Function,
      Aspect_Put_Image                    => Name_Put_Image,
      Aspect_Read                         => Name_Read,
      Aspect_Real_Literal                 => Name_Real_Literal,
      Aspect_Refined_Depends              => Name_Refined_Depends,
      Aspect_Refined_Global               => Name_Refined_Global,
      Aspect_Refined_Post                 => Name_Refined_Post,
      Aspect_Refined_State                => Name_Refined_State,
      Aspect_Relative_Deadline            => Name_Relative_Deadline,
      Aspect_Relaxed_Initialization       => Name_Relaxed_Initialization,
      Aspect_Remote_Access_Type           => Name_Remote_Access_Type,
      Aspect_Remote_Call_Interface        => Name_Remote_Call_Interface,
      Aspect_Remote_Types                 => Name_Remote_Types,
      Aspect_Scalar_Storage_Order         => Name_Scalar_Storage_Order,
      Aspect_Secondary_Stack_Size         => Name_Secondary_Stack_Size,
      Aspect_Shared                       => Name_Shared,
      Aspect_Shared_Passive               => Name_Shared_Passive,
      Aspect_Side_Effects                 => Name_Side_Effects,
      Aspect_Simple_Storage_Pool          => Name_Simple_Storage_Pool,
      Aspect_Simple_Storage_Pool_Type     => Name_Simple_Storage_Pool_Type,
      Aspect_Size                         => Name_Size,
      Aspect_Small                        => Name_Small,
      Aspect_SPARK_Mode                   => Name_SPARK_Mode,
      Aspect_Stable_Properties            => Name_Stable_Properties,
      Aspect_Static                       => Name_Static,
      Aspect_Static_Predicate             => Name_Static_Predicate,
      Aspect_Storage_Model_Type           => Name_Storage_Model_Type,
      Aspect_Storage_Pool                 => Name_Storage_Pool,
      Aspect_Storage_Size                 => Name_Storage_Size,
      Aspect_Stream_Size                  => Name_Stream_Size,
      Aspect_String_Literal               => Name_String_Literal,
      Aspect_Subprogram_Variant           => Name_Subprogram_Variant,
      Aspect_Suppress                     => Name_Suppress,
      Aspect_Suppress_Debug_Info          => Name_Suppress_Debug_Info,
      Aspect_Suppress_Initialization      => Name_Suppress_Initialization,
      Aspect_Thread_Local_Storage         => Name_Thread_Local_Storage,
      Aspect_Synchronization              => Name_Synchronization,
      Aspect_Test_Case                    => Name_Test_Case,
      Aspect_Type_Invariant               => Name_Type_Invariant,
      Aspect_Unchecked_Union              => Name_Unchecked_Union,
      Aspect_Unimplemented                => Name_Unimplemented,
      Aspect_Universal_Aliasing           => Name_Universal_Aliasing,
      Aspect_Unmodified                   => Name_Unmodified,
      Aspect_Unreferenced                 => Name_Unreferenced,
      Aspect_Unreferenced_Objects         => Name_Unreferenced_Objects,
      Aspect_Unsuppress                   => Name_Unsuppress,
      Aspect_User_Aspect                  => Name_User_Aspect,
      Aspect_Value_Size                   => Name_Value_Size,
      Aspect_Variable_Indexing            => Name_Variable_Indexing,
      Aspect_Volatile                     => Name_Volatile,
      Aspect_Volatile_Components          => Name_Volatile_Components,
      Aspect_Volatile_Full_Access         => Name_Volatile_Full_Access,
      Aspect_Volatile_Function            => Name_Volatile_Function,
      Aspect_Warnings                     => Name_Warnings,
      Aspect_Write                        => Name_Write,
      Aspect_Yield                        => Name_Yield);

   function Get_Aspect_Id (Name : Name_Id) return Aspect_Id;
   pragma Inline (Get_Aspect_Id);
   --  Given a name Nam, returns the corresponding aspect id value. If the name
   --  does not match any aspect, then No_Aspect is returned as the result.

   function Get_Aspect_Id (Aspect : Node_Id) return Aspect_Id;
   pragma Inline (Get_Aspect_Id);
   --  Given an aspect specification, return the corresponding aspect_id value.
   --  If the name does not match any aspect, return No_Aspect.

   function Is_Aspect_Id (Aspect : Name_Id) return Boolean;
   pragma Inline (Is_Aspect_Id);
   --  Return True if a corresponding aspect id exists

   function Is_Aspect_Id (Aspect : Node_Id) return Boolean;
   pragma Inline (Is_Aspect_Id);
   --  Return True if a corresponding aspect id exists

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
   --  second flag is set on a derived type: May_Inherit_Delayed_Rep_Aspects
   --  is set if the parent type has Has_Delayed_Rep_Aspects set.

   --  When we freeze a derived type, if the May_Inherit_Delayed_Rep_Aspects
   --  flag is set, then we call Sem_Ch13.Inherit_Delayed_Rep_Aspects when
   --  the derived type is frozen, which deals with the necessary copying of
   --  information from the parent type, which must be frozen at that point
   --  (since freezing the derived type first freezes the parent type).

   --  SPARK 2014 aspects do not follow the general delay mechanism as they
   --  act as annotations and cannot modify the attributes of their related
   --  constructs. To handle forward references in such aspects, the compiler
   --  delays the analysis of their respective pragmas by collecting them in
   --  N_Contract nodes. The pragmas are then analyzed at the end of the
   --  declarative region containing the related construct. For details,
   --  see routines Analyze_xxx_In_Decl_Part.

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
      --  delays (and for our own aspects we can do what we like).

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
      Aspect_Aggregate                    => Always_Delay,
      Aspect_All_Calls_Remote             => Always_Delay,
      Aspect_Asynchronous                 => Always_Delay,
      Aspect_Attach_Handler               => Always_Delay,
      Aspect_Constant_Indexing            => Always_Delay,
      Aspect_CPU                          => Always_Delay,
      Aspect_CUDA_Device                  => Always_Delay,
      Aspect_CUDA_Global                  => Always_Delay,
      Aspect_Default_Iterator             => Always_Delay,
      Aspect_Default_Storage_Pool         => Always_Delay,
      Aspect_Default_Value                => Always_Delay,
      Aspect_Default_Component_Value      => Always_Delay,
      Aspect_Designated_Storage_Model     => Always_Delay,
      Aspect_Discard_Names                => Always_Delay,
      Aspect_Dispatching_Domain           => Always_Delay,
      Aspect_Dynamic_Predicate            => Always_Delay,
      Aspect_Elaborate_Body               => Always_Delay,
      Aspect_Exclusive_Functions          => Always_Delay,
      Aspect_External_Name                => Always_Delay,
      Aspect_External_Tag                 => Always_Delay,
      Aspect_Favor_Top_Level              => Always_Delay,
      Aspect_Ghost_Predicate              => Always_Delay,
      Aspect_Implicit_Dereference         => Always_Delay,
      Aspect_Independent                  => Always_Delay,
      Aspect_Independent_Components       => Always_Delay,
      Aspect_Inline                       => Always_Delay,
      Aspect_Inline_Always                => Always_Delay,
      Aspect_Input                        => Always_Delay,
      Aspect_Integer_Literal              => Always_Delay,
      Aspect_Interrupt_Handler            => Always_Delay,
      Aspect_Interrupt_Priority           => Always_Delay,
      Aspect_Invariant                    => Always_Delay,
      Aspect_Iterable                     => Always_Delay,
      Aspect_Iterator_Element             => Always_Delay,
      Aspect_Link_Name                    => Always_Delay,
      Aspect_Linker_Section               => Always_Delay,
      Aspect_Lock_Free                    => Always_Delay,
      Aspect_No_Inline                    => Always_Delay,
      Aspect_No_Return                    => Always_Delay,
      Aspect_Output                       => Always_Delay,
      Aspect_Persistent_BSS               => Always_Delay,
      Aspect_Post                         => Always_Delay,
      Aspect_Postcondition                => Always_Delay,
      Aspect_Pre                          => Always_Delay,
      Aspect_Precondition                 => Always_Delay,
      Aspect_Predicate                    => Always_Delay,
      Aspect_Predicate_Failure            => Always_Delay,
      Aspect_Preelaborable_Initialization => Always_Delay,
      Aspect_Preelaborate                 => Always_Delay,
      Aspect_Priority                     => Always_Delay,
      Aspect_Pure                         => Always_Delay,
      Aspect_Pure_Function                => Always_Delay,
      Aspect_Put_Image                    => Always_Delay,
      Aspect_Read                         => Always_Delay,
      Aspect_Real_Literal                 => Always_Delay,
      Aspect_Relative_Deadline            => Always_Delay,
      Aspect_Remote_Access_Type           => Always_Delay,
      Aspect_Remote_Call_Interface        => Always_Delay,
      Aspect_Remote_Types                 => Always_Delay,
      Aspect_Secondary_Stack_Size         => Always_Delay,
      Aspect_Shared                       => Always_Delay,
      Aspect_Shared_Passive               => Always_Delay,
      Aspect_Simple_Storage_Pool          => Always_Delay,
      Aspect_Simple_Storage_Pool_Type     => Always_Delay,
      Aspect_Static_Predicate             => Always_Delay,
      Aspect_Storage_Model_Type           => Always_Delay,
      Aspect_Storage_Pool                 => Always_Delay,
      Aspect_Stream_Size                  => Always_Delay,
      Aspect_String_Literal               => Always_Delay,
      Aspect_Suppress                     => Always_Delay,
      Aspect_Suppress_Debug_Info          => Always_Delay,
      Aspect_Suppress_Initialization      => Always_Delay,
      Aspect_Thread_Local_Storage         => Always_Delay,
      Aspect_Type_Invariant               => Always_Delay,
      Aspect_Unchecked_Union              => Always_Delay,
      Aspect_Universal_Aliasing           => Always_Delay,
      Aspect_Unmodified                   => Always_Delay,
      Aspect_Unreferenced                 => Always_Delay,
      Aspect_Unreferenced_Objects         => Always_Delay,
      Aspect_Unsuppress                   => Always_Delay,
      Aspect_Variable_Indexing            => Always_Delay,
      Aspect_Write                        => Always_Delay,

      Aspect_Abstract_State               => Never_Delay,
      Aspect_Always_Terminates            => Never_Delay,
      Aspect_Annotate                     => Never_Delay,
      Aspect_Async_Readers                => Never_Delay,
      Aspect_Async_Writers                => Never_Delay,
      Aspect_Constant_After_Elaboration   => Never_Delay,
      Aspect_Contract_Cases               => Never_Delay,
      Aspect_Convention                   => Never_Delay,
      Aspect_Default_Initial_Condition    => Never_Delay,
      Aspect_Depends                      => Never_Delay,
      Aspect_Dimension                    => Never_Delay,
      Aspect_Dimension_System             => Never_Delay,
      Aspect_Disable_Controlled           => Never_Delay,
      Aspect_Effective_Reads              => Never_Delay,
      Aspect_Effective_Writes             => Never_Delay,
      Aspect_Exceptional_Cases            => Never_Delay,
      Aspect_Export                       => Never_Delay,
      Aspect_Extensions_Visible           => Never_Delay,
      Aspect_Ghost                        => Never_Delay,
      Aspect_Global                       => Never_Delay,
      Aspect_GNAT_Annotate                => Never_Delay,
      Aspect_Import                       => Never_Delay,
      Aspect_Initial_Condition            => Never_Delay,
      Aspect_Local_Restrictions           => Never_Delay,
      Aspect_Initializes                  => Never_Delay,
      Aspect_Max_Entry_Queue_Depth        => Never_Delay,
      Aspect_Max_Entry_Queue_Length       => Never_Delay,
      Aspect_Max_Queue_Length             => Never_Delay,
      Aspect_No_Caching                   => Never_Delay,
      Aspect_No_Controlled_Parts          => Never_Delay,
      Aspect_No_Task_Parts                => Never_Delay,
      Aspect_No_Elaboration_Code_All      => Never_Delay,
      Aspect_No_Tagged_Streams            => Never_Delay,
      Aspect_Obsolescent                  => Never_Delay,
      Aspect_Part_Of                      => Never_Delay,
      Aspect_Refined_Depends              => Never_Delay,
      Aspect_Refined_Global               => Never_Delay,
      Aspect_Refined_Post                 => Never_Delay,
      Aspect_Refined_State                => Never_Delay,
      Aspect_Relaxed_Initialization       => Never_Delay,
      Aspect_Side_Effects                 => Never_Delay,
      Aspect_SPARK_Mode                   => Never_Delay,
      Aspect_Stable_Properties            => Always_Delay,
      Aspect_Static                       => Never_Delay,
      Aspect_Subprogram_Variant           => Never_Delay,
      Aspect_Synchronization              => Never_Delay,
      Aspect_Test_Case                    => Never_Delay,
      Aspect_Unimplemented                => Never_Delay,
      Aspect_User_Aspect                  => Never_Delay,
      Aspect_Volatile_Function            => Never_Delay,
      Aspect_Warnings                     => Never_Delay,
      Aspect_Yield                        => Never_Delay,

      Aspect_Alignment                    => Rep_Aspect,
      Aspect_Atomic                       => Rep_Aspect,
      Aspect_Atomic_Components            => Rep_Aspect,
      Aspect_Bit_Order                    => Rep_Aspect,
      Aspect_Component_Size               => Rep_Aspect,
      Aspect_Full_Access_Only             => Rep_Aspect,
      Aspect_Machine_Radix                => Rep_Aspect,
      Aspect_Object_Size                  => Rep_Aspect,
      Aspect_Pack                         => Rep_Aspect,
      Aspect_Scalar_Storage_Order         => Rep_Aspect,
      Aspect_Size                         => Rep_Aspect,
      Aspect_Small                        => Rep_Aspect,
      Aspect_Storage_Size                 => Rep_Aspect,
      Aspect_Value_Size                   => Rep_Aspect,
      Aspect_Volatile                     => Rep_Aspect,
      Aspect_Volatile_Components          => Rep_Aspect,
      Aspect_Volatile_Full_Access         => Rep_Aspect);

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
   --  Sem_Prag.

   Aspect_On_Body_Or_Stub_OK : constant array (Aspect_Id) of Boolean :=
     (Aspect_Refined_Depends              => True,
      Aspect_Refined_Global               => True,
      Aspect_Refined_Post                 => True,
      Aspect_SPARK_Mode                   => True,
      Aspect_Warnings                     => True,
      others                              => False);

   -------------------------------------------------------------------
   -- Handling of Aspects Specifications on Single Concurrent Types --
   -------------------------------------------------------------------

   --  Certain aspects that appear on the following nodes

   --    N_Single_Protected_Declaration
   --    N_Single_Task_Declaration

   --  are treated as if they apply to the anonymous object produced by the
   --  analysis of a single concurrent type. The following table lists all
   --  aspects that should apply to the anonymous object. The table should
   --  be synchronized with Pragma_On_Anonymous_Object_OK in unit Sem_Prag.

   Aspect_On_Anonymous_Object_OK : constant array (Aspect_Id) of Boolean :=
     (Aspect_Depends                      => True,
      Aspect_Global                       => True,
      Aspect_Part_Of                      => True,
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

   function Aspects_On_Body_Or_Stub_OK (N : Node_Id) return Boolean;
   --  N denotes a body [stub] with aspects. Determine whether all aspects of N
   --  are allowed to appear on a body [stub].

   procedure Copy_Aspects (From : Node_Id; To : Node_Id);
   --  Create a copy of Aspect of From and add them to To.

   function Find_Aspect (Id            : Entity_Id;
                         A             : Aspect_Id;
                         Class_Present : Boolean := False;
                         Or_Rep_Item   : Boolean := False) return Node_Id;
   --  Find the aspect specification of aspect A (or A'Class if Class_Present)
   --  associated with entity I.
   --  If found, then return the aspect specification.
   --  If not found and Or_Rep_Item is true, then look for a representation
   --  item (as opposed to an N_Aspect_Specification node) which specifies
   --  the given aspect; if found, then return the representation item.
   --  [Currently only N_Attribute_Definition_Clause representation items
   --  are checked for, but support for detecting N_Pragma representation
   --  items could easily be added in the future if there is a need.]
   --  Otherwise, return Empty.

   function Find_Value_Of_Aspect
     (Id            : Entity_Id;
      A             : Aspect_Id;
      Class_Present : Boolean := False) return Node_Id;
   --  Find the value of aspect A (or A'Class, if Class_Present) associated
   --  with entity Id. Return Empty if Id does not have the requested aspect.

   function Has_Aspect (Id            : Entity_Id;
                        A             : Aspect_Id;
                        Class_Present : Boolean := False) return Boolean;
   --  Determine whether entity Id has aspect A (or A'Class, if Class_Present)

   function Has_Aspects (N : Node_Id) return Boolean;
   --  Returns whether the node has any aspect specifications

   procedure Move_Aspects (From : Node_Id; To : Node_Id);
   --  Relocate the aspect specifications of node From to node To. On entry it
   --  is assumed that To does not have aspect specifications. If From has no
   --  aspects, the routine has no effect.

   procedure Move_Or_Merge_Aspects (From : Node_Id; To : Node_Id);
   --  Relocate the aspect specifications of node From to node To. If To has
   --  aspects, the aspects of From are appended to the aspects of To. If From
   --  has no aspects, the routine has no effect. Special behavior:
   --    * When node From denotes a subprogram body stub without a previous
   --      declaration, the only aspects relocated to node To are those found
   --      in table Aspect_On_Body_Or_Stub_OK.
   --    * When node From denotes a single synchronized type declaration, the
   --      only aspects relocated to node To are those found in table
   --      Aspect_On_Anonymous_Object_OK.

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

   package User_Aspect_Support is
      procedure Register_UAD_Pragma (UAD_Pragma : Node_Id);
      --  Argument is a User_Aspect_Definition pragma.

      function Registered_UAD_Pragma (Aspect_Name : Name_Id) return Node_Id;
      --  Returns the registered pragma, if any, for the given name.
      --  Returns empty if no pragma with a matching first argument is
      --  in the map.

      --  In Find_Aspect we want to call
      --  Sem_Ch13.Analyze_User_Aspect_Specification, but doing this in the
      --  obvious way introduces problems (by pulling the bulk of semantics
      --  into the closure of package Aspects). So we declare an
      --  access-to-subp object here and call through it later if it happens
      --  to be non-null; it is initialized in the body of package Sem_Ch13.

      type Analyze_User_Aspect_Aspect_Specification_Ref is
        access procedure (N : Node_Id);

      Analyze_User_Aspect_Aspect_Specification_Hook :
        Analyze_User_Aspect_Aspect_Specification_Ref;
   end User_Aspect_Support;

end Aspects;
