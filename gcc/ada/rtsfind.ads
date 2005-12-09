------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              R T S F I N D                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

with Types; use Types;

package Rtsfind is

--  This package contains the routine that is used to obtain runtime library
--  entities, loading in the required runtime library packages on demand. It
--  is also used for such purposes as finding System.Address when System has
--  not been explicitly With'ed.

   ------------------------
   -- Runtime Unit Table --
   ------------------------

   --  The following type includes an enumeration entry for each runtime
   --  unit. The enumeration literal represents the fully qualified
   --  name of the unit, as follows:

   --    Names of the form Ada_xxx are first level children of Ada, whose
   --    name is Ada.xxx. For example, the name Ada_Tags refers to package
   --    Ada.Tags.

   --    Names of the form Ada_Calendar_xxx are second level children
   --    of Ada.Calendar. This is part of a temporary implementation of
   --    delays; eventually, packages implementing delays will be found
   --    relative to the package that declares the time type.

   --    Names of the form Ada_Finalization_xxx are second level children
   --    of Ada.Finalization.

   --    Names of the form Ada_Interrupts_xxx are second level children
   --    of Ada.Interrupts. This is needed for Ada.Interrupts.Names which
   --    is used by pragma Interrupt_State.

   --    Names of the form Ada_Real_Time_xxx are second level children
   --    of Ada.Real_Time.

   --    Names of the form Ada_Streams_xxx are second level children
   --    of Ada.Streams.

   --    Names of the form Ada_Text_IO_xxx are second level children
   --    of Ada.Text_IO.

   --    Names of the form Ada_Wide_Text_IO_xxx are second level children
   --    of Ada.Wide_Text_IO.

   --    Names of the form Ada_Wide_Wide_Text_IO_xxx are second level children
   --    of Ada.Wide_Wide_Text_IO.

   --    Names of the form Interfaces_xxx are first level children of
   --    Interfaces_CPP refers to package Interfaces.CPP

   --    Names of the form System_xxx are first level children of System, whose
   --    name is System.xxx. For example, the name System_Str_Concat refers to
   --    package System.Str_Concat.

   --    Names of the form System_Tasking_xxx are second level children of the
   --    package System.Tasking. For example, System_Tasking_Stages refers to
   --    refers to the package System.Tasking.Stages.

   --    Other names stand for themselves (e.g. System for package System)

   --  This list can contain both subprogram and package unit names. For
   --  packages, the accessible entities in the package are separately
   --  listed in the package entity table. The units must be either library
   --  level package declarations, or library level subprogram declarations.
   --  Generic units, library level instantiations and subprogram bodies
   --  acting as specs may not be referenced (all these cases could be added
   --  at the expense of additional complexity in the body of Rtsfind, but
   --  it doesn't seem worth while, since the implementation controls the
   --  set of units that are referenced, and this restrictions is easily met.

   --  IMPORTANT NOTE: the specs of packages and procedures with'ed using
   --  this mechanism may not contain use clauses. This is because these
   --  subprograms are compiled in the current visibility environment, and
   --  it would be too much trouble to establish a clean environment for the
   --  compilation. The presence of extraneous visible stuff has no effect
   --  on the compilation except in the presence of use clauses (which might
   --  result in unexpected ambiguities).

   type RTU_Id is (
      --  Runtime packages, for list of accessible entities in each
      --  package see declarations in the runtime entity table below.

      RTU_Null,
      --  Used as a null entry. Will cause an error if referenced

      --  Children of Ada

      Ada_Calendar,
      Ada_Exceptions,
      Ada_Finalization,
      Ada_Interrupts,
      Ada_Real_Time,
      Ada_Streams,
      Ada_Tags,
      Ada_Task_Identification,
      Ada_Task_Termination,

      --  Children of Ada.Calendar

      Ada_Calendar_Delays,

      --  Children of Ada.Finalization

      Ada_Finalization_List_Controller,

      --  Children of Ada.Interrupts

      Ada_Interrupts_Names,

      --  Children of Ada.Real_Time

      Ada_Real_Time_Delays,

      --  Children of Ada.Streams

      Ada_Streams_Stream_IO,

      --  Children of Ada.Text_IO (for Text_IO_Kludge)

      Ada_Text_IO_Decimal_IO,
      Ada_Text_IO_Enumeration_IO,
      Ada_Text_IO_Fixed_IO,
      Ada_Text_IO_Float_IO,
      Ada_Text_IO_Integer_IO,
      Ada_Text_IO_Modular_IO,

      --  Children of Ada.Wide_Text_IO (for Text_IO_Kludge)

      Ada_Wide_Text_IO_Decimal_IO,
      Ada_Wide_Text_IO_Enumeration_IO,
      Ada_Wide_Text_IO_Fixed_IO,
      Ada_Wide_Text_IO_Float_IO,
      Ada_Wide_Text_IO_Integer_IO,
      Ada_Wide_Text_IO_Modular_IO,

      --  Children of Ada.Wide_Wide_Text_IO (for Text_IO_Kludge)

      Ada_Wide_Wide_Text_IO_Decimal_IO,
      Ada_Wide_Wide_Text_IO_Enumeration_IO,
      Ada_Wide_Wide_Text_IO_Fixed_IO,
      Ada_Wide_Wide_Text_IO_Float_IO,
      Ada_Wide_Wide_Text_IO_Integer_IO,
      Ada_Wide_Wide_Text_IO_Modular_IO,

      --  Interfaces

      Interfaces,

      --  Children of Interfaces

      Interfaces_CPP,
      Interfaces_Packed_Decimal,

      --  Package System

      System,

      --  Children of System

      System_Arith_64,
      System_AST_Handling,
      System_Assertions,
      System_Aux_DEC,
      System_Bit_Ops,
      System_Boolean_Array_Operations,
      System_Checked_Pools,
      System_Compare_Array_Signed_16,
      System_Compare_Array_Signed_32,
      System_Compare_Array_Signed_64,
      System_Compare_Array_Signed_8,
      System_Compare_Array_Unsigned_16,
      System_Compare_Array_Unsigned_32,
      System_Compare_Array_Unsigned_64,
      System_Compare_Array_Unsigned_8,
      System_Exception_Table,
      System_Exn_Int,
      System_Exn_LLF,
      System_Exn_LLI,
      System_Exp_Int,
      System_Exp_LInt,
      System_Exp_LLI,
      System_Exp_LLU,
      System_Exp_Mod,
      System_Exp_Uns,
      System_Fat_Flt,
      System_Fat_IEEE_Long_Float,
      System_Fat_IEEE_Short_Float,
      System_Fat_LFlt,
      System_Fat_LLF,
      System_Fat_SFlt,
      System_Fat_VAX_D_Float,
      System_Fat_VAX_F_Float,
      System_Fat_VAX_G_Float,
      System_Finalization_Implementation,
      System_Finalization_Root,
      System_Fore,
      System_Img_Bool,
      System_Img_Char,
      System_Img_Dec,
      System_Img_Enum,
      System_Img_Int,
      System_Img_LLD,
      System_Img_LLI,
      System_Img_LLU,
      System_Img_Name,
      System_Img_Real,
      System_Img_Uns,
      System_Img_WChar,
      System_Interrupts,
      System_Machine_Code,
      System_Mantissa,
      System_Memcop,
      System_Pack_03,
      System_Pack_05,
      System_Pack_06,
      System_Pack_07,
      System_Pack_09,
      System_Pack_10,
      System_Pack_11,
      System_Pack_12,
      System_Pack_13,
      System_Pack_14,
      System_Pack_15,
      System_Pack_17,
      System_Pack_18,
      System_Pack_19,
      System_Pack_20,
      System_Pack_21,
      System_Pack_22,
      System_Pack_23,
      System_Pack_24,
      System_Pack_25,
      System_Pack_26,
      System_Pack_27,
      System_Pack_28,
      System_Pack_29,
      System_Pack_30,
      System_Pack_31,
      System_Pack_33,
      System_Pack_34,
      System_Pack_35,
      System_Pack_36,
      System_Pack_37,
      System_Pack_38,
      System_Pack_39,
      System_Pack_40,
      System_Pack_41,
      System_Pack_42,
      System_Pack_43,
      System_Pack_44,
      System_Pack_45,
      System_Pack_46,
      System_Pack_47,
      System_Pack_48,
      System_Pack_49,
      System_Pack_50,
      System_Pack_51,
      System_Pack_52,
      System_Pack_53,
      System_Pack_54,
      System_Pack_55,
      System_Pack_56,
      System_Pack_57,
      System_Pack_58,
      System_Pack_59,
      System_Pack_60,
      System_Pack_61,
      System_Pack_62,
      System_Pack_63,
      System_Parameters,
      System_Partition_Interface,
      System_PolyORB_Interface,
      System_Pool_Global,
      System_Pool_Empty,
      System_Pool_Local,
      System_Pool_Size,
      System_RPC,
      System_Scalar_Values,
      System_Secondary_Stack,
      System_Shared_Storage,
      System_Soft_Links,
      System_Standard_Library,
      System_Storage_Elements,
      System_Storage_Pools,
      System_Stream_Attributes,
      System_String_Ops,
      System_String_Ops_Concat_3,
      System_String_Ops_Concat_4,
      System_String_Ops_Concat_5,
      System_Task_Info,
      System_Tasking,
      System_Threads,
      System_Unsigned_Types,
      System_Val_Bool,
      System_Val_Char,
      System_Val_Dec,
      System_Val_Enum,
      System_Val_Int,
      System_Val_LLD,
      System_Val_LLI,
      System_Val_LLU,
      System_Val_Name,
      System_Val_Real,
      System_Val_Uns,
      System_Val_WChar,
      System_Vax_Float_Operations,
      System_Version_Control,
      System_VMS_Exception_Table,
      System_WCh_StW,
      System_WCh_WtS,
      System_Wid_Bool,
      System_Wid_Char,
      System_Wid_Enum,
      System_Wid_LLI,
      System_Wid_LLU,
      System_Wid_Name,
      System_Wid_WChar,
      System_WWd_Char,
      System_WWd_Enum,
      System_WWd_Wchar,

      --  Children of System.Tasking

      System_Tasking_Async_Delays,
      System_Tasking_Async_Delays_Enqueue_Calendar,
      System_Tasking_Async_Delays_Enqueue_RT,
      System_Tasking_Protected_Objects,
      System_Tasking_Protected_Objects_Entries,
      System_Tasking_Protected_Objects_Operations,
      System_Tasking_Protected_Objects_Single_Entry,
      System_Tasking_Restricted_Stages,
      System_Tasking_Rendezvous,
      System_Tasking_Stages);

   subtype Ada_Child is RTU_Id
     range Ada_Calendar .. Ada_Wide_Wide_Text_IO_Modular_IO;
   --  Range of values for children or grand-children of Ada

   subtype Ada_Calendar_Child is Ada_Child
     range Ada_Calendar_Delays .. Ada_Calendar_Delays;
   --  Range of values for children of Ada.Calendar

   subtype Ada_Finalization_Child is Ada_Child range
     Ada_Finalization_List_Controller .. Ada_Finalization_List_Controller;
   --  Range of values for children of Ada.Finalization

   subtype Ada_Interrupts_Child is Ada_Child range
     Ada_Interrupts_Names .. Ada_Interrupts_Names;
   --  Range of values for children of Ada.Interrupts

   subtype Ada_Real_Time_Child is Ada_Child
     range Ada_Real_Time_Delays .. Ada_Real_Time_Delays;
   --  Range of values for children of Ada.Real_Time

   subtype Ada_Streams_Child is Ada_Child
     range Ada_Streams_Stream_IO .. Ada_Streams_Stream_IO;

   subtype Ada_Text_IO_Child is Ada_Child
     range Ada_Text_IO_Decimal_IO .. Ada_Text_IO_Modular_IO;
   --  Range of values for children of Ada.Text_IO

   subtype Ada_Wide_Text_IO_Child is Ada_Child
     range Ada_Wide_Text_IO_Decimal_IO .. Ada_Wide_Text_IO_Modular_IO;
   --  Range of values for children of Ada.Text_IO

   subtype Ada_Wide_Wide_Text_IO_Child is Ada_Child
     range Ada_Wide_Wide_Text_IO_Decimal_IO ..
           Ada_Wide_Wide_Text_IO_Modular_IO;

   subtype Interfaces_Child is RTU_Id
     range Interfaces_CPP .. Interfaces_Packed_Decimal;
   --  Range of values for children of Interfaces

   subtype System_Child is RTU_Id
     range System_Arith_64 .. System_Tasking_Stages;
   --  Range of values for children or grandchildren of System

   subtype System_Tasking_Child is System_Child
     range System_Tasking_Async_Delays .. System_Tasking_Stages;
   --  Range of values for children of System.Tasking

   subtype System_Tasking_Protected_Objects_Child is System_Tasking_Child
     range System_Tasking_Protected_Objects_Entries ..
       System_Tasking_Protected_Objects_Single_Entry;
   --  Range of values for children of System.Tasking.Protected_Objects

   subtype System_Tasking_Restricted_Child is System_Tasking_Child
     range System_Tasking_Restricted_Stages ..
       System_Tasking_Restricted_Stages;
   --  Range of values for children of System.Tasking.Restricted

   subtype System_Tasking_Async_Delays_Child is System_Tasking_Child
     range System_Tasking_Async_Delays_Enqueue_Calendar ..
       System_Tasking_Async_Delays_Enqueue_RT;
   --  Range of values for children of System.Tasking.Async_Delays

   --------------------------
   -- Runtime Entity Table --
   --------------------------

   --  This is the enumeration type used to define the argument passed to
   --  the RTE function. The name must exactly match the name of the entity
   --  involved, and in the case of a package entity, this name must uniquely
   --  imply the package containing the entity.

   --  As far as possible, we avoid duplicate names in runtime packages, so
   --  that the name RE_nnn uniquely identifies the entity nnn. In some cases,
   --  it is impossible to avoid such duplication because the names come from
   --  RM defined packages. In such cases, the name is of the form RO_XX_nnn
   --  where XX is two letters used to differentiate the multiple occurrences
   --  of the name xx, and nnn is the entity name.

   --  Note that not all entities in the units contained in the run-time unit
   --  table are included in the following table, only those that actually
   --  have to be referenced from generated code.

   --  Note on RE_Null. This value is used as a null entry where an RE_Id
   --  value is required syntactically, but no real entry is required or
   --  needed. Use of this value will cause a fatal error in an RTE call.

   type RE_Id is (

     RE_Null,

     RE_Exceptions_Available_In_HIE,     -- Ada.Exceptions
     RE_Code_Loc,                        -- Ada.Exceptions
     RE_Current_Target_Exception,        -- Ada.Exceptions (JGNAT use only)
     RE_Exception_Id,                    -- Ada.Exceptions
     RE_Exception_Information,           -- Ada.Exceptions
     RE_Exception_Message,               -- Ada.Exceptions
     RE_Exception_Name_Simple,           -- Ada.Exceptions
     RE_Exception_Occurrence,            -- Ada.Exceptions
     RE_Null_Id,                         -- Ada.Exceptions
     RE_Null_Occurrence,                 -- Ada.Exceptions
     RE_Poll,                            -- Ada.Exceptions
     RE_Raise_Exception,                 -- Ada.Exceptions
     RE_Raise_Exception_Always,          -- Ada.Exceptions
     RE_Reraise_Occurrence,              -- Ada.Exceptions
     RE_Reraise_Occurrence_Always,       -- Ada.Exceptions
     RE_Reraise_Occurrence_No_Defer,     -- Ada.Exceptions
     RE_Save_Occurrence,                 -- Ada.Exceptions

     RE_Simple_List_Controller,          -- Ada.Finalization.List_Controller
     RE_List_Controller,                 -- Ada.Finalization.List_Controller

     RE_Interrupt_ID,                    -- Ada.Interrupts
     RE_Is_Reserved,                     -- Ada.Interrupts
     RE_Is_Attached,                     -- Ada.Interrupts
     RE_Current_Handler,                 -- Ada.Interrupts
     RE_Attach_Handler,                  -- Ada.Interrupts
     RE_Exchange_Handler,                -- Ada.Interrupts
     RE_Detach_Handler,                  -- Ada.Interrupts
     RE_Reference,                       -- Ada.Interrupts

     RE_Names,                           -- Ada.Interupts.Names

     RE_Root_Stream_Type,                -- Ada.Streams
     RE_Stream_Element,                  -- Ada.Streams
     RE_Stream_Element_Count,            -- Ada.Streams
     RE_Stream_Element_Offset,           -- Ada.Streams
     RE_Stream_Element_Array,            -- Ada.Streams

     RE_Stream_Access,                   -- Ada.Streams.Stream_IO

     RE_Abstract_Interface,              -- Ada.Tags
     RE_Addr_Ptr,                        -- Ada.Tags
     RE_Address_Array,                   -- Ada.Tags
     RE_CW_Membership,                   -- Ada.Tags
     RE_Descendant_Tag,                  -- Ada.Tags
     RE_Displace,                        -- Ada.Tags
     RE_DT_Entry_Size,                   -- Ada.Tags
     RE_DT_Prologue_Size,                -- Ada.Tags
     RE_External_Tag,                    -- Ada.Tags
     RE_Get_Access_Level,                -- Ada.Tags
     RE_Get_Entry_Index,                 -- Ada.Tags
     RE_Get_External_Tag,                -- Ada.Tags
     RE_Get_Offset_Index,                -- Ada.Tags
     RE_Get_Prim_Op_Address,             -- Ada.Tags
     RE_Get_Prim_Op_Kind,                -- Ada.Tags
     RE_Get_RC_Offset,                   -- Ada.Tags
     RE_Get_Remotely_Callable,           -- Ada.Tags
     RE_Get_Tagged_Kind,                 -- Ada.Tags
     RE_Inherit_DT,                      -- Ada.Tags
     RE_Inherit_TSD,                     -- Ada.Tags
     RE_Interface_Data,                  -- Ada.Tags
     RE_Interface_Tag,                   -- Ada.Tags
     RE_Internal_Tag,                    -- Ada.Tags
     RE_Is_Descendant_At_Same_Level,     -- Ada.Tags
     RE_IW_Membership,                   -- Ada.Tags
     RE_Object_Specific_Data,            -- Ada.Tags
     RE_Offset_To_Top,                   -- Ada.Tags
     RE_POK_Function,                    -- Ada.Tags
     RE_POK_Procedure,                   -- Ada.Tags
     RE_POK_Protected_Entry,             -- Ada.Tags
     RE_POK_Protected_Function,          -- Ada.Tags
     RE_POK_Protected_Procedure,         -- Ada.Tags
     RE_POK_Task_Entry,                  -- Ada.Tags
     RE_POK_Task_Function,               -- Ada.Tags
     RE_POK_Task_Procedure,              -- Ada.Tags
     RE_Prim_Op_Kind,                    -- Ada.Tags
     RE_Primary_DT,                      -- Ada.Tags
     RE_Register_Interface_Tag,          -- Ada.Tags
     RE_Register_Tag,                    -- Ada.Tags
     RE_Secondary_DT,                    -- Ada.Tags
     RE_Select_Specific_Data,            -- Ada.Tags
     RE_Set_Access_Level,                -- Ada.Tags
     RE_Set_Entry_Index,                 -- Ada.Tags
     RE_Set_Expanded_Name,               -- Ada.Tags
     RE_Set_External_Tag,                -- Ada.Tags
     RE_Set_Interface_Table,             -- Ada.Tags
     RE_Set_Num_Prim_Ops,                -- Ada.Tags
     RE_Set_Offset_Index,                -- Ada.Tags
     RE_Set_Offset_To_Top,               -- Ada.Tags
     RE_Set_OSD,                         -- Ada.Tags
     RE_Set_Prim_Op_Address,             -- Ada.Tags
     RE_Set_Prim_Op_Kind,                -- Ada.Tags
     RE_Set_RC_Offset,                   -- Ada.Tags
     RE_Set_Remotely_Callable,           -- Ada.Tags
     RE_Set_SSD,                         -- Ada.Tags
     RE_Set_Tagged_Kind,                 -- Ada.Tags
     RE_Set_TSD,                         -- Ada.Tags
     RE_Tag,                             -- Ada.Tags
     RE_Tag_Error,                       -- Ada.Tags
     RE_Tagged_Kind,                     -- Ada.Tags
     RE_TSD_Entry_Size,                  -- Ada.Tags
     RE_TSD_Prologue_Size,               -- Ada.Tags
     RE_TK_Abstract_Limited_Tagged,      -- Ada.Tags
     RE_TK_Abstract_Tagged,              -- Ada.Tags
     RE_TK_Limited_Tagged,               -- Ada.Tags
     RE_TK_Protected,                    -- Ada.Tags
     RE_TK_Tagged,                       -- Ada.Tags
     RE_TK_Task,                         -- Ada.Tags
     RE_Valid_Signature,                 -- Ada.Tags

     RE_Abort_Task,                      -- Ada.Task_Identification
     RE_Current_Task,                    -- Ada.Task_Identification
     RO_AT_Task_Id,                      -- Ada.Task_Identification

     RO_CA_Time,                         -- Ada.Calendar

     RO_CA_Delay_For,                    -- Ada.Calendar.Delays
     RO_CA_Delay_Until,                  -- Ada.Calendar.Delays
     RO_CA_To_Duration,                  -- Ada.Calendar.Delays

     RO_RT_Time,                         -- Ada.Real_Time

     RO_RT_Delay_Until,                  -- Ada.Real_Time.Delays
     RO_RT_To_Duration,                  -- Ada.Real_Time.Delays

     RE_Integer_64,                      -- Interfaces
     RE_Unsigned_8,                      -- Interfaces
     RE_Unsigned_16,                     -- Interfaces
     RE_Unsigned_32,                     -- Interfaces
     RE_Unsigned_64,                     -- Interfaces

     RE_Vtable_Ptr,                      -- Interfaces.CPP
     RE_Displaced_This,                  -- Interfaces.CPP
     RE_CPP_CW_Membership,               -- Interfaces.CPP
     RE_CPP_DT_Entry_Size,               -- Interfaces.CPP
     RE_CPP_DT_Prologue_Size,            -- Interfaces.CPP
     RE_CPP_Get_External_Tag,            -- Interfaces.CPP
     RE_CPP_Get_Prim_Op_Address,         -- Interfaces.CPP
     RE_CPP_Get_RC_Offset,               -- Interfaces.CPP
     RE_CPP_Get_Remotely_Callable,       -- Interfaces.CPP
     RE_CPP_Inherit_DT,                  -- Interfaces.CPP
     RE_CPP_Inherit_TSD,                 -- Interfaces.CPP
     RE_CPP_Register_Tag,                -- Interfaces.CPP
     RE_CPP_Set_Expanded_Name,           -- Interfaces.CPP
     RE_CPP_Set_External_Tag,            -- Interfaces.CPP
     RE_CPP_Set_Prim_Op_Address,         -- Interfaces.CPP
     RE_CPP_Set_RC_Offset,               -- Interfaces.CPP
     RE_CPP_Set_Remotely_Callable,       -- Interfaces.CPP
     RE_CPP_Set_TSD,                     -- Interfaces.CPP
     RE_CPP_TSD_Entry_Size,              -- Interfaces.CPP
     RE_CPP_TSD_Prologue_Size,           -- Interfaces.CPP

     RE_Packed_Size,                     -- Interfaces.Packed_Decimal
     RE_Packed_To_Int32,                 -- Interfaces.Packed_Decimal
     RE_Packed_To_Int64,                 -- Interfaces.Packed_Decimal
     RE_Int32_To_Packed,                 -- Interfaces.Packed_Decimal
     RE_Int64_To_Packed,                 -- Interfaces.Packed_Decimal

     RE_Address,                         -- System
     RE_Any_Priority,                    -- System
     RE_Bit_Order,                       -- System
     RE_Default_Priority,                -- System
     RE_High_Order_First,                -- System
     RE_Interrupt_Priority,              -- System
     RE_Lib_Stop,                        -- System
     RE_Low_Order_First,                 -- System
     RE_Max_Interrupt_Priority,          -- System
     RE_Max_Priority,                    -- System
     RE_Null_Address,                    -- System
     RE_Priority,                        -- System

     RE_Add_With_Ovflo_Check,            -- System.Arith_64
     RE_Double_Divide,                   -- System.Arith_64
     RE_Multiply_With_Ovflo_Check,       -- System.Arith_64
     RE_Scaled_Divide,                   -- System.Arith_64
     RE_Subtract_With_Ovflo_Check,       -- System.Arith_64

     RE_Create_AST_Handler,              -- System.AST_Handling

     RE_Raise_Assert_Failure,            -- System.Assertions

     RE_AST_Handler,                     -- System.Aux_DEC
     RE_Import_Value,                    -- System.Aux_DEC
     RE_No_AST_Handler,                  -- System.Aux_DEC
     RE_Type_Class,                      -- System.Aux_DEC
     RE_Type_Class_Enumeration,          -- System.Aux_DEC
     RE_Type_Class_Integer,              -- System.Aux_DEC
     RE_Type_Class_Fixed_Point,          -- System.Aux_DEC
     RE_Type_Class_Floating_Point,       -- System.Aux_DEC
     RE_Type_Class_Array,                -- System.Aux_DEC
     RE_Type_Class_Record,               -- System.Aux_DEC
     RE_Type_Class_Access,               -- System.Aux_DEC
     RE_Type_Class_Task,                 -- System.Aux_DEC
     RE_Type_Class_Address,              -- System.Aux_DEC

     RE_Bit_And,                         -- System.Bit_Ops
     RE_Bit_Eq,                          -- System.Bit_Ops
     RE_Bit_Not,                         -- System.Bit_Ops
     RE_Bit_Or,                          -- System.Bit_Ops
     RE_Bit_Xor,                         -- System.Bit_Ops

     RE_Boolean_Array,                   -- System_Boolean_Array_Operations,
     RE_Vector_Not,                      -- System_Boolean_Array_Operations,
     RE_Vector_And,                      -- System_Boolean_Array_Operations,
     RE_Vector_Or,                       -- System_Boolean_Array_Operations,
     RE_Vector_Nand,                     -- System_Boolean_Array_Operations,
     RE_Vector_Nor,                      -- System_Boolean_Array_Operations,
     RE_Vector_Nxor,                     -- System_Boolean_Array_Operations,
     RE_Vector_Xor,                      -- System_Boolean_Array_Operations,

     RE_Checked_Pool,                    -- System.Checked_Pools

     RE_Compare_Array_S8,                -- System.Compare_Array_Signed_8
     RE_Compare_Array_S8_Unaligned,      -- System.Compare_Array_Signed_8

     RE_Compare_Array_S16,               -- System.Compare_Array_Signed_16

     RE_Compare_Array_S32,               -- System.Compare_Array_Signed_16

     RE_Compare_Array_S64,               -- System.Compare_Array_Signed_16

     RE_Compare_Array_U8,                -- System.Compare_Array_Unsigned_8
     RE_Compare_Array_U8_Unaligned,      -- System.Compare_Array_Unsigned_8

     RE_Compare_Array_U16,               -- System.Compare_Array_Unsigned_16

     RE_Compare_Array_U32,               -- System.Compare_Array_Unsigned_16

     RE_Compare_Array_U64,               -- System.Compare_Array_Unsigned_16

     RE_Register_Exception,              -- System.Exception_Table

     RE_Exn_Integer,                     -- System.Exn_Int

     RE_Exn_Long_Long_Float,             -- System.Exn_LLF

     RE_Exn_Long_Long_Integer,           -- System.Exn_LLI

     RE_Exp_Integer,                     -- System.Exp_Int

     RE_Exp_Long_Long_Integer,           -- System.Exp_LLI

     RE_Exp_Long_Long_Unsigned,          -- System.Exp_LLU

     RE_Exp_Modular,                     -- System.Exp_Mod

     RE_Exp_Unsigned,                    -- System.Exp_Uns

     RE_Attr_Float,                      -- System.Fat_Flt

     RE_Attr_IEEE_Long,                  -- System.Fat_IEEE_Long_Float
     RE_Fat_IEEE_Long,                   -- System.Fat_IEEE_Long_Float

     RE_Attr_IEEE_Short,                 -- System.Fat_IEEE_Short_Float
     RE_Fat_IEEE_Short,                  -- System.Fat_IEEE_Short_Float

     RE_Attr_Long_Float,                 -- System.Fat_LFlt

     RE_Attr_Long_Long_Float,            -- System.Fat_LLF

     RE_Attr_Short_Float,                -- System.Fat_SFlt

     RE_Attr_VAX_D_Float,                -- System.Fat_VAX_D_Float
     RE_Fat_VAX_D,                       -- System.Fat_VAX_D_Float

     RE_Attr_VAX_F_Float,                -- System.Fat_VAX_F_Float
     RE_Fat_VAX_F,                       -- System.Fat_VAX_F_Float

     RE_Attr_VAX_G_Float,                -- System.Fat_VAX_G_Float
     RE_Fat_VAX_G,                       -- System.Fat_VAX_G_Float

     RE_Attach_To_Final_List,            -- System.Finalization_Implementation
     RE_Finalize_List,                   -- System.Finalization_Implementation
     RE_Finalize_One,                    -- System.Finalization_Implementation
     RE_Global_Final_List,               -- System.Finalization_Implementation
     RE_Record_Controller,               -- System.Finalization_Implementation
     RE_Limited_Record_Controller,       -- System.Finalization_Implementation
     RE_Deep_Tag_Initialize,             -- System.Finalization_Implementation
     RE_Deep_Tag_Adjust,                 -- System.Finalization_Implementation
     RE_Deep_Tag_Finalize,               -- System.Finalization_Implementation
     RE_Deep_Tag_Attach,                 -- System.Finalization_Implementation
     RE_Deep_Rec_Initialize,             -- System.Finalization_Implementation
     RE_Deep_Rec_Adjust,                 -- System.Finalization_Implementation
     RE_Deep_Rec_Finalize,               -- System.Finalization_Implementation

     RE_Root_Controlled,                 -- System.Finalization_Root
     RE_Finalizable,                     -- System.Finalization_Root
     RE_Finalizable_Ptr,                 -- System.Finalization_Root

     RE_Fore,                            -- System.Fore

     RE_Image_Boolean,                   -- System.Img_Bool

     RE_Image_Character,                 -- System.Img_Char

     RE_Image_Decimal,                   -- System.Img_Dec

     RE_Image_Enumeration_8,             -- System.Img_Enum
     RE_Image_Enumeration_16,            -- System.Img_Enum
     RE_Image_Enumeration_32,            -- System.Img_Enum

     RE_Image_Integer,                   -- System.Img_Int

     RE_Image_Long_Long_Decimal,         -- System.Img_LLD

     RE_Image_Long_Long_Integer,         -- System.Img_LLI

     RE_Image_Long_Long_Unsigned,        -- System.Img_LLU

     RE_Image_Ordinary_Fixed_Point,      -- System.Img_Real
     RE_Image_Floating_Point,            -- System.Img_Real

     RE_Image_Unsigned,                  -- System.Img_Uns

     RE_Image_Wide_Character,            -- System.Img_WChar
     RE_Image_Wide_Wide_Character,       -- System.Img_WChar

     RE_Bind_Interrupt_To_Entry,         -- System.Interrupts
     RE_Default_Interrupt_Priority,      -- System.Interrupts
     RE_Dynamic_Interrupt_Protection,    -- System.Interrupts
     RE_Install_Handlers,                -- System.Interrupts
     RE_Register_Interrupt_Handler,      -- System.Interrupts
     RE_Static_Interrupt_Protection,     -- System.Interrupts
     RE_System_Interrupt_Id,             -- System.Interrupts

     RE_Asm_Insn,                        -- System.Machine_Code
     RE_Asm_Input_Operand,               -- System.Machine_Code
     RE_Asm_Output_Operand,              -- System.Machine_Code

     RE_Mantissa_Value,                  -- System_Mantissa

     RE_memcpy,                          -- System_Memcop
     RE_memmove,                         -- System_Memcop

     RE_Bits_03,                         -- System.Pack_03
     RE_Get_03,                          -- System.Pack_03
     RE_Set_03,                          -- System.Pack_03

     RE_Bits_05,                         -- System.Pack_05
     RE_Get_05,                          -- System.Pack_05
     RE_Set_05,                          -- System.Pack_05

     RE_Bits_06,                         -- System.Pack_06
     RE_Get_06,                          -- System.Pack_06
     RE_GetU_06,                         -- System.Pack_06
     RE_Set_06,                          -- System.Pack_06
     RE_SetU_06,                         -- System.Pack_06

     RE_Bits_07,                         -- System.Pack_07
     RE_Get_07,                          -- System.Pack_07
     RE_Set_07,                          -- System.Pack_07

     RE_Bits_09,                         -- System.Pack_09
     RE_Get_09,                          -- System.Pack_09
     RE_Set_09,                          -- System.Pack_09

     RE_Bits_10,                         -- System.Pack_10
     RE_Get_10,                          -- System.Pack_10
     RE_GetU_10,                         -- System.Pack_10
     RE_Set_10,                          -- System.Pack_10
     RE_SetU_10,                         -- System.Pack_10

     RE_Bits_11,                         -- System.Pack_11
     RE_Get_11,                          -- System.Pack_11
     RE_Set_11,                          -- System.Pack_11

     RE_Bits_12,                         -- System.Pack_12
     RE_Get_12,                          -- System.Pack_12
     RE_GetU_12,                         -- System.Pack_12
     RE_Set_12,                          -- System.Pack_12
     RE_SetU_12,                         -- System.Pack_12

     RE_Bits_13,                         -- System.Pack_13
     RE_Get_13,                          -- System.Pack_13
     RE_Set_13,                          -- System.Pack_13

     RE_Bits_14,                         -- System.Pack_14
     RE_Get_14,                          -- System.Pack_14
     RE_GetU_14,                         -- System.Pack_14
     RE_Set_14,                          -- System.Pack_14
     RE_SetU_14,                         -- System.Pack_14

     RE_Bits_15,                         -- System.Pack_15
     RE_Get_15,                          -- System.Pack_15
     RE_Set_15,                          -- System.Pack_15

     RE_Bits_17,                         -- System.Pack_17
     RE_Get_17,                          -- System.Pack_17
     RE_Set_17,                          -- System.Pack_17

     RE_Bits_18,                         -- System.Pack_18
     RE_Get_18,                          -- System.Pack_18
     RE_GetU_18,                         -- System.Pack_18
     RE_Set_18,                          -- System.Pack_18
     RE_SetU_18,                         -- System.Pack_18

     RE_Bits_19,                         -- System.Pack_19
     RE_Get_19,                          -- System.Pack_19
     RE_Set_19,                          -- System.Pack_19

     RE_Bits_20,                         -- System.Pack_20
     RE_Get_20,                          -- System.Pack_20
     RE_GetU_20,                         -- System.Pack_20
     RE_Set_20,                          -- System.Pack_20
     RE_SetU_20,                         -- System.Pack_20

     RE_Bits_21,                         -- System.Pack_21
     RE_Get_21,                          -- System.Pack_21
     RE_Set_21,                          -- System.Pack_21

     RE_Bits_22,                         -- System.Pack_22
     RE_Get_22,                          -- System.Pack_22
     RE_GetU_22,                         -- System.Pack_22
     RE_Set_22,                          -- System.Pack_22
     RE_SetU_22,                         -- System.Pack_22

     RE_Bits_23,                         -- System.Pack_23
     RE_Get_23,                          -- System.Pack_23
     RE_Set_23,                          -- System.Pack_23

     RE_Bits_24,                         -- System.Pack_24
     RE_Get_24,                          -- System.Pack_24
     RE_GetU_24,                         -- System.Pack_24
     RE_Set_24,                          -- System.Pack_24
     RE_SetU_24,                         -- System.Pack_24

     RE_Bits_25,                         -- System.Pack_25
     RE_Get_25,                          -- System.Pack_25
     RE_Set_25,                          -- System.Pack_25

     RE_Bits_26,                         -- System.Pack_26
     RE_Get_26,                          -- System.Pack_26
     RE_GetU_26,                         -- System.Pack_26
     RE_Set_26,                          -- System.Pack_26
     RE_SetU_26,                         -- System.Pack_26

     RE_Bits_27,                         -- System.Pack_27
     RE_Get_27,                          -- System.Pack_27
     RE_Set_27,                          -- System.Pack_27

     RE_Bits_28,                         -- System.Pack_28
     RE_Get_28,                          -- System.Pack_28
     RE_GetU_28,                         -- System.Pack_28
     RE_Set_28,                          -- System.Pack_28
     RE_SetU_28,                         -- System.Pack_28

     RE_Bits_29,                         -- System.Pack_29
     RE_Get_29,                          -- System.Pack_29
     RE_Set_29,                          -- System.Pack_29

     RE_Bits_30,                         -- System.Pack_30
     RE_Get_30,                          -- System.Pack_30
     RE_GetU_30,                         -- System.Pack_30
     RE_Set_30,                          -- System.Pack_30
     RE_SetU_30,                         -- System.Pack_30

     RE_Bits_31,                         -- System.Pack_31
     RE_Get_31,                          -- System.Pack_31
     RE_Set_31,                          -- System.Pack_31

     RE_Bits_33,                         -- System.Pack_33
     RE_Get_33,                          -- System.Pack_33
     RE_Set_33,                          -- System.Pack_33

     RE_Bits_34,                         -- System.Pack_34
     RE_Get_34,                          -- System.Pack_34
     RE_GetU_34,                         -- System.Pack_34
     RE_Set_34,                          -- System.Pack_34
     RE_SetU_34,                         -- System.Pack_34

     RE_Bits_35,                         -- System.Pack_35
     RE_Get_35,                          -- System.Pack_35
     RE_Set_35,                          -- System.Pack_35

     RE_Bits_36,                         -- System.Pack_36
     RE_Get_36,                          -- System.Pack_36
     RE_GetU_36,                         -- System.Pack_36
     RE_Set_36,                          -- System.Pack_36
     RE_SetU_36,                         -- System.Pack_36

     RE_Bits_37,                         -- System.Pack_37
     RE_Get_37,                          -- System.Pack_37
     RE_Set_37,                          -- System.Pack_37

     RE_Bits_38,                         -- System.Pack_38
     RE_Get_38,                          -- System.Pack_38
     RE_GetU_38,                         -- System.Pack_38
     RE_Set_38,                          -- System.Pack_38
     RE_SetU_38,                         -- System.Pack_38

     RE_Bits_39,                         -- System.Pack_39
     RE_Get_39,                          -- System.Pack_39
     RE_Set_39,                          -- System.Pack_39

     RE_Bits_40,                         -- System.Pack_40
     RE_Get_40,                          -- System.Pack_40
     RE_GetU_40,                         -- System.Pack_40
     RE_Set_40,                          -- System.Pack_40
     RE_SetU_40,                         -- System.Pack_40

     RE_Bits_41,                         -- System.Pack_41
     RE_Get_41,                          -- System.Pack_41
     RE_Set_41,                          -- System.Pack_41

     RE_Bits_42,                         -- System.Pack_42
     RE_Get_42,                          -- System.Pack_42
     RE_GetU_42,                         -- System.Pack_42
     RE_Set_42,                          -- System.Pack_42
     RE_SetU_42,                         -- System.Pack_42

     RE_Bits_43,                         -- System.Pack_43
     RE_Get_43,                          -- System.Pack_43
     RE_Set_43,                          -- System.Pack_43

     RE_Bits_44,                         -- System.Pack_44
     RE_Get_44,                          -- System.Pack_44
     RE_GetU_44,                         -- System.Pack_44
     RE_Set_44,                          -- System.Pack_44
     RE_SetU_44,                         -- System.Pack_44

     RE_Bits_45,                         -- System.Pack_45
     RE_Get_45,                          -- System.Pack_45
     RE_Set_45,                          -- System.Pack_45

     RE_Bits_46,                         -- System.Pack_46
     RE_Get_46,                          -- System.Pack_46
     RE_GetU_46,                         -- System.Pack_46
     RE_Set_46,                          -- System.Pack_46
     RE_SetU_46,                         -- System.Pack_46

     RE_Bits_47,                         -- System.Pack_47
     RE_Get_47,                          -- System.Pack_47
     RE_Set_47,                          -- System.Pack_47

     RE_Bits_48,                         -- System.Pack_48
     RE_Get_48,                          -- System.Pack_48
     RE_GetU_48,                         -- System.Pack_48
     RE_Set_48,                          -- System.Pack_48
     RE_SetU_48,                         -- System.Pack_48

     RE_Bits_49,                         -- System.Pack_49
     RE_Get_49,                          -- System.Pack_49
     RE_Set_49,                          -- System.Pack_49

     RE_Bits_50,                         -- System.Pack_50
     RE_Get_50,                          -- System.Pack_50
     RE_GetU_50,                         -- System.Pack_50
     RE_Set_50,                          -- System.Pack_50
     RE_SetU_50,                         -- System.Pack_50

     RE_Bits_51,                         -- System.Pack_51
     RE_Get_51,                          -- System.Pack_51
     RE_Set_51,                          -- System.Pack_51

     RE_Bits_52,                         -- System.Pack_52
     RE_Get_52,                          -- System.Pack_52
     RE_GetU_52,                         -- System.Pack_52
     RE_Set_52,                          -- System.Pack_52
     RE_SetU_52,                         -- System.Pack_52

     RE_Bits_53,                         -- System.Pack_53
     RE_Get_53,                          -- System.Pack_53
     RE_Set_53,                          -- System.Pack_53

     RE_Bits_54,                         -- System.Pack_54
     RE_Get_54,                          -- System.Pack_54
     RE_GetU_54,                         -- System.Pack_54
     RE_Set_54,                          -- System.Pack_54
     RE_SetU_54,                         -- System.Pack_54

     RE_Bits_55,                         -- System.Pack_55
     RE_Get_55,                          -- System.Pack_55
     RE_Set_55,                          -- System.Pack_55

     RE_Bits_56,                         -- System.Pack_56
     RE_Get_56,                          -- System.Pack_56
     RE_GetU_56,                         -- System.Pack_56
     RE_Set_56,                          -- System.Pack_56
     RE_SetU_56,                         -- System.Pack_56

     RE_Bits_57,                         -- System.Pack_57
     RE_Get_57,                          -- System.Pack_57
     RE_Set_57,                          -- System.Pack_57

     RE_Bits_58,                         -- System.Pack_58
     RE_Get_58,                          -- System.Pack_58
     RE_GetU_58,                         -- System.Pack_58
     RE_Set_58,                          -- System.Pack_58
     RE_SetU_58,                         -- System.Pack_58

     RE_Bits_59,                         -- System.Pack_59
     RE_Get_59,                          -- System.Pack_59
     RE_Set_59,                          -- System.Pack_59

     RE_Bits_60,                         -- System.Pack_60
     RE_Get_60,                          -- System.Pack_60
     RE_GetU_60,                         -- System.Pack_60
     RE_Set_60,                          -- System.Pack_60
     RE_SetU_60,                         -- System.Pack_60

     RE_Bits_61,                         -- System.Pack_61
     RE_Get_61,                          -- System.Pack_61
     RE_Set_61,                          -- System.Pack_61

     RE_Bits_62,                         -- System.Pack_62
     RE_Get_62,                          -- System.Pack_62
     RE_GetU_62,                         -- System.Pack_62
     RE_Set_62,                          -- System.Pack_62
     RE_SetU_62,                         -- System.Pack_62

     RE_Bits_63,                         -- System.Pack_63
     RE_Get_63,                          -- System.Pack_63
     RE_Set_63,                          -- System.Pack_63

     RE_Adjust_Storage_Size,             -- System_Parameters
     RE_Default_Stack_Size,              -- System.Parameters
     RE_Garbage_Collected,               -- System.Parameters
     RE_Size_Type,                       -- System.Parameters
     RE_Unspecified_Size,                -- System.Parameters

     RE_DSA_Implementation,              -- System.Partition_Interface
     RE_Get_Active_Partition_Id,         -- System.Partition_Interface
     RE_Get_Passive_Partition_Id,        -- System.Partition_Interface
     RE_Get_Local_Partition_Id,          -- System.Partition_Interface
     RE_Get_RCI_Package_Receiver,        -- System.Partition_Interface
     RE_Get_Unique_Remote_Pointer,       -- System.Partition_Interface
     RE_RACW_Stub_Type,                  -- System.Partition_Interface
     RE_RACW_Stub_Type_Access,           -- System.Partition_Interface
     RE_RAS_Proxy_Type,                  -- System.Partition_Interface
     RE_RAS_Proxy_Type_Access,           -- System.Partition_Interface
     RE_Raise_Program_Error_Unknown_Tag, -- System.Partition_Interface
     RE_Register_Passive_Package,        -- System.Partition_Interface
     RE_Register_Receiving_Stub,         -- System.Partition_Interface
     RE_Request_Access,                  -- System.Partition_Interface
     RE_RCI_Locator,                     -- System.Partition_Interface
     RE_RCI_Subp_Info,                   -- System.Partition_Interface
     RE_RCI_Subp_Info_Array,             -- System.Partition_Interface
     RE_Same_Partition,                  -- System.Partition_Interface
     RE_Subprogram_Id,                   -- System.Partition_Interface
     RE_Get_RAS_Info,                    -- System.Partition_Interface

     RE_Global_Pool_Object,              -- System.Pool_Global

     RE_Stack_Bounded_Pool,              -- System.Pool_Size

     RE_Do_Apc,                          -- System.RPC
     RE_Do_Rpc,                          -- System.RPC
     RE_Params_Stream_Type,              -- System.RPC
     RE_Partition_ID,                    -- System.RPC

     RE_To_PolyORB_String,               -- System.PolyORB_Interface
     RE_To_Standard_String,              -- System.PolyORB_Interface
     RE_Caseless_String_Eq,              -- System.PolyORB_Interface
     RE_TypeCode,                        -- System.PolyORB_Interface
     RE_Any,                             -- System.PolyORB_Interface
     RE_Mode_In,                         -- System.PolyORB_Interface
     RE_Mode_Out,                        -- System.PolyORB_Interface
     RE_Mode_Inout,                      -- System.PolyORB_Interface
     RE_NamedValue,                      -- System.PolyORB_Interface
     RE_Result_Name,                     -- System.PolyORB_Interface
     RE_Object_Ref,                      -- System.PolyORB_Interface
     RE_Create_Any,                      -- System.PolyORB_Interface
     RE_Any_Aggregate_Build,             -- System.PolyORB_Interface
     RE_Add_Aggregate_Element,           -- System.PolyORB_Interface
     RE_Get_Aggregate_Element,           -- System.PolyORB_Interface
     RE_Content_Type,                    -- System.PolyORB_Interface
     RE_Any_Member_Type,                 -- System.PolyORB_Interface
     RE_Get_Nested_Sequence_Length,      -- System.PolyORB_Interface
     RE_Extract_Union_Value,             -- System.PolyORB_Interface
     RE_NVList_Ref,                      -- System.PolyORB_Interface
     RE_NVList_Create,                   -- System.PolyORB_Interface
     RE_NVList_Add_Item,                 -- System.PolyORB_Interface
     RE_Request_Create,                  -- System.PolyORB_Interface
     RE_Request_Invoke,                  -- System.PolyORB_Interface
     RE_Request_Arguments,               -- System.PolyORB_Interface
     RE_Request_Set_Out,                 -- System.PolyORB_Interface
     RE_Request_Raise_Occurrence,        -- System.PolyORB_Interface
     RE_Nil_Exc_List,                    -- System.PolyORB_Interface
     RE_Servant,                         -- System.PolyORB_Interface
     RE_Copy_Any_Value,                  -- System.PolyORB_Interface
     RE_Set_Result,                      -- System.PolyORB_Interface
     RE_Register_Obj_Receiving_Stub,     -- System.PolyORB_Interface
     RE_Register_Pkg_Receiving_Stub,     -- System.PolyORB_Interface
     RE_Is_Nil,                          -- System.PolyORB_Interface
     RE_Entity_Ptr,                      -- System.PolyORB_Interface
     RE_Entity_Of,                       -- System.PolyORB_Interface
     RE_Inc_Usage,                       -- System.PolyORB_Interface
     RE_Set_Ref,                         -- System.PolyORB_Interface
     RE_Make_Ref,                        -- System.PolyORB_Interface
     RE_Get_Local_Address,               -- System.PolyORB_Interface
     RE_Get_Reference,                   -- System.PolyORB_Interface
     RE_Local_Oid_To_Address,            -- System.PolyORB_Interface
     RE_Asynchronous_P_To_Sync_Scope,    -- System.PolyORB_Interface
     RE_Buffer_Stream_Type,              -- System.PolyORB_Interface
     RE_Allocate_Buffer,                 -- System.PolyORB_Interface
     RE_Release_Buffer,                  -- System.PolyORB_Interface
     RE_BS_To_Any,                       -- System.PolyORB_Interface
     RE_Any_To_BS,                       -- System.PolyORB_Interface

     RE_FA_AD,                           -- System.PolyORB_Interface
     RE_FA_AS,                           -- System.PolyORB_Interface
     RE_FA_B,                            -- System.PolyORB_Interface
     RE_FA_C,                            -- System.PolyORB_Interface
     RE_FA_F,                            -- System.PolyORB_Interface
     RE_FA_I,                            -- System.PolyORB_Interface
     RE_FA_LF,                           -- System.PolyORB_Interface
     RE_FA_LI,                           -- System.PolyORB_Interface
     RE_FA_LLF,                          -- System.PolyORB_Interface
     RE_FA_LLI,                          -- System.PolyORB_Interface
     RE_FA_LLU,                          -- System.PolyORB_Interface
     RE_FA_LU,                           -- System.PolyORB_Interface
     RE_FA_SF,                           -- System.PolyORB_Interface
     RE_FA_SI,                           -- System.PolyORB_Interface
     RE_FA_SSI,                          -- System.PolyORB_Interface
     RE_FA_SSU,                          -- System.PolyORB_Interface
     RE_FA_SU,                           -- System.PolyORB_Interface
     RE_FA_U,                            -- System.PolyORB_Interface
     RE_FA_WC,                           -- System.PolyORB_Interface
     RE_FA_WWC,                          -- System.PolyORB_Interface
     RE_FA_String,                       -- System.PolyORB_Interface
     RE_FA_ObjRef,                       -- System.PolyORB_Interface

     RE_TA_AD,                           -- System.PolyORB_Interface
     RE_TA_AS,                           -- System.PolyORB_Interface
     RE_TA_B,                            -- System.PolyORB_Interface
     RE_TA_C,                            -- System.PolyORB_Interface
     RE_TA_F,                            -- System.PolyORB_Interface
     RE_TA_I,                            -- System.PolyORB_Interface
     RE_TA_LF,                           -- System.PolyORB_Interface
     RE_TA_LI,                           -- System.PolyORB_Interface
     RE_TA_LLF,                          -- System.PolyORB_Interface
     RE_TA_LLI,                          -- System.PolyORB_Interface
     RE_TA_LLU,                          -- System.PolyORB_Interface
     RE_TA_LU,                           -- System.PolyORB_Interface
     RE_TA_SF,                           -- System.PolyORB_Interface
     RE_TA_SI,                           -- System.PolyORB_Interface
     RE_TA_SSI,                          -- System.PolyORB_Interface
     RE_TA_SSU,                          -- System.PolyORB_Interface
     RE_TA_SU,                           -- System.PolyORB_Interface
     RE_TA_U,                            -- System.PolyORB_Interface
     RE_TA_WC,                           -- System.PolyORB_Interface
     RE_TA_WWC,                          -- System.PolyORB_Interface
     RE_TA_String,                       -- System.PolyORB_Interface
     RE_TA_ObjRef,                       -- System.PolyORB_Interface
     RE_TA_TC,                           -- System.PolyORB_Interface

     RE_TC_Alias,                        -- System.PolyORB_Interface
     RE_TC_Build,                        -- System.PolyORB_Interface
     RE_Get_TC,                          -- System.PolyORB_Interface
     RE_Set_TC,                          -- System.PolyORB_Interface
     RE_TC_Any,                          -- System.PolyORB_Interface
     RE_TC_AD,                           -- System.PolyORB_Interface
     RE_TC_AS,                           -- System.PolyORB_Interface
     RE_TC_B,                            -- System.PolyORB_Interface
     RE_TC_C,                            -- System.PolyORB_Interface
     RE_TC_F,                            -- System.PolyORB_Interface
     RE_TC_I,                            -- System.PolyORB_Interface
     RE_TC_LF,                           -- System.PolyORB_Interface
     RE_TC_LI,                           -- System.PolyORB_Interface
     RE_TC_LLF,                          -- System.PolyORB_Interface
     RE_TC_LLI,                          -- System.PolyORB_Interface
     RE_TC_LLU,                          -- System.PolyORB_Interface
     RE_TC_LU,                           -- System.PolyORB_Interface
     RE_TC_SF,                           -- System.PolyORB_Interface
     RE_TC_SI,                           -- System.PolyORB_Interface
     RE_TC_SSI,                          -- System.PolyORB_Interface
     RE_TC_SSU,                          -- System.PolyORB_Interface
     RE_TC_SU,                           -- System.PolyORB_Interface
     RE_TC_U,                            -- System.PolyORB_Interface
     RE_TC_Void,                         -- System.PolyORB_Interface
     RE_TC_Opaque,                       -- System.PolyORB_Interface,
     RE_TC_WC,                           -- System.PolyORB_Interface
     RE_TC_WWC,                          -- System.PolyORB_Interface
     RE_TC_Array,                        -- System.PolyORB_Interface,
     RE_TC_Sequence,                     -- System.PolyORB_Interface,
     RE_TC_String,                       -- System.PolyORB_Interface,
     RE_TC_Struct,                       -- System.PolyORB_Interface,
     RE_TC_Union,                        -- System.PolyORB_Interface,
     RE_TC_Object,                       -- System.PolyORB_Interface,

     RE_IS_Is1,                          -- System.Scalar_Values
     RE_IS_Is2,                          -- System.Scalar_Values
     RE_IS_Is4,                          -- System.Scalar_Values
     RE_IS_Is8,                          -- System.Scalar_Values
     RE_IS_Iu1,                          -- System.Scalar_Values
     RE_IS_Iu2,                          -- System.Scalar_Values
     RE_IS_Iu4,                          -- System.Scalar_Values
     RE_IS_Iu8,                          -- System.Scalar_Values
     RE_IS_Iz1,                          -- System.Scalar_Values
     RE_IS_Iz2,                          -- System.Scalar_Values
     RE_IS_Iz4,                          -- System.Scalar_Values
     RE_IS_Iz8,                          -- System.Scalar_Values
     RE_IS_Isf,                          -- System.Scalar_Values
     RE_IS_Ifl,                          -- System.Scalar_Values
     RE_IS_Ilf,                          -- System.Scalar_Values
     RE_IS_Ill,                          -- System.Scalar_Values

     RE_Default_Secondary_Stack_Size,    -- System.Secondary_Stack
     RE_Mark_Id,                         -- System.Secondary_Stack
     RE_SS_Allocate,                     -- System.Secondary_Stack
     RE_SS_Pool,                         -- System.Secondary_Stack
     RE_SS_Mark,                         -- System.Secondary_Stack
     RE_SS_Release,                      -- System.Secondary_Stack

     RE_Shared_Var_Close,                -- System.Shared_Storage
     RE_Shared_Var_Lock,                 -- System.Shared_Storage
     RE_Shared_Var_ROpen,                -- System.Shared_Storage
     RE_Shared_Var_Unlock,               -- System.Shared_Storage
     RE_Shared_Var_WOpen,                -- System.Shared_Storage

     RE_Abort_Undefer_Direct,            -- System.Standard_Library
     RE_Exception_Code,                  -- System.Standard_Library
     RE_Exception_Data_Ptr,              -- System.Standard_Library

     RE_Integer_Address,                 -- System.Storage_Elements
     RE_Storage_Offset,                  -- System.Storage_Elements
     RE_Storage_Array,                   -- System.Storage_Elements
     RE_Storage_Element,                 -- System.Storage_Elements
     RE_To_Address,                      -- System.Storage_Elements

     RE_Root_Storage_Pool,               -- System.Storage_Pools
     RE_Allocate_Any,                    -- System_Storage_Pools,
     RE_Deallocate_Any,                  -- System_Storage_Pools,

     RE_Thin_Pointer,                    -- System.Stream_Attributes
     RE_Fat_Pointer,                     -- System.Stream_Attributes

     RE_I_AD,                            -- System.Stream_Attributes
     RE_I_AS,                            -- System.Stream_Attributes
     RE_I_B,                             -- System.Stream_Attributes
     RE_I_C,                             -- System.Stream_Attributes
     RE_I_F,                             -- System.Stream_Attributes
     RE_I_I,                             -- System.Stream_Attributes
     RE_I_LF,                            -- System.Stream_Attributes
     RE_I_LI,                            -- System.Stream_Attributes
     RE_I_LLF,                           -- System.Stream_Attributes
     RE_I_LLI,                           -- System.Stream_Attributes
     RE_I_LLU,                           -- System.Stream_Attributes
     RE_I_LU,                            -- System.Stream_Attributes
     RE_I_SF,                            -- System.Stream_Attributes
     RE_I_SI,                            -- System.Stream_Attributes
     RE_I_SSI,                           -- System.Stream_Attributes
     RE_I_SSU,                           -- System.Stream_Attributes
     RE_I_SU,                            -- System.Stream_Attributes
     RE_I_U,                             -- System.Stream_Attributes
     RE_I_WC,                            -- System.Stream_Attributes
     RE_I_WWC,                           -- System.Stream_Attributes

     RE_W_AD,                            -- System.Stream_Attributes
     RE_W_AS,                            -- System.Stream_Attributes
     RE_W_B,                             -- System.Stream_Attributes
     RE_W_C,                             -- System.Stream_Attributes
     RE_W_F,                             -- System.Stream_Attributes
     RE_W_I,                             -- System.Stream_Attributes
     RE_W_LF,                            -- System.Stream_Attributes
     RE_W_LI,                            -- System.Stream_Attributes
     RE_W_LLF,                           -- System.Stream_Attributes
     RE_W_LLI,                           -- System.Stream_Attributes
     RE_W_LLU,                           -- System.Stream_Attributes
     RE_W_LU,                            -- System.Stream_Attributes
     RE_W_SF,                            -- System.Stream_Attributes
     RE_W_SI,                            -- System.Stream_Attributes
     RE_W_SSI,                           -- System.Stream_Attributes
     RE_W_SSU,                           -- System.Stream_Attributes
     RE_W_SU,                            -- System.Stream_Attributes
     RE_W_U,                             -- System.Stream_Attributes
     RE_W_WC,                            -- System.Stream_Attributes
     RE_W_WWC,                           -- System.Stream_Attributes

     RE_Block_Stream_Ops_OK,             -- System.Stream_Attributes

     RE_Str_Concat,                      -- System.String_Ops
     RE_Str_Concat_CC,                   -- System.String_Ops
     RE_Str_Concat_CS,                   -- System.String_Ops
     RE_Str_Concat_SC,                   -- System.String_Ops

     RE_Str_Concat_3,                    -- System.String_Ops_Concat_3

     RE_Str_Concat_4,                    -- System.String_Ops_Concat_4

     RE_Str_Concat_5,                    -- System.String_Ops_Concat_5

     RE_Task_Info_Type,                  -- System.Task_Info
     RE_Unspecified_Task_Info,           -- System.Task_Info

     RE_Library_Task_Level,              -- System.Tasking

     RE_Task_Procedure_Access,           -- System.Tasking

     RO_ST_Task_Id,                      -- System.Tasking
     RO_ST_Null_Task,                    -- System.Tasking

     RE_Call_Modes,                      -- System.Tasking
     RE_Simple_Call,                     -- System.Tasking
     RE_Conditional_Call,                -- System.Tasking
     RE_Asynchronous_Call,               -- System.Tasking
     RE_Timed_Call,                      -- System.Tasking

     RE_Ada_Task_Control_Block,          -- System.Tasking

     RE_Task_List,                       -- System.Tasking

     RE_Accept_Alternative,              -- System.Tasking
     RE_Accept_List,                     -- System.Tasking
     RE_Accept_List_Access,              -- System.Tasking
     RE_Max_Select,                      -- System.Tasking
     RE_Max_Task_Entry,                  -- System.Tasking
     RE_No_Rendezvous,                   -- System.Tasking
     RE_Null_Task_Entry,                 -- System.Tasking
     RE_Positive_Select_Index,           -- System.Tasking
     RE_Select_Index,                    -- System.Tasking
     RE_Select_Modes,                    -- System.Tasking
     RE_Else_Mode,                       -- System.Tasking
     RE_Simple_Mode,                     -- System.Tasking
     RE_Terminate_Mode,                  -- System.Tasking
     RE_Delay_Mode,                      -- System.Tasking
     RE_Task_Entry_Index,                -- System.Tasking
     RE_Self,                            -- System.Tasking

     RE_Master_Id,                       -- System.Tasking
     RE_Unspecified_Priority,            -- System.Tasking

     RE_Activation_Chain,                -- System.Tasking

     RE_Abort_Defer,                     -- System.Soft_Links
     RE_Abort_Undefer,                   -- System.Soft_Links
     RE_Complete_Master,                 -- System.Soft_Links
     RE_Current_Master,                  -- System.Soft_Links
     RE_Enter_Master,                    -- System.Soft_Links
     RE_Get_Current_Excep,               -- System.Soft_Links
     RE_Get_GNAT_Exception,              -- System.Soft_Links
     RE_Update_Exception,                -- System.Soft_Links

     RE_ATSD,                            -- System.Threads
     RE_Thread_Body_Enter,               -- System.Threads
     RE_Thread_Body_Exceptional_Exit,    -- System.Threads
     RE_Thread_Body_Leave,               -- System.Threads

     RE_Bits_1,                          -- System.Unsigned_Types
     RE_Bits_2,                          -- System.Unsigned_Types
     RE_Bits_4,                          -- System.Unsigned_Types
     RE_Float_Unsigned,                  -- System.Unsigned_Types
     RE_Long_Unsigned,                   -- System.Unsigned_Types
     RE_Long_Long_Unsigned,              -- System.Unsigned_Types
     RE_Packed_Byte,                     -- System.Unsigned_Types
     RE_Packed_Bytes1,                   -- System.Unsigned_Types
     RE_Packed_Bytes2,                   -- System.Unsigned_Types
     RE_Packed_Bytes4,                   -- System.Unsigned_Types
     RE_Short_Unsigned,                  -- System.Unsigned_Types
     RE_Short_Short_Unsigned,            -- System.Unsigned_Types
     RE_Unsigned,                        -- System.Unsigned_Types

     RE_Value_Boolean,                   -- System.Val_Bool

     RE_Value_Character,                 -- System.Val_Char

     RE_Value_Decimal,                   -- System.Val_Dec

     RE_Value_Enumeration_8,             -- System.Val_Enum
     RE_Value_Enumeration_16,            -- System.Val_Enum
     RE_Value_Enumeration_32,            -- System.Val_Enum

     RE_Value_Integer,                   -- System.Val_Int

     RE_Value_Long_Long_Decimal,         -- System.Val_LLD

     RE_Value_Long_Long_Integer,         -- System.Val_LLI

     RE_Value_Long_Long_Unsigned,        -- System.Val_LLU

     RE_Value_Real,                      -- System.Val_Real

     RE_Value_Unsigned,                  -- System.Val_Uns

     RE_Value_Wide_Character,            -- System.Val_WChar
     RE_Value_Wide_Wide_Character,       -- System.Val_WChar

     RE_D,                               -- System.Vax_Float_Operations
     RE_F,                               -- System.Vax_Float_Operations
     RE_G,                               -- System.Vax_Float_Operations
     RE_Q,                               -- System.Vax_Float_Operations
     RE_S,                               -- System.Vax_Float_Operations
     RE_T,                               -- System.Vax_Float_Operations

     RE_D_To_G,                          -- System.Vax_Float_Operations
     RE_F_To_G,                          -- System.Vax_Float_Operations
     RE_F_To_Q,                          -- System.Vax_Float_Operations
     RE_F_To_S,                          -- System.Vax_Float_Operations
     RE_G_To_D,                          -- System.Vax_Float_Operations
     RE_G_To_F,                          -- System.Vax_Float_Operations
     RE_G_To_Q,                          -- System.Vax_Float_Operations
     RE_G_To_T,                          -- System.Vax_Float_Operations
     RE_Q_To_F,                          -- System.Vax_Float_Operations
     RE_Q_To_G,                          -- System.Vax_Float_Operations
     RE_S_To_F,                          -- System.Vax_Float_Operations
     RE_T_To_D,                          -- System.Vax_Float_Operations
     RE_T_To_G,                          -- System.Vax_Float_Operations

     RE_Abs_F,                           -- System.Vax_Float_Operations
     RE_Abs_G,                           -- System.Vax_Float_Operations
     RE_Add_F,                           -- System.Vax_Float_Operations
     RE_Add_G,                           -- System.Vax_Float_Operations
     RE_Div_F,                           -- System.Vax_Float_Operations
     RE_Div_G,                           -- System.Vax_Float_Operations
     RE_Mul_F,                           -- System.Vax_Float_Operations
     RE_Mul_G,                           -- System.Vax_Float_Operations
     RE_Neg_F,                           -- System.Vax_Float_Operations
     RE_Neg_G,                           -- System.Vax_Float_Operations
     RE_Sub_F,                           -- System.Vax_Float_Operations
     RE_Sub_G,                           -- System.Vax_Float_Operations

     RE_Eq_F,                            -- System.Vax_Float_Operations
     RE_Eq_G,                            -- System.Vax_Float_Operations
     RE_Le_F,                            -- System.Vax_Float_Operations
     RE_Le_G,                            -- System.Vax_Float_Operations
     RE_Lt_F,                            -- System.Vax_Float_Operations
     RE_Lt_G,                            -- System.Vax_Float_Operations
     RE_Ne_F,                            -- System.Vax_Float_Operations
     RE_Ne_G,                            -- System.Vax_Float_Operations

     RE_Valid_D,                         -- System.Vax_Float_Operations
     RE_Valid_F,                         -- System.Vax_Float_Operations
     RE_Valid_G,                         -- System.Vax_Float_Operations

     RE_Version_String,                  -- System.Version_Control
     RE_Get_Version_String,              -- System.Version_Control

     RE_Register_VMS_Exception,          -- System.VMS_Exception_Table

     RE_String_To_Wide_String,           -- System.WCh_StW
     RE_String_To_Wide_Wide_String,      -- System.WCh_StW

     RE_Wide_String_To_String,           -- System.WCh_WtS
     RE_Wide_Wide_String_To_String,      -- System.WCh_WtS

     RE_Wide_Width_Character,            -- System.WWd_Char
     RE_Wide_Wide_Width_Character,       -- System.WWd_Char

     RE_Wide_Wide_Width_Enumeration_8,   -- System.WWd_Enum
     RE_Wide_Wide_Width_Enumeration_16,  -- System.WWd_Enum
     RE_Wide_Wide_Width_Enumeration_32,  -- System.WWd_Enum

     RE_Wide_Width_Enumeration_8,        -- System.WWd_Enum
     RE_Wide_Width_Enumeration_16,       -- System.WWd_Enum
     RE_Wide_Width_Enumeration_32,       -- System.WWd_Enum

     RE_Wide_Wide_Width_Wide_Character,  -- System.WWd_Wchar
     RE_Wide_Wide_Width_Wide_Wide_Char,  -- System.WWd_Wchar
     RE_Wide_Width_Wide_Character,       -- System.WWd_Wchar
     RE_Wide_Width_Wide_Wide_Character,  -- System.WWd_Wchar

     RE_Width_Boolean,                   -- System.Wid_Bool

     RE_Width_Character,                 -- System.Wid_Char

     RE_Width_Enumeration_8,             -- System.Wid_Enum
     RE_Width_Enumeration_16,            -- System.Wid_Enum
     RE_Width_Enumeration_32,            -- System.Wid_Enum

     RE_Width_Long_Long_Integer,         -- System.Wid_LLI

     RE_Width_Long_Long_Unsigned,        -- System.Wid_LLU

     RE_Width_Wide_Character,            -- System.Wid_WChar
     RE_Width_Wide_Wide_Character,       -- System.Wid_WChar

     RE_Protected_Entry_Body_Array,      -- Tasking.Protected_Objects.Entries
     RE_Protection_Entries,              -- Tasking.Protected_Objects.Entries
     RE_Initialize_Protection_Entries,   -- Tasking.Protected_Objects.Entries
     RE_Lock_Entries,                    -- Tasking.Protected_Objects.Entries
     RE_Lock_Read_Only_Entries,          -- Tasking.Protected_Objects.Entries
     RE_Unlock_Entries,                  -- Tasking.Protected_Objects.Entries
     RE_Communication_Block,             -- Protected_Objects.Operations
     RE_Protected_Entry_Call,            -- Protected_Objects.Operations
     RE_Service_Entries,                 -- Protected_Objects.Operations
     RE_Cancel_Protected_Entry_Call,     -- Protected_Objects.Operations
     RE_Enqueued,                        -- Protected_Objects.Operations
     RE_Cancelled,                       -- Protected_Objects.Operations
     RE_Complete_Entry_Body,             -- Protected_Objects.Operations
     RE_Exceptional_Complete_Entry_Body, -- Protected_Objects.Operations
     RE_Requeue_Protected_Entry,         -- Protected_Objects.Operations
     RE_Requeue_Task_To_Protected_Entry, -- Protected_Objects.Operations
     RE_Protected_Count,                 -- Protected_Objects.Operations
     RE_Protected_Entry_Caller,          -- Protected_Objects.Operations
     RE_Timed_Protected_Entry_Call,      -- Protected_Objects.Operations

     RE_Protection_Entry,                -- Protected_Objects.Single_Entry
     RE_Initialize_Protection_Entry,     -- Protected_Objects.Single_Entry
     RE_Lock_Entry,                      -- Protected_Objects.Single_Entry
     RE_Lock_Read_Only_Entry,            -- Protected_Objects.Single_Entry
     RE_Unlock_Entry,                    -- Protected_Objects.Single_Entry
     RE_Protected_Single_Entry_Call,     -- Protected_Objects.Single_Entry
     RE_Service_Entry,                   -- Protected_Objects.Single_Entry
     RE_Complete_Single_Entry_Body,      -- Protected_Objects.Single_Entry
     RE_Exceptional_Complete_Single_Entry_Body,
     RE_Protected_Count_Entry,           -- Protected_Objects.Single_Entry
     RE_Protected_Single_Entry_Caller,   -- Protected_Objects.Single_Entry
     RE_Timed_Protected_Single_Entry_Call,

     RE_Protected_Entry_Index,           -- System.Tasking.Protected_Objects
     RE_Entry_Body,                      -- System.Tasking.Protected_Objects
     RE_Protection,                      -- System.Tasking.Protected_Objects
     RE_Initialize_Protection,           -- System.Tasking.Protected_Objects
     RE_Finalize_Protection,             -- System.Tasking.Protected_Objects
     RE_Lock,                            -- System.Tasking.Protected_Objects
     RE_Lock_Read_Only,                  -- System.Tasking.Protected_Objects
     RE_Unlock,                          -- System.Tasking.Protected_Objects

     RE_Delay_Block,                     -- System.Tasking.Async_Delays
     RE_Timed_Out,                       -- System.Tasking.Async_Delays
     RE_Cancel_Async_Delay,              -- System.Tasking.Async_Delays
     RE_Enqueue_Duration,                -- System.Tasking.Async_Delays
     RE_Enqueue_Calendar,                -- System.Tasking.Async_Delays
     RE_Enqueue_RT,                      -- System.Tasking.Async_Delays

     RE_Accept_Call,                     -- System.Tasking.Rendezvous
     RE_Accept_Trivial,                  -- System.Tasking.Rendezvous
     RE_Callable,                        -- System.Tasking.Rendezvous
     RE_Call_Simple,                     -- System.Tasking.Rendezvous
     RE_Requeue_Task_Entry,              -- System.Tasking.Rendezvous
     RE_Requeue_Protected_To_Task_Entry, -- System.Tasking.Rendezvous
     RE_Cancel_Task_Entry_Call,          -- System.Tasking.Rendezvous
     RE_Complete_Rendezvous,             -- System.Tasking.Rendezvous
     RE_Task_Count,                      -- System.Tasking.Rendezvous
     RE_Exceptional_Complete_Rendezvous, -- System.Tasking.Rendezvous
     RE_Selective_Wait,                  -- System.Tasking.Rendezvous
     RE_Task_Entry_Call,                 -- System.Tasking.Rendezvous
     RE_Task_Entry_Caller,               -- System.Tasking.Rendezvous
     RE_Timed_Task_Entry_Call,           -- System.Tasking.Rendezvous
     RE_Timed_Selective_Wait,            -- System.Tasking.Rendezvous

     RE_Activate_Restricted_Tasks,       -- System.Tasking.Restricted.Stages
     RE_Complete_Restricted_Activation,  -- System.Tasking.Restricted.Stages
     RE_Create_Restricted_Task,          -- System.Tasking.Restricted.Stages
     RE_Complete_Restricted_Task,        -- System.Tasking.Restricted.Stages
     RE_Restricted_Terminated,           -- System.Tasking.Restricted.Stages

     RE_Abort_Tasks,                     -- System.Tasking.Stages
     RE_Activate_Tasks,                  -- System.Tasking.Stages
     RE_Complete_Activation,             -- System.Tasking.Stages
     RE_Create_Task,                     -- System.Tasking.Stages
     RE_Complete_Task,                   -- System.Tasking.Stages
     RE_Free_Task,                       -- System.Tasking.Stages
     RE_Expunge_Unactivated_Tasks,       -- System.Tasking.Stages
     RE_Terminated);                     -- System.Tasking.Stages

   --  The following declarations build a table that is indexed by the
   --  RTE function to determine the unit containing the given entity.
   --  This table is sorted in order of package names.

   RE_Unit_Table : array (RE_Id) of RTU_Id := (

     RE_Null                             => RTU_Null,

     RE_Exceptions_Available_In_HIE      => Ada_Exceptions,
     RE_Code_Loc                         => Ada_Exceptions,
     RE_Current_Target_Exception         => Ada_Exceptions, -- of JGNAT
     RE_Exception_Id                     => Ada_Exceptions,
     RE_Exception_Information            => Ada_Exceptions,
     RE_Exception_Message                => Ada_Exceptions,
     RE_Exception_Name_Simple            => Ada_Exceptions,
     RE_Exception_Occurrence             => Ada_Exceptions,
     RE_Null_Id                          => Ada_Exceptions,
     RE_Null_Occurrence                  => Ada_Exceptions,
     RE_Poll                             => Ada_Exceptions,
     RE_Raise_Exception                  => Ada_Exceptions,
     RE_Raise_Exception_Always           => Ada_Exceptions,
     RE_Reraise_Occurrence               => Ada_Exceptions,
     RE_Reraise_Occurrence_Always        => Ada_Exceptions,
     RE_Reraise_Occurrence_No_Defer      => Ada_Exceptions,
     RE_Save_Occurrence                  => Ada_Exceptions,

     RE_Simple_List_Controller           => Ada_Finalization_List_Controller,
     RE_List_Controller                  => Ada_Finalization_List_Controller,

     RE_Interrupt_ID                     => Ada_Interrupts,
     RE_Is_Reserved                      => Ada_Interrupts,
     RE_Is_Attached                      => Ada_Interrupts,
     RE_Current_Handler                  => Ada_Interrupts,
     RE_Attach_Handler                   => Ada_Interrupts,
     RE_Exchange_Handler                 => Ada_Interrupts,
     RE_Detach_Handler                   => Ada_Interrupts,
     RE_Reference                        => Ada_Interrupts,

     RE_Names                            => Ada_Interrupts_Names,

     RE_Root_Stream_Type                 => Ada_Streams,
     RE_Stream_Element                   => Ada_Streams,
     RE_Stream_Element_Count             => Ada_Streams,
     RE_Stream_Element_Offset            => Ada_Streams,
     RE_Stream_Element_Array             => Ada_Streams,

     RE_Stream_Access                    => Ada_Streams_Stream_IO,

     RE_Abstract_Interface               => Ada_Tags,
     RE_Addr_Ptr                         => Ada_Tags,
     RE_Address_Array                    => Ada_Tags,
     RE_CW_Membership                    => Ada_Tags,
     RE_Descendant_Tag                   => Ada_Tags,
     RE_Displace                         => Ada_Tags,
     RE_DT_Entry_Size                    => Ada_Tags,
     RE_DT_Prologue_Size                 => Ada_Tags,
     RE_External_Tag                     => Ada_Tags,
     RE_Get_Access_Level                 => Ada_Tags,
     RE_Get_Entry_Index                  => Ada_Tags,
     RE_Get_External_Tag                 => Ada_Tags,
     RE_Get_Offset_Index                 => Ada_Tags,
     RE_Get_Prim_Op_Address              => Ada_Tags,
     RE_Get_Prim_Op_Kind                 => Ada_Tags,
     RE_Get_RC_Offset                    => Ada_Tags,
     RE_Get_Remotely_Callable            => Ada_Tags,
     RE_Get_Tagged_Kind                  => Ada_Tags,
     RE_Inherit_DT                       => Ada_Tags,
     RE_Inherit_TSD                      => Ada_Tags,
     RE_Interface_Data                   => Ada_Tags,
     RE_Interface_Tag                    => Ada_Tags,
     RE_Internal_Tag                     => Ada_Tags,
     RE_Is_Descendant_At_Same_Level      => Ada_Tags,
     RE_IW_Membership                    => Ada_Tags,
     RE_Object_Specific_Data             => Ada_Tags,
     RE_Offset_To_Top                    => Ada_Tags,
     RE_POK_Function                     => Ada_Tags,
     RE_POK_Procedure                    => Ada_Tags,
     RE_POK_Protected_Entry              => Ada_Tags,
     RE_POK_Protected_Function           => Ada_Tags,
     RE_POK_Protected_Procedure          => Ada_Tags,
     RE_POK_Task_Entry                   => Ada_Tags,
     RE_POK_Task_Function                => Ada_Tags,
     RE_POK_Task_Procedure               => Ada_Tags,
     RE_Prim_Op_Kind                     => Ada_Tags,
     RE_Primary_DT                       => Ada_Tags,
     RE_Register_Interface_Tag           => Ada_Tags,
     RE_Register_Tag                     => Ada_Tags,
     RE_Secondary_DT                     => Ada_Tags,
     RE_Select_Specific_Data             => Ada_Tags,
     RE_Set_Access_Level                 => Ada_Tags,
     RE_Set_Entry_Index                  => Ada_Tags,
     RE_Set_Expanded_Name                => Ada_Tags,
     RE_Set_External_Tag                 => Ada_Tags,
     RE_Set_Interface_Table              => Ada_Tags,
     RE_Set_Num_Prim_Ops                 => Ada_Tags,
     RE_Set_Offset_Index                 => Ada_Tags,
     RE_Set_Offset_To_Top                => Ada_Tags,
     RE_Set_OSD                          => Ada_Tags,
     RE_Set_Prim_Op_Address              => Ada_Tags,
     RE_Set_Prim_Op_Kind                 => Ada_Tags,
     RE_Set_RC_Offset                    => Ada_Tags,
     RE_Set_Remotely_Callable            => Ada_Tags,
     RE_Set_SSD                          => Ada_Tags,
     RE_Set_Tagged_Kind                  => Ada_Tags,
     RE_Set_TSD                          => Ada_Tags,
     RE_Tag                              => Ada_Tags,
     RE_Tag_Error                        => Ada_Tags,
     RE_Tagged_Kind                      => Ada_Tags,
     RE_TSD_Entry_Size                   => Ada_Tags,
     RE_TSD_Prologue_Size                => Ada_Tags,
     RE_TK_Abstract_Limited_Tagged       => Ada_Tags,
     RE_TK_Abstract_Tagged               => Ada_Tags,
     RE_TK_Limited_Tagged                => Ada_Tags,
     RE_TK_Protected                     => Ada_Tags,
     RE_TK_Tagged                        => Ada_Tags,
     RE_TK_Task                          => Ada_Tags,
     RE_Valid_Signature                  => Ada_Tags,

     RE_Abort_Task                       => Ada_Task_Identification,
     RE_Current_Task                     => Ada_Task_Identification,
     RO_AT_Task_Id                       => Ada_Task_Identification,

     RO_CA_Time                          => Ada_Calendar,
     RO_CA_Delay_For                     => Ada_Calendar_Delays,
     RO_CA_Delay_Until                   => Ada_Calendar_Delays,
     RO_CA_To_Duration                   => Ada_Calendar_Delays,

     RO_RT_Time                          => Ada_Real_Time,
     RO_RT_Delay_Until                   => Ada_Real_Time_Delays,
     RO_RT_To_Duration                   => Ada_Real_Time_Delays,

     RE_Integer_64                       => Interfaces,
     RE_Unsigned_8                       => Interfaces,
     RE_Unsigned_16                      => Interfaces,
     RE_Unsigned_32                      => Interfaces,
     RE_Unsigned_64                      => Interfaces,

     RE_Vtable_Ptr                       => Interfaces_CPP,
     RE_Displaced_This                   => Interfaces_CPP,
     RE_CPP_CW_Membership                => Interfaces_CPP,
     RE_CPP_DT_Entry_Size                => Interfaces_CPP,
     RE_CPP_DT_Prologue_Size             => Interfaces_CPP,
     RE_CPP_Get_External_Tag             => Interfaces_CPP,
     RE_CPP_Get_Prim_Op_Address          => Interfaces_CPP,
     RE_CPP_Get_RC_Offset                => Interfaces_CPP,
     RE_CPP_Get_Remotely_Callable        => Interfaces_CPP,
     RE_CPP_Inherit_DT                   => Interfaces_CPP,
     RE_CPP_Inherit_TSD                  => Interfaces_CPP,
     RE_CPP_Register_Tag                 => Interfaces_CPP,
     RE_CPP_Set_Expanded_Name            => Interfaces_CPP,
     RE_CPP_Set_External_Tag             => Interfaces_CPP,
     RE_CPP_Set_Prim_Op_Address          => Interfaces_CPP,
     RE_CPP_Set_RC_Offset                => Interfaces_CPP,
     RE_CPP_Set_Remotely_Callable        => Interfaces_CPP,
     RE_CPP_Set_TSD                      => Interfaces_CPP,
     RE_CPP_TSD_Entry_Size               => Interfaces_CPP,
     RE_CPP_TSD_Prologue_Size            => Interfaces_CPP,

     RE_Packed_Size                      => Interfaces_Packed_Decimal,
     RE_Packed_To_Int32                  => Interfaces_Packed_Decimal,
     RE_Packed_To_Int64                  => Interfaces_Packed_Decimal,
     RE_Int32_To_Packed                  => Interfaces_Packed_Decimal,
     RE_Int64_To_Packed                  => Interfaces_Packed_Decimal,

     RE_Address                          => System,
     RE_Any_Priority                     => System,
     RE_Bit_Order                        => System,
     RE_Default_Priority                 => System,
     RE_High_Order_First                 => System,
     RE_Interrupt_Priority               => System,
     RE_Lib_Stop                         => System,
     RE_Low_Order_First                  => System,
     RE_Max_Interrupt_Priority           => System,
     RE_Max_Priority                     => System,
     RE_Null_Address                     => System,
     RE_Priority                         => System,

     RE_Add_With_Ovflo_Check             => System_Arith_64,
     RE_Double_Divide                    => System_Arith_64,
     RE_Multiply_With_Ovflo_Check        => System_Arith_64,
     RE_Scaled_Divide                    => System_Arith_64,
     RE_Subtract_With_Ovflo_Check        => System_Arith_64,

     RE_Create_AST_Handler               => System_AST_Handling,

     RE_Raise_Assert_Failure             => System_Assertions,

     RE_AST_Handler                      => System_Aux_DEC,
     RE_Import_Value                     => System_Aux_DEC,
     RE_No_AST_Handler                   => System_Aux_DEC,
     RE_Type_Class                       => System_Aux_DEC,
     RE_Type_Class_Enumeration           => System_Aux_DEC,
     RE_Type_Class_Integer               => System_Aux_DEC,
     RE_Type_Class_Fixed_Point           => System_Aux_DEC,
     RE_Type_Class_Floating_Point        => System_Aux_DEC,
     RE_Type_Class_Array                 => System_Aux_DEC,
     RE_Type_Class_Record                => System_Aux_DEC,
     RE_Type_Class_Access                => System_Aux_DEC,
     RE_Type_Class_Task                  => System_Aux_DEC,
     RE_Type_Class_Address               => System_Aux_DEC,

     RE_Bit_And                          => System_Bit_Ops,
     RE_Bit_Eq                           => System_Bit_Ops,
     RE_Bit_Not                          => System_Bit_Ops,
     RE_Bit_Or                           => System_Bit_Ops,
     RE_Bit_Xor                          => System_Bit_Ops,

     RE_Checked_Pool                     => System_Checked_Pools,

     RE_Boolean_Array                    => System_Boolean_Array_Operations,
     RE_Vector_Not                       => System_Boolean_Array_Operations,
     RE_Vector_And                       => System_Boolean_Array_Operations,
     RE_Vector_Or                        => System_Boolean_Array_Operations,
     RE_Vector_Nand                      => System_Boolean_Array_Operations,
     RE_Vector_Nor                       => System_Boolean_Array_Operations,
     RE_Vector_Nxor                      => System_Boolean_Array_Operations,
     RE_Vector_Xor                       => System_Boolean_Array_Operations,

     RE_Compare_Array_S8                 => System_Compare_Array_Signed_8,
     RE_Compare_Array_S8_Unaligned       => System_Compare_Array_Signed_8,

     RE_Compare_Array_S16                => System_Compare_Array_Signed_16,

     RE_Compare_Array_S32                => System_Compare_Array_Signed_32,

     RE_Compare_Array_S64                => System_Compare_Array_Signed_64,

     RE_Compare_Array_U8                 => System_Compare_Array_Unsigned_8,
     RE_Compare_Array_U8_Unaligned       => System_Compare_Array_Unsigned_8,

     RE_Compare_Array_U16                => System_Compare_Array_Unsigned_16,

     RE_Compare_Array_U32                => System_Compare_Array_Unsigned_32,

     RE_Compare_Array_U64                => System_Compare_Array_Unsigned_64,

     RE_Register_Exception               => System_Exception_Table,

     RE_Exn_Integer                      => System_Exn_Int,

     RE_Exn_Long_Long_Float              => System_Exn_LLF,

     RE_Exn_Long_Long_Integer            => System_Exn_LLI,

     RE_Exp_Integer                      => System_Exp_Int,

     RE_Exp_Long_Long_Integer            => System_Exp_LLI,

     RE_Exp_Long_Long_Unsigned           => System_Exp_LLU,

     RE_Exp_Modular                      => System_Exp_Mod,

     RE_Exp_Unsigned                     => System_Exp_Uns,

     RE_Attr_Float                       => System_Fat_Flt,

     RE_Attr_IEEE_Long                   => System_Fat_IEEE_Long_Float,
     RE_Fat_IEEE_Long                    => System_Fat_IEEE_Long_Float,

     RE_Attr_IEEE_Short                  => System_Fat_IEEE_Short_Float,
     RE_Fat_IEEE_Short                   => System_Fat_IEEE_Short_Float,

     RE_Attr_Long_Float                  => System_Fat_LFlt,

     RE_Attr_Long_Long_Float             => System_Fat_LLF,

     RE_Attr_Short_Float                 => System_Fat_SFlt,

     RE_Attr_VAX_D_Float                 => System_Fat_VAX_D_Float,
     RE_Fat_VAX_D                        => System_Fat_VAX_D_Float,

     RE_Attr_VAX_F_Float                 => System_Fat_VAX_F_Float,
     RE_Fat_VAX_F                        => System_Fat_VAX_F_Float,

     RE_Attr_VAX_G_Float                 => System_Fat_VAX_G_Float,
     RE_Fat_VAX_G                        => System_Fat_VAX_G_Float,

     RE_Attach_To_Final_List             => System_Finalization_Implementation,
     RE_Finalize_List                    => System_Finalization_Implementation,
     RE_Finalize_One                     => System_Finalization_Implementation,
     RE_Global_Final_List                => System_Finalization_Implementation,
     RE_Record_Controller                => System_Finalization_Implementation,
     RE_Limited_Record_Controller        => System_Finalization_Implementation,
     RE_Deep_Tag_Initialize              => System_Finalization_Implementation,
     RE_Deep_Tag_Adjust                  => System_Finalization_Implementation,
     RE_Deep_Tag_Finalize                => System_Finalization_Implementation,
     RE_Deep_Tag_Attach                  => System_Finalization_Implementation,
     RE_Deep_Rec_Initialize              => System_Finalization_Implementation,
     RE_Deep_Rec_Adjust                  => System_Finalization_Implementation,
     RE_Deep_Rec_Finalize                => System_Finalization_Implementation,

     RE_Root_Controlled                  => System_Finalization_Root,
     RE_Finalizable                      => System_Finalization_Root,
     RE_Finalizable_Ptr                  => System_Finalization_Root,

     RE_Fore                             => System_Fore,

     RE_Image_Boolean                    => System_Img_Bool,

     RE_Image_Character                  => System_Img_Char,

     RE_Image_Decimal                    => System_Img_Dec,

     RE_Image_Enumeration_8              => System_Img_Enum,
     RE_Image_Enumeration_16             => System_Img_Enum,
     RE_Image_Enumeration_32             => System_Img_Enum,

     RE_Image_Integer                    => System_Img_Int,

     RE_Image_Long_Long_Decimal          => System_Img_LLD,

     RE_Image_Long_Long_Integer          => System_Img_LLI,

     RE_Image_Long_Long_Unsigned         => System_Img_LLU,

     RE_Image_Ordinary_Fixed_Point       => System_Img_Real,
     RE_Image_Floating_Point             => System_Img_Real,

     RE_Image_Unsigned                   => System_Img_Uns,

     RE_Image_Wide_Character             => System_Img_WChar,
     RE_Image_Wide_Wide_Character        => System_Img_WChar,

     RE_Bind_Interrupt_To_Entry          => System_Interrupts,
     RE_Default_Interrupt_Priority       => System_Interrupts,
     RE_Dynamic_Interrupt_Protection     => System_Interrupts,
     RE_Install_Handlers                 => System_Interrupts,
     RE_Register_Interrupt_Handler       => System_Interrupts,
     RE_Static_Interrupt_Protection      => System_Interrupts,
     RE_System_Interrupt_Id              => System_Interrupts,

     RE_Asm_Insn                         => System_Machine_Code,
     RE_Asm_Input_Operand                => System_Machine_Code,
     RE_Asm_Output_Operand               => System_Machine_Code,

     RE_Mantissa_Value                   => System_Mantissa,

     RE_memcpy                           => System_Memcop,
     RE_memmove                          => System_Memcop,

     RE_Bits_03                          => System_Pack_03,
     RE_Get_03                           => System_Pack_03,
     RE_Set_03                           => System_Pack_03,

     RE_Bits_05                          => System_Pack_05,
     RE_Get_05                           => System_Pack_05,
     RE_Set_05                           => System_Pack_05,

     RE_Bits_06                          => System_Pack_06,
     RE_Get_06                           => System_Pack_06,
     RE_GetU_06                          => System_Pack_06,
     RE_Set_06                           => System_Pack_06,
     RE_SetU_06                          => System_Pack_06,

     RE_Bits_07                          => System_Pack_07,
     RE_Get_07                           => System_Pack_07,
     RE_Set_07                           => System_Pack_07,

     RE_Bits_09                          => System_Pack_09,
     RE_Get_09                           => System_Pack_09,
     RE_Set_09                           => System_Pack_09,

     RE_Bits_10                          => System_Pack_10,
     RE_Get_10                           => System_Pack_10,
     RE_GetU_10                          => System_Pack_10,
     RE_Set_10                           => System_Pack_10,
     RE_SetU_10                          => System_Pack_10,

     RE_Bits_11                          => System_Pack_11,
     RE_Get_11                           => System_Pack_11,
     RE_Set_11                           => System_Pack_11,

     RE_Bits_12                          => System_Pack_12,
     RE_Get_12                           => System_Pack_12,
     RE_GetU_12                          => System_Pack_12,
     RE_Set_12                           => System_Pack_12,
     RE_SetU_12                          => System_Pack_12,

     RE_Bits_13                          => System_Pack_13,
     RE_Get_13                           => System_Pack_13,
     RE_Set_13                           => System_Pack_13,

     RE_Bits_14                          => System_Pack_14,
     RE_Get_14                           => System_Pack_14,
     RE_GetU_14                          => System_Pack_14,
     RE_Set_14                           => System_Pack_14,
     RE_SetU_14                          => System_Pack_14,

     RE_Bits_15                          => System_Pack_15,
     RE_Get_15                           => System_Pack_15,
     RE_Set_15                           => System_Pack_15,

     RE_Bits_17                          => System_Pack_17,
     RE_Get_17                           => System_Pack_17,
     RE_Set_17                           => System_Pack_17,

     RE_Bits_18                          => System_Pack_18,
     RE_Get_18                           => System_Pack_18,
     RE_GetU_18                          => System_Pack_18,
     RE_Set_18                           => System_Pack_18,
     RE_SetU_18                          => System_Pack_18,

     RE_Bits_19                          => System_Pack_19,
     RE_Get_19                           => System_Pack_19,
     RE_Set_19                           => System_Pack_19,

     RE_Bits_20                          => System_Pack_20,
     RE_Get_20                           => System_Pack_20,
     RE_GetU_20                          => System_Pack_20,
     RE_Set_20                           => System_Pack_20,
     RE_SetU_20                          => System_Pack_20,

     RE_Bits_21                          => System_Pack_21,
     RE_Get_21                           => System_Pack_21,
     RE_Set_21                           => System_Pack_21,

     RE_Bits_22                          => System_Pack_22,
     RE_Get_22                           => System_Pack_22,
     RE_GetU_22                          => System_Pack_22,
     RE_Set_22                           => System_Pack_22,
     RE_SetU_22                          => System_Pack_22,

     RE_Bits_23                          => System_Pack_23,
     RE_Get_23                           => System_Pack_23,
     RE_Set_23                           => System_Pack_23,

     RE_Bits_24                          => System_Pack_24,
     RE_Get_24                           => System_Pack_24,
     RE_GetU_24                          => System_Pack_24,
     RE_Set_24                           => System_Pack_24,
     RE_SetU_24                          => System_Pack_24,

     RE_Bits_25                          => System_Pack_25,
     RE_Get_25                           => System_Pack_25,
     RE_Set_25                           => System_Pack_25,

     RE_Bits_26                          => System_Pack_26,
     RE_Get_26                           => System_Pack_26,
     RE_GetU_26                          => System_Pack_26,
     RE_Set_26                           => System_Pack_26,
     RE_SetU_26                          => System_Pack_26,

     RE_Bits_27                          => System_Pack_27,
     RE_Get_27                           => System_Pack_27,
     RE_Set_27                           => System_Pack_27,

     RE_Bits_28                          => System_Pack_28,
     RE_Get_28                           => System_Pack_28,
     RE_GetU_28                          => System_Pack_28,
     RE_Set_28                           => System_Pack_28,
     RE_SetU_28                          => System_Pack_28,

     RE_Bits_29                          => System_Pack_29,
     RE_Get_29                           => System_Pack_29,
     RE_Set_29                           => System_Pack_29,

     RE_Bits_30                          => System_Pack_30,
     RE_Get_30                           => System_Pack_30,
     RE_GetU_30                          => System_Pack_30,
     RE_Set_30                           => System_Pack_30,
     RE_SetU_30                          => System_Pack_30,

     RE_Bits_31                          => System_Pack_31,
     RE_Get_31                           => System_Pack_31,
     RE_Set_31                           => System_Pack_31,

     RE_Bits_33                          => System_Pack_33,
     RE_Get_33                           => System_Pack_33,
     RE_Set_33                           => System_Pack_33,

     RE_Bits_34                          => System_Pack_34,
     RE_Get_34                           => System_Pack_34,
     RE_GetU_34                          => System_Pack_34,
     RE_Set_34                           => System_Pack_34,
     RE_SetU_34                          => System_Pack_34,

     RE_Bits_35                          => System_Pack_35,
     RE_Get_35                           => System_Pack_35,
     RE_Set_35                           => System_Pack_35,

     RE_Bits_36                          => System_Pack_36,
     RE_Get_36                           => System_Pack_36,
     RE_GetU_36                          => System_Pack_36,
     RE_Set_36                           => System_Pack_36,
     RE_SetU_36                          => System_Pack_36,

     RE_Bits_37                          => System_Pack_37,
     RE_Get_37                           => System_Pack_37,
     RE_Set_37                           => System_Pack_37,

     RE_Bits_38                          => System_Pack_38,
     RE_Get_38                           => System_Pack_38,
     RE_GetU_38                          => System_Pack_38,
     RE_Set_38                           => System_Pack_38,
     RE_SetU_38                          => System_Pack_38,

     RE_Bits_39                          => System_Pack_39,
     RE_Get_39                           => System_Pack_39,
     RE_Set_39                           => System_Pack_39,

     RE_Bits_40                          => System_Pack_40,
     RE_Get_40                           => System_Pack_40,
     RE_GetU_40                          => System_Pack_40,
     RE_Set_40                           => System_Pack_40,
     RE_SetU_40                          => System_Pack_40,

     RE_Bits_41                          => System_Pack_41,
     RE_Get_41                           => System_Pack_41,
     RE_Set_41                           => System_Pack_41,

     RE_Bits_42                          => System_Pack_42,
     RE_Get_42                           => System_Pack_42,
     RE_GetU_42                          => System_Pack_42,
     RE_Set_42                           => System_Pack_42,
     RE_SetU_42                          => System_Pack_42,

     RE_Bits_43                          => System_Pack_43,
     RE_Get_43                           => System_Pack_43,
     RE_Set_43                           => System_Pack_43,

     RE_Bits_44                          => System_Pack_44,
     RE_Get_44                           => System_Pack_44,
     RE_GetU_44                          => System_Pack_44,
     RE_Set_44                           => System_Pack_44,
     RE_SetU_44                          => System_Pack_44,

     RE_Bits_45                          => System_Pack_45,
     RE_Get_45                           => System_Pack_45,
     RE_Set_45                           => System_Pack_45,

     RE_Bits_46                          => System_Pack_46,
     RE_Get_46                           => System_Pack_46,
     RE_GetU_46                          => System_Pack_46,
     RE_Set_46                           => System_Pack_46,
     RE_SetU_46                          => System_Pack_46,

     RE_Bits_47                          => System_Pack_47,
     RE_Get_47                           => System_Pack_47,
     RE_Set_47                           => System_Pack_47,

     RE_Bits_48                          => System_Pack_48,
     RE_Get_48                           => System_Pack_48,
     RE_GetU_48                          => System_Pack_48,
     RE_Set_48                           => System_Pack_48,
     RE_SetU_48                          => System_Pack_48,

     RE_Bits_49                          => System_Pack_49,
     RE_Get_49                           => System_Pack_49,
     RE_Set_49                           => System_Pack_49,

     RE_Bits_50                          => System_Pack_50,
     RE_Get_50                           => System_Pack_50,
     RE_GetU_50                          => System_Pack_50,
     RE_Set_50                           => System_Pack_50,
     RE_SetU_50                          => System_Pack_50,

     RE_Bits_51                          => System_Pack_51,
     RE_Get_51                           => System_Pack_51,
     RE_Set_51                           => System_Pack_51,

     RE_Bits_52                          => System_Pack_52,
     RE_Get_52                           => System_Pack_52,
     RE_GetU_52                          => System_Pack_52,
     RE_Set_52                           => System_Pack_52,
     RE_SetU_52                          => System_Pack_52,

     RE_Bits_53                          => System_Pack_53,
     RE_Get_53                           => System_Pack_53,
     RE_Set_53                           => System_Pack_53,

     RE_Bits_54                          => System_Pack_54,
     RE_Get_54                           => System_Pack_54,
     RE_GetU_54                          => System_Pack_54,
     RE_Set_54                           => System_Pack_54,
     RE_SetU_54                          => System_Pack_54,

     RE_Bits_55                          => System_Pack_55,
     RE_Get_55                           => System_Pack_55,
     RE_Set_55                           => System_Pack_55,

     RE_Bits_56                          => System_Pack_56,
     RE_Get_56                           => System_Pack_56,
     RE_GetU_56                          => System_Pack_56,
     RE_Set_56                           => System_Pack_56,
     RE_SetU_56                          => System_Pack_56,

     RE_Bits_57                          => System_Pack_57,
     RE_Get_57                           => System_Pack_57,
     RE_Set_57                           => System_Pack_57,

     RE_Bits_58                          => System_Pack_58,
     RE_Get_58                           => System_Pack_58,
     RE_GetU_58                          => System_Pack_58,
     RE_Set_58                           => System_Pack_58,
     RE_SetU_58                          => System_Pack_58,

     RE_Bits_59                          => System_Pack_59,
     RE_Get_59                           => System_Pack_59,
     RE_Set_59                           => System_Pack_59,

     RE_Bits_60                          => System_Pack_60,
     RE_Get_60                           => System_Pack_60,
     RE_GetU_60                          => System_Pack_60,
     RE_Set_60                           => System_Pack_60,
     RE_SetU_60                          => System_Pack_60,

     RE_Bits_61                          => System_Pack_61,
     RE_Get_61                           => System_Pack_61,
     RE_Set_61                           => System_Pack_61,

     RE_Bits_62                          => System_Pack_62,
     RE_Get_62                           => System_Pack_62,
     RE_GetU_62                          => System_Pack_62,
     RE_Set_62                           => System_Pack_62,
     RE_SetU_62                          => System_Pack_62,

     RE_Bits_63                          => System_Pack_63,
     RE_Get_63                           => System_Pack_63,
     RE_Set_63                           => System_Pack_63,

     RE_Adjust_Storage_Size              => System_Parameters,
     RE_Default_Stack_Size               => System_Parameters,
     RE_Garbage_Collected                => System_Parameters,
     RE_Size_Type                        => System_Parameters,
     RE_Unspecified_Size                 => System_Parameters,

     RE_DSA_Implementation               => System_Partition_Interface,
     RE_Get_Active_Partition_Id          => System_Partition_Interface,
     RE_Get_Passive_Partition_Id         => System_Partition_Interface,
     RE_Get_Local_Partition_Id           => System_Partition_Interface,
     RE_Get_RCI_Package_Receiver         => System_Partition_Interface,
     RE_Get_Unique_Remote_Pointer        => System_Partition_Interface,
     RE_RACW_Stub_Type                   => System_Partition_Interface,
     RE_RACW_Stub_Type_Access            => System_Partition_Interface,
     RE_RAS_Proxy_Type                   => System_Partition_Interface,
     RE_RAS_Proxy_Type_Access            => System_Partition_Interface,
     RE_Raise_Program_Error_Unknown_Tag  => System_Partition_Interface,
     RE_Register_Passive_Package         => System_Partition_Interface,
     RE_Register_Receiving_Stub          => System_Partition_Interface,
     RE_Request_Access                   => System_Partition_Interface,
     RE_RCI_Locator                      => System_Partition_Interface,
     RE_RCI_Subp_Info                    => System_Partition_Interface,
     RE_RCI_Subp_Info_Array              => System_Partition_Interface,
     RE_Same_Partition                   => System_Partition_Interface,
     RE_Subprogram_Id                    => System_Partition_Interface,
     RE_Get_RAS_Info                     => System_Partition_Interface,

     RE_To_PolyORB_String                => System_PolyORB_Interface,
     RE_To_Standard_String               => System_PolyORB_Interface,
     RE_Caseless_String_Eq               => System_PolyORB_Interface,
     RE_TypeCode                         => System_PolyORB_Interface,
     RE_Any                              => System_PolyORB_Interface,
     RE_Mode_In                          => System_PolyORB_Interface,
     RE_Mode_Out                         => System_PolyORB_Interface,
     RE_Mode_Inout                       => System_PolyORB_Interface,
     RE_NamedValue                       => System_PolyORB_Interface,
     RE_Result_Name                      => System_PolyORB_Interface,
     RE_Object_Ref                       => System_PolyORB_Interface,
     RE_Create_Any                       => System_PolyORB_Interface,
     RE_Any_Aggregate_Build              => System_PolyORB_Interface,
     RE_Add_Aggregate_Element            => System_PolyORB_Interface,
     RE_Get_Aggregate_Element            => System_PolyORB_Interface,
     RE_Content_Type                     => System_PolyORB_Interface,
     RE_Any_Member_Type                  => System_PolyORB_Interface,
     RE_Get_Nested_Sequence_Length       => System_PolyORB_Interface,
     RE_Extract_Union_Value              => System_PolyORB_Interface,
     RE_NVList_Ref                       => System_PolyORB_Interface,
     RE_NVList_Create                    => System_PolyORB_Interface,
     RE_NVList_Add_Item                  => System_PolyORB_Interface,
     RE_Request_Create                   => System_PolyORB_Interface,
     RE_Request_Invoke                   => System_PolyORB_Interface,
     RE_Request_Arguments                => System_PolyORB_Interface,
     RE_Request_Set_Out                  => System_PolyORB_Interface,
     RE_Request_Raise_Occurrence         => System_PolyORB_Interface,
     RE_Nil_Exc_List                     => System_PolyORB_Interface,
     RE_Servant                          => System_PolyORB_Interface,
     RE_Copy_Any_Value                   => System_PolyORB_Interface,
     RE_Set_Result                       => System_PolyORB_Interface,
     RE_Register_Obj_Receiving_Stub      => System_PolyORB_Interface,
     RE_Register_Pkg_Receiving_Stub      => System_PolyORB_Interface,
     RE_Is_Nil                           => System_PolyORB_Interface,
     RE_Entity_Ptr                       => System_PolyORB_Interface,
     RE_Entity_Of                        => System_PolyORB_Interface,
     RE_Inc_Usage                        => System_PolyORB_Interface,
     RE_Set_Ref                          => System_PolyORB_Interface,
     RE_Make_Ref                         => System_PolyORB_Interface,
     RE_Get_Local_Address                => System_PolyORB_Interface,
     RE_Get_Reference                    => System_PolyORB_Interface,
     RE_Local_Oid_To_Address             => System_PolyORB_Interface,
     RE_Asynchronous_P_To_Sync_Scope     => System_PolyORB_Interface,
     RE_Buffer_Stream_Type               => System_PolyORB_Interface,
     RE_Allocate_Buffer                  => System_PolyORB_Interface,
     RE_Release_Buffer                   => System_PolyORB_Interface,
     RE_BS_To_Any                        => System_PolyORB_Interface,
     RE_Any_To_BS                        => System_PolyORB_Interface,

     RE_FA_AD                            => System_PolyORB_Interface,
     RE_FA_AS                            => System_PolyORB_Interface,
     RE_FA_B                             => System_PolyORB_Interface,
     RE_FA_C                             => System_PolyORB_Interface,
     RE_FA_F                             => System_PolyORB_Interface,
     RE_FA_I                             => System_PolyORB_Interface,
     RE_FA_LF                            => System_PolyORB_Interface,
     RE_FA_LI                            => System_PolyORB_Interface,
     RE_FA_LLF                           => System_PolyORB_Interface,
     RE_FA_LLI                           => System_PolyORB_Interface,
     RE_FA_LLU                           => System_PolyORB_Interface,
     RE_FA_LU                            => System_PolyORB_Interface,
     RE_FA_SF                            => System_PolyORB_Interface,
     RE_FA_SI                            => System_PolyORB_Interface,
     RE_FA_SSI                           => System_PolyORB_Interface,
     RE_FA_SSU                           => System_PolyORB_Interface,
     RE_FA_SU                            => System_PolyORB_Interface,
     RE_FA_U                             => System_PolyORB_Interface,
     RE_FA_WC                            => System_PolyORB_Interface,
     RE_FA_WWC                           => System_PolyORB_Interface,
     RE_FA_String                        => System_PolyORB_Interface,
     RE_FA_ObjRef                        => System_PolyORB_Interface,

     RE_TA_AD                            => System_PolyORB_Interface,
     RE_TA_AS                            => System_PolyORB_Interface,
     RE_TA_B                             => System_PolyORB_Interface,
     RE_TA_C                             => System_PolyORB_Interface,
     RE_TA_F                             => System_PolyORB_Interface,
     RE_TA_I                             => System_PolyORB_Interface,
     RE_TA_LF                            => System_PolyORB_Interface,
     RE_TA_LI                            => System_PolyORB_Interface,
     RE_TA_LLF                           => System_PolyORB_Interface,
     RE_TA_LLI                           => System_PolyORB_Interface,
     RE_TA_LLU                           => System_PolyORB_Interface,
     RE_TA_LU                            => System_PolyORB_Interface,
     RE_TA_SF                            => System_PolyORB_Interface,
     RE_TA_SI                            => System_PolyORB_Interface,
     RE_TA_SSI                           => System_PolyORB_Interface,
     RE_TA_SSU                           => System_PolyORB_Interface,
     RE_TA_SU                            => System_PolyORB_Interface,
     RE_TA_U                             => System_PolyORB_Interface,
     RE_TA_WC                            => System_PolyORB_Interface,
     RE_TA_WWC                           => System_PolyORB_Interface,
     RE_TA_String                        => System_PolyORB_Interface,
     RE_TA_ObjRef                        => System_PolyORB_Interface,
     RE_TA_TC                            => System_PolyORB_Interface,

     RE_TC_Alias                         => System_PolyORB_Interface,
     RE_TC_Build                         => System_PolyORB_Interface,
     RE_Get_TC                           => System_PolyORB_Interface,
     RE_Set_TC                           => System_PolyORB_Interface,
     RE_TC_Any                           => System_PolyORB_Interface,
     RE_TC_AD                            => System_PolyORB_Interface,
     RE_TC_AS                            => System_PolyORB_Interface,
     RE_TC_B                             => System_PolyORB_Interface,
     RE_TC_C                             => System_PolyORB_Interface,
     RE_TC_F                             => System_PolyORB_Interface,
     RE_TC_I                             => System_PolyORB_Interface,
     RE_TC_LF                            => System_PolyORB_Interface,
     RE_TC_LI                            => System_PolyORB_Interface,
     RE_TC_LLF                           => System_PolyORB_Interface,
     RE_TC_LLI                           => System_PolyORB_Interface,
     RE_TC_LLU                           => System_PolyORB_Interface,
     RE_TC_LU                            => System_PolyORB_Interface,
     RE_TC_SF                            => System_PolyORB_Interface,
     RE_TC_SI                            => System_PolyORB_Interface,
     RE_TC_SSI                           => System_PolyORB_Interface,
     RE_TC_SSU                           => System_PolyORB_Interface,
     RE_TC_SU                            => System_PolyORB_Interface,
     RE_TC_U                             => System_PolyORB_Interface,
     RE_TC_Void                          => System_PolyORB_Interface,
     RE_TC_Opaque                        => System_PolyORB_Interface,
     RE_TC_WC                            => System_PolyORB_Interface,
     RE_TC_WWC                           => System_PolyORB_Interface,
     RE_TC_Array                         => System_PolyORB_Interface,
     RE_TC_Sequence                      => System_PolyORB_Interface,
     RE_TC_String                        => System_PolyORB_Interface,
     RE_TC_Struct                        => System_PolyORB_Interface,
     RE_TC_Union                         => System_PolyORB_Interface,
     RE_TC_Object                        => System_PolyORB_Interface,

     RE_Global_Pool_Object               => System_Pool_Global,

     RE_Stack_Bounded_Pool               => System_Pool_Size,

     RE_Do_Apc                           => System_RPC,
     RE_Do_Rpc                           => System_RPC,
     RE_Params_Stream_Type               => System_RPC,
     RE_Partition_ID                     => System_RPC,

     RE_IS_Is1                           => System_Scalar_Values,
     RE_IS_Is2                           => System_Scalar_Values,
     RE_IS_Is4                           => System_Scalar_Values,
     RE_IS_Is8                           => System_Scalar_Values,
     RE_IS_Iu1                           => System_Scalar_Values,
     RE_IS_Iu2                           => System_Scalar_Values,
     RE_IS_Iu4                           => System_Scalar_Values,
     RE_IS_Iu8                           => System_Scalar_Values,
     RE_IS_Iz1                           => System_Scalar_Values,
     RE_IS_Iz2                           => System_Scalar_Values,
     RE_IS_Iz4                           => System_Scalar_Values,
     RE_IS_Iz8                           => System_Scalar_Values,
     RE_IS_Isf                           => System_Scalar_Values,
     RE_IS_Ifl                           => System_Scalar_Values,
     RE_IS_Ilf                           => System_Scalar_Values,
     RE_IS_Ill                           => System_Scalar_Values,

     RE_Default_Secondary_Stack_Size     => System_Secondary_Stack,
     RE_Mark_Id                          => System_Secondary_Stack,
     RE_SS_Allocate                      => System_Secondary_Stack,
     RE_SS_Mark                          => System_Secondary_Stack,
     RE_SS_Pool                          => System_Secondary_Stack,
     RE_SS_Release                       => System_Secondary_Stack,

     RE_Shared_Var_Close                 => System_Shared_Storage,
     RE_Shared_Var_Lock                  => System_Shared_Storage,
     RE_Shared_Var_ROpen                 => System_Shared_Storage,
     RE_Shared_Var_Unlock                => System_Shared_Storage,
     RE_Shared_Var_WOpen                 => System_Shared_Storage,

     RE_Abort_Undefer_Direct             => System_Standard_Library,
     RE_Exception_Code                   => System_Standard_Library,
     RE_Exception_Data_Ptr               => System_Standard_Library,

     RE_Integer_Address                  => System_Storage_Elements,
     RE_Storage_Offset                   => System_Storage_Elements,
     RE_Storage_Array                    => System_Storage_Elements,
     RE_Storage_Element                  => System_Storage_Elements,
     RE_To_Address                       => System_Storage_Elements,

     RE_Root_Storage_Pool                => System_Storage_Pools,
     RE_Allocate_Any                     => System_Storage_Pools,
     RE_Deallocate_Any                   => System_Storage_Pools,

     RE_Thin_Pointer                     => System_Stream_Attributes,
     RE_Fat_Pointer                      => System_Stream_Attributes,

     RE_I_AD                             => System_Stream_Attributes,
     RE_I_AS                             => System_Stream_Attributes,
     RE_I_B                              => System_Stream_Attributes,
     RE_I_C                              => System_Stream_Attributes,
     RE_I_F                              => System_Stream_Attributes,
     RE_I_I                              => System_Stream_Attributes,
     RE_I_LF                             => System_Stream_Attributes,
     RE_I_LI                             => System_Stream_Attributes,
     RE_I_LLF                            => System_Stream_Attributes,
     RE_I_LLI                            => System_Stream_Attributes,
     RE_I_LLU                            => System_Stream_Attributes,
     RE_I_LU                             => System_Stream_Attributes,
     RE_I_SF                             => System_Stream_Attributes,
     RE_I_SI                             => System_Stream_Attributes,
     RE_I_SSI                            => System_Stream_Attributes,
     RE_I_SSU                            => System_Stream_Attributes,
     RE_I_SU                             => System_Stream_Attributes,
     RE_I_U                              => System_Stream_Attributes,
     RE_I_WC                             => System_Stream_Attributes,
     RE_I_WWC                            => System_Stream_Attributes,

     RE_W_AD                             => System_Stream_Attributes,
     RE_W_AS                             => System_Stream_Attributes,
     RE_W_B                              => System_Stream_Attributes,
     RE_W_C                              => System_Stream_Attributes,
     RE_W_F                              => System_Stream_Attributes,
     RE_W_I                              => System_Stream_Attributes,
     RE_W_LF                             => System_Stream_Attributes,
     RE_W_LI                             => System_Stream_Attributes,
     RE_W_LLF                            => System_Stream_Attributes,
     RE_W_LLI                            => System_Stream_Attributes,
     RE_W_LLU                            => System_Stream_Attributes,
     RE_W_LU                             => System_Stream_Attributes,
     RE_W_SF                             => System_Stream_Attributes,
     RE_W_SI                             => System_Stream_Attributes,
     RE_W_SSI                            => System_Stream_Attributes,
     RE_W_SSU                            => System_Stream_Attributes,
     RE_W_SU                             => System_Stream_Attributes,
     RE_W_U                              => System_Stream_Attributes,
     RE_W_WC                             => System_Stream_Attributes,
     RE_W_WWC                            => System_Stream_Attributes,
     RE_Block_Stream_Ops_OK              => System_Stream_Attributes,

     RE_Str_Concat                       => System_String_Ops,
     RE_Str_Concat_CC                    => System_String_Ops,
     RE_Str_Concat_CS                    => System_String_Ops,
     RE_Str_Concat_SC                    => System_String_Ops,

     RE_Str_Concat_3                     => System_String_Ops_Concat_3,

     RE_Str_Concat_4                     => System_String_Ops_Concat_4,

     RE_Str_Concat_5                     => System_String_Ops_Concat_5,

     RE_Task_Info_Type                   => System_Task_Info,
     RE_Unspecified_Task_Info            => System_Task_Info,

     RE_Library_Task_Level               => System_Tasking,

     RE_Task_Procedure_Access            => System_Tasking,

     RO_ST_Task_Id                       => System_Tasking,
     RO_ST_Null_Task                     => System_Tasking,

     RE_Call_Modes                       => System_Tasking,
     RE_Simple_Call                      => System_Tasking,
     RE_Conditional_Call                 => System_Tasking,
     RE_Asynchronous_Call                => System_Tasking,
     RE_Timed_Call                       => System_Tasking,

     RE_Ada_Task_Control_Block           => System_Tasking,

     RE_Task_List                        => System_Tasking,

     RE_Accept_Alternative               => System_Tasking,
     RE_Accept_List                      => System_Tasking,
     RE_Accept_List_Access               => System_Tasking,
     RE_Max_Select                       => System_Tasking,
     RE_Max_Task_Entry                   => System_Tasking,
     RE_No_Rendezvous                    => System_Tasking,
     RE_Null_Task_Entry                  => System_Tasking,
     RE_Positive_Select_Index            => System_Tasking,
     RE_Select_Index                     => System_Tasking,
     RE_Select_Modes                     => System_Tasking,
     RE_Else_Mode                        => System_Tasking,
     RE_Simple_Mode                      => System_Tasking,
     RE_Terminate_Mode                   => System_Tasking,
     RE_Delay_Mode                       => System_Tasking,
     RE_Task_Entry_Index                 => System_Tasking,
     RE_Self                             => System_Tasking,

     RE_Master_Id                        => System_Tasking,
     RE_Unspecified_Priority             => System_Tasking,

     RE_Activation_Chain                 => System_Tasking,

     RE_Abort_Defer                      => System_Soft_Links,
     RE_Abort_Undefer                    => System_Soft_Links,
     RE_Complete_Master                  => System_Soft_Links,
     RE_Current_Master                   => System_Soft_Links,
     RE_Enter_Master                     => System_Soft_Links,
     RE_Get_Current_Excep                => System_Soft_Links,
     RE_Get_GNAT_Exception               => System_Soft_Links,
     RE_Update_Exception                 => System_Soft_Links,

     RE_ATSD                             => System_Threads,
     RE_Thread_Body_Enter                => System_Threads,
     RE_Thread_Body_Exceptional_Exit     => System_Threads,
     RE_Thread_Body_Leave                => System_Threads,

     RE_Bits_1                           => System_Unsigned_Types,
     RE_Bits_2                           => System_Unsigned_Types,
     RE_Bits_4                           => System_Unsigned_Types,
     RE_Float_Unsigned                   => System_Unsigned_Types,
     RE_Long_Unsigned                    => System_Unsigned_Types,
     RE_Long_Long_Unsigned               => System_Unsigned_Types,
     RE_Packed_Byte                      => System_Unsigned_Types,
     RE_Packed_Bytes1                    => System_Unsigned_Types,
     RE_Packed_Bytes2                    => System_Unsigned_Types,
     RE_Packed_Bytes4                    => System_Unsigned_Types,
     RE_Short_Unsigned                   => System_Unsigned_Types,
     RE_Short_Short_Unsigned             => System_Unsigned_Types,
     RE_Unsigned                         => System_Unsigned_Types,

     RE_Value_Boolean                    => System_Val_Bool,

     RE_Value_Character                  => System_Val_Char,

     RE_Value_Decimal                    => System_Val_Dec,

     RE_Value_Enumeration_8              => System_Val_Enum,
     RE_Value_Enumeration_16             => System_Val_Enum,
     RE_Value_Enumeration_32             => System_Val_Enum,

     RE_Value_Integer                    => System_Val_Int,

     RE_Value_Long_Long_Decimal          => System_Val_LLD,

     RE_Value_Long_Long_Integer          => System_Val_LLI,

     RE_Value_Long_Long_Unsigned         => System_Val_LLU,

     RE_Value_Real                       => System_Val_Real,

     RE_Value_Unsigned                   => System_Val_Uns,

     RE_Value_Wide_Character             => System_Val_WChar,
     RE_Value_Wide_Wide_Character        => System_Val_WChar,

     RE_D                                => System_Vax_Float_Operations,
     RE_F                                => System_Vax_Float_Operations,
     RE_G                                => System_Vax_Float_Operations,
     RE_Q                                => System_Vax_Float_Operations,
     RE_S                                => System_Vax_Float_Operations,
     RE_T                                => System_Vax_Float_Operations,

     RE_D_To_G                           => System_Vax_Float_Operations,
     RE_F_To_G                           => System_Vax_Float_Operations,
     RE_F_To_Q                           => System_Vax_Float_Operations,
     RE_F_To_S                           => System_Vax_Float_Operations,
     RE_G_To_D                           => System_Vax_Float_Operations,
     RE_G_To_F                           => System_Vax_Float_Operations,
     RE_G_To_Q                           => System_Vax_Float_Operations,
     RE_G_To_T                           => System_Vax_Float_Operations,
     RE_Q_To_F                           => System_Vax_Float_Operations,
     RE_Q_To_G                           => System_Vax_Float_Operations,
     RE_S_To_F                           => System_Vax_Float_Operations,
     RE_T_To_D                           => System_Vax_Float_Operations,
     RE_T_To_G                           => System_Vax_Float_Operations,

     RE_Abs_F                            => System_Vax_Float_Operations,
     RE_Abs_G                            => System_Vax_Float_Operations,
     RE_Add_F                            => System_Vax_Float_Operations,
     RE_Add_G                            => System_Vax_Float_Operations,
     RE_Div_F                            => System_Vax_Float_Operations,
     RE_Div_G                            => System_Vax_Float_Operations,
     RE_Mul_F                            => System_Vax_Float_Operations,
     RE_Mul_G                            => System_Vax_Float_Operations,
     RE_Neg_F                            => System_Vax_Float_Operations,
     RE_Neg_G                            => System_Vax_Float_Operations,
     RE_Sub_F                            => System_Vax_Float_Operations,
     RE_Sub_G                            => System_Vax_Float_Operations,

     RE_Eq_F                             => System_Vax_Float_Operations,
     RE_Eq_G                             => System_Vax_Float_Operations,
     RE_Le_F                             => System_Vax_Float_Operations,
     RE_Le_G                             => System_Vax_Float_Operations,
     RE_Lt_F                             => System_Vax_Float_Operations,
     RE_Lt_G                             => System_Vax_Float_Operations,
     RE_Ne_F                             => System_Vax_Float_Operations,
     RE_Ne_G                             => System_Vax_Float_Operations,

     RE_Valid_D                          => System_Vax_Float_Operations,
     RE_Valid_F                          => System_Vax_Float_Operations,
     RE_Valid_G                          => System_Vax_Float_Operations,

     RE_Version_String                   => System_Version_Control,
     RE_Get_Version_String               => System_Version_Control,

     RE_Register_VMS_Exception           => System_VMS_Exception_Table,

     RE_String_To_Wide_String            => System_WCh_StW,
     RE_String_To_Wide_Wide_String       => System_WCh_StW,

     RE_Wide_String_To_String            => System_WCh_WtS,
     RE_Wide_Wide_String_To_String       => System_WCh_WtS,

     RE_Wide_Wide_Width_Character        => System_WWd_Char,
     RE_Wide_Width_Character             => System_WWd_Char,

     RE_Wide_Wide_Width_Enumeration_8    => System_WWd_Enum,
     RE_Wide_Wide_Width_Enumeration_16   => System_WWd_Enum,
     RE_Wide_Wide_Width_Enumeration_32   => System_WWd_Enum,

     RE_Wide_Width_Enumeration_8         => System_WWd_Enum,
     RE_Wide_Width_Enumeration_16        => System_WWd_Enum,
     RE_Wide_Width_Enumeration_32        => System_WWd_Enum,

     RE_Wide_Wide_Width_Wide_Character   => System_WWd_Wchar,
     RE_Wide_Wide_Width_Wide_Wide_Char   => System_WWd_Wchar,

     RE_Wide_Width_Wide_Character        => System_WWd_Wchar,
     RE_Wide_Width_Wide_Wide_Character   => System_WWd_Wchar,

     RE_Width_Boolean                    => System_Wid_Bool,

     RE_Width_Character                  => System_Wid_Char,

     RE_Width_Enumeration_8              => System_Wid_Enum,
     RE_Width_Enumeration_16             => System_Wid_Enum,
     RE_Width_Enumeration_32             => System_Wid_Enum,

     RE_Width_Long_Long_Integer          => System_Wid_LLI,

     RE_Width_Long_Long_Unsigned         => System_Wid_LLU,

     RE_Width_Wide_Character             => System_Wid_WChar,
     RE_Width_Wide_Wide_Character        => System_Wid_WChar,

     RE_Protected_Entry_Body_Array       =>
       System_Tasking_Protected_Objects_Entries,
     RE_Protection_Entries               =>
       System_Tasking_Protected_Objects_Entries,
     RE_Initialize_Protection_Entries    =>
       System_Tasking_Protected_Objects_Entries,
     RE_Lock_Entries                     =>
       System_Tasking_Protected_Objects_Entries,
     RE_Lock_Read_Only_Entries           =>
       System_Tasking_Protected_Objects_Entries,
     RE_Unlock_Entries                   =>
       System_Tasking_Protected_Objects_Entries,
     RE_Communication_Block              =>
       System_Tasking_Protected_Objects_Operations,
     RE_Protected_Entry_Call             =>
       System_Tasking_Protected_Objects_Operations,
     RE_Service_Entries                  =>
       System_Tasking_Protected_Objects_Operations,
     RE_Cancel_Protected_Entry_Call      =>
       System_Tasking_Protected_Objects_Operations,
     RE_Enqueued                         =>
       System_Tasking_Protected_Objects_Operations,
     RE_Cancelled                        =>
       System_Tasking_Protected_Objects_Operations,
     RE_Complete_Entry_Body              =>
       System_Tasking_Protected_Objects_Operations,
     RE_Exceptional_Complete_Entry_Body  =>
       System_Tasking_Protected_Objects_Operations,
     RE_Requeue_Protected_Entry          =>
       System_Tasking_Protected_Objects_Operations,
     RE_Requeue_Task_To_Protected_Entry  =>
       System_Tasking_Protected_Objects_Operations,
     RE_Protected_Count                  =>
       System_Tasking_Protected_Objects_Operations,
     RE_Protected_Entry_Caller           =>
       System_Tasking_Protected_Objects_Operations,
     RE_Timed_Protected_Entry_Call       =>
       System_Tasking_Protected_Objects_Operations,

     RE_Protection_Entry                 =>
       System_Tasking_Protected_Objects_Single_Entry,
     RE_Initialize_Protection_Entry      =>
       System_Tasking_Protected_Objects_Single_Entry,
     RE_Lock_Entry                       =>
       System_Tasking_Protected_Objects_Single_Entry,
     RE_Lock_Read_Only_Entry             =>
       System_Tasking_Protected_Objects_Single_Entry,
     RE_Unlock_Entry                     =>
       System_Tasking_Protected_Objects_Single_Entry,
     RE_Protected_Single_Entry_Call      =>
       System_Tasking_Protected_Objects_Single_Entry,
     RE_Service_Entry                    =>
       System_Tasking_Protected_Objects_Single_Entry,
     RE_Complete_Single_Entry_Body       =>
       System_Tasking_Protected_Objects_Single_Entry,
     RE_Exceptional_Complete_Single_Entry_Body =>
       System_Tasking_Protected_Objects_Single_Entry,
     RE_Protected_Count_Entry            =>
       System_Tasking_Protected_Objects_Single_Entry,
     RE_Protected_Single_Entry_Caller    =>
       System_Tasking_Protected_Objects_Single_Entry,
     RE_Timed_Protected_Single_Entry_Call =>
       System_Tasking_Protected_Objects_Single_Entry,

     RE_Protected_Entry_Index            => System_Tasking_Protected_Objects,
     RE_Entry_Body                       => System_Tasking_Protected_Objects,
     RE_Protection                       => System_Tasking_Protected_Objects,
     RE_Initialize_Protection            => System_Tasking_Protected_Objects,
     RE_Finalize_Protection              => System_Tasking_Protected_Objects,
     RE_Lock                             => System_Tasking_Protected_Objects,
     RE_Lock_Read_Only                   => System_Tasking_Protected_Objects,
     RE_Unlock                           => System_Tasking_Protected_Objects,

     RE_Delay_Block                      => System_Tasking_Async_Delays,
     RE_Timed_Out                        => System_Tasking_Async_Delays,
     RE_Cancel_Async_Delay               => System_Tasking_Async_Delays,
     RE_Enqueue_Duration                 => System_Tasking_Async_Delays,

     RE_Enqueue_Calendar                 =>
       System_Tasking_Async_Delays_Enqueue_Calendar,
     RE_Enqueue_RT                       =>
       System_Tasking_Async_Delays_Enqueue_RT,

     RE_Accept_Call                      => System_Tasking_Rendezvous,
     RE_Accept_Trivial                   => System_Tasking_Rendezvous,
     RE_Callable                         => System_Tasking_Rendezvous,
     RE_Call_Simple                      => System_Tasking_Rendezvous,
     RE_Cancel_Task_Entry_Call           => System_Tasking_Rendezvous,
     RE_Requeue_Task_Entry               => System_Tasking_Rendezvous,
     RE_Requeue_Protected_To_Task_Entry  => System_Tasking_Rendezvous,
     RE_Complete_Rendezvous              => System_Tasking_Rendezvous,
     RE_Task_Count                       => System_Tasking_Rendezvous,
     RE_Exceptional_Complete_Rendezvous  => System_Tasking_Rendezvous,
     RE_Selective_Wait                   => System_Tasking_Rendezvous,
     RE_Task_Entry_Call                  => System_Tasking_Rendezvous,
     RE_Task_Entry_Caller                => System_Tasking_Rendezvous,
     RE_Timed_Task_Entry_Call            => System_Tasking_Rendezvous,
     RE_Timed_Selective_Wait             => System_Tasking_Rendezvous,

     RE_Activate_Restricted_Tasks        => System_Tasking_Restricted_Stages,
     RE_Complete_Restricted_Activation   => System_Tasking_Restricted_Stages,
     RE_Create_Restricted_Task           => System_Tasking_Restricted_Stages,
     RE_Complete_Restricted_Task         => System_Tasking_Restricted_Stages,
     RE_Restricted_Terminated            => System_Tasking_Restricted_Stages,

     RE_Abort_Tasks                      => System_Tasking_Stages,
     RE_Activate_Tasks                   => System_Tasking_Stages,
     RE_Complete_Activation              => System_Tasking_Stages,
     RE_Create_Task                      => System_Tasking_Stages,
     RE_Complete_Task                    => System_Tasking_Stages,
     RE_Free_Task                        => System_Tasking_Stages,
     RE_Expunge_Unactivated_Tasks        => System_Tasking_Stages,
     RE_Terminated                       => System_Tasking_Stages);

   --------------------------------
   -- Configurable Run-Time Mode --
   --------------------------------

   --  Part of the job of Rtsfind is to enforce run-time restrictions in
   --  configurable run-time mode. This is done by monitoring implicit access
   --  to the run time library requested by calls to the RTE function. A call
   --  may be invalid in configurable run-time mode for either of the
   --  following two reasons:

   --     1. File in which entity lives is not present in run-time library
   --     2. File is present, but entity is not defined in the file

   --  In normal mode, either or these two situations is a fatal error
   --  that indicates that the run-time library is incorrectly configured,
   --  and a fatal error message is issued to signal this error.

   --  In configurable run-time mode, either of these two situations indicates
   --  simply that the corresponding operation is not available in the current
   --  run-time that is use. This is not a configuration error, but rather a
   --  natural result of a limited run-time. This situation is signalled by
   --  raising the exception RE_Not_Available. The caller must respond to
   --  this exception by posting an appropriate error message.

   ----------------------
   -- No_Run_Time_Mode --
   ----------------------

   --  For backwards compatibility with previous versions of GNAT, the
   --  compiler recognizes the pragma No_Run_Time. This provides a special
   --  version of configurable run-time mode that operates with the standard
   --  run-time library, but allows only a subset of entities to be
   --  accessed. If any other entity is accessed, then it is treated
   --  as a configurable run-time violation, and the exception
   --  RE_Not_Availble is raised.

   --  The following array defines the set of units that contain entities
   --  that can be referenced in No_Run_Time mode. For each of these units,
   --  all entities defined in the unit can be used in this mode.

   OK_No_Run_Time_Unit : constant array (RTU_Id) of Boolean :=
     (Ada_Exceptions          => True,
      Ada_Tags                => True,
      Interfaces              => True,
      System                  => True,
      System_Parameters       => True,
      System_Fat_Flt          => True,
      System_Fat_LFlt         => True,
      System_Fat_LLF          => True,
      System_Fat_SFlt         => True,
      System_Machine_Code     => True,
      System_Secondary_Stack  => True,
      System_Storage_Elements => True,
      System_Task_Info        => True,
      System_Unsigned_Types   => True,
      others                  => False);

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Procedure to initialize data structures used by RTE. Called at the
   --  start of processing a new main source file. Must be called after
   --  Initialize_Snames (since names it enters into name table must come
   --  after names entered by Snames).

   RE_Not_Available : exception;
   --  Raised by RTE if the requested entity is not available. This can
   --  occur either because the file in which the entity should be found
   --  does not exist, or because the entity is not present in the file.

   function RTE (E : RE_Id) return Entity_Id;
   --  Given the entity defined in the above tables, as identified by the
   --  corresponding value in the RE_Id enumeration type, returns the Id
   --  of the corresponding entity, first loading in (parsing, analyzing and
   --  expanding) its spec if the unit has not already been loaded.
   --
   --  Note: In the case of a package, RTE can return either an entity that
   --  is declared at the top level of the package, or the package entity
   --  itself. If an entity within the package has the same simple name as
   --  the package, then the entity within the package is returned rather
   --
   --  If RTE returns, the returned value is the required entity
   --
   --  If the entity is not available, then an error message is given. The
   --  form of the message depends on whether we are in configurable run time
   --  mode or not. In configurable run time mode, a missing entity is not
   --  that surprising and merely says that the particular construct is not
   --  supported by the run-time in use. If we are not in configurable run
   --  time mode, a missing entity is some kind of run-time configuration
   --  error. In either case, the result of the call is to raise the exception
   --  RE_Not_Available, which should terminate the expansion of the current
   --  construct.

   function Is_RTE (Ent : Entity_Id; E : RE_Id) return Boolean;
   --  This function determines if the given entity corresponds to the entity
   --  referenced by RE_Id. It is similar in effect to (Ent = RTE (E)) except
   --  that the latter would unconditionally load the unit containing E. For
   --  this call, if the unit is not loaded, then a result of False is returned
   --  immediately, since obviously Ent cannot be the entity in question if the
   --  corresponding unit has not been loaded.

   function Is_RTU (Ent : Entity_Id;  U : RTU_Id) return Boolean;
   pragma Inline (Is_RTU);
   --  This function determines if the given entity corresponds to the entity
   --  for the unit referenced by U. If this unit has not been loaded, the
   --  answer will always be False. If the unit has been loaded, then the
   --  entity id values are compared and True is returned if Ent is the
   --  entity for this unit.

   function RTE_Available (E : RE_Id) return Boolean;
   --  Returns true if a call to RTE will succeed without raising an
   --  exception and without generating an error message, i.e. if the
   --  call will obtain the desired entity without any problems.

   function RTU_Loaded (U : RTU_Id) return Boolean;
   pragma Inline (RTU_Loaded);
   --  Returns true if indicated unit has already been successfully loaded.
   --  If the unit has not been loaded, returns False. Note that this does
   --  not mean that an attempt to load it subsequently would fail.

   procedure Set_RTU_Loaded (N : Node_Id);
   --  Register the predefined unit N as already loaded

   procedure Text_IO_Kludge (Nam : Node_Id);
   --  In Ada 83, and hence for compatibility in Ada 9X, package Text_IO has
   --  generic subpackages (e.g. Integer_IO). They really should be child
   --  packages, and in GNAT, they *are* child packages. To maintain the
   --  required compatibility, this routine is called for package renamings
   --  and generic instantiations, with the simple name of the referenced
   --  package. If Text_IO has been with'ed and if the simple name of Nam
   --  matches one of the subpackages of Text_IO, then this subpackage is
   --  with'ed automatically. The important result of this approach is that
   --  Text_IO does not drag in all the code for the subpackages unless they
   --  are used. Our test is a little crude, and could drag in stuff when it
   --  is not necessary, but that doesn't matter. Wide_[Wide_]Text_IO is
   --  handled in a similar manner.

   function Is_Text_IO_Kludge_Unit (Nam : Node_Id) return Boolean;
   --  Returns True if the given Nam is an Expanded Name, whose Prefix is Ada,
   --  and whose selector is either Text_IO.xxx or Wide_Text_IO.xxx or
   --  Wide_Wide_Text_IO.xxx, where xxx is one of the subpackages of Text_IO
   --  that is specially handled as described above for Text_IO_Kludge.

end Rtsfind;
