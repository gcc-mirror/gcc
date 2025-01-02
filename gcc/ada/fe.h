/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                    FE                                    *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2025, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This is the C header that contains the declarations of Ada subprograms and
   variables used by gigi and not declared in other C header files.  It was
   created manually from Ada specifications.  The original Ada declarations
   in these specifications must be marked with:

   "WARNING: There is a matching C declaration of this <entity_kind> in fe.h"

   where <entity_kind> is either "subprogram" or "variable" or "type".

   WARNING: functions taking String_Pointer parameters must abide by the rule
   documented alongside the definition of String_Pointer in types.h.  */

#ifdef __cplusplus
extern "C" {
#endif

/* atree: */

#define Serious_Errors_Detected		atree__serious_errors_detected

extern Nat Serious_Errors_Detected;

/* comperr: */

#define Compiler_Abort		comperr__compiler_abort

extern void Compiler_Abort (String_Pointer, String_Pointer, Boolean) ATTRIBUTE_NORETURN;

/* debug: */

#define Debug_Flag_Dot_KK	debug__debug_flag_dot_kk
#define Debug_Flag_Dot_R	debug__debug_flag_dot_r
#define Debug_Flag_Dot_8	debug__debug_flag_dot_8
#define Debug_Flag_NN		debug__debug_flag_nn

extern Boolean Debug_Flag_Dot_KK;
extern Boolean Debug_Flag_Dot_R;
extern Boolean Debug_Flag_Dot_8;
extern Boolean Debug_Flag_NN;

/* einfo: */

/* Valid_Uint is used to preserve the old behavior of Esize and
   friends, where Uint_0 was the default. All calls to this
   are questionable. */
INLINE Valid_Uint
No_Uint_To_0 (Uint X)
{
  return X == No_Uint ? Uint_0 : X;
}

#define Set_Alignment			einfo__entities__set_alignment
#define Set_Component_Bit_Offset	einfo__entities__set_component_bit_offset
#define Set_Component_Size		einfo__entities__set_component_size
#define Set_Esize			einfo__entities__set_esize
#define Set_Mechanism			einfo__entities__set_mechanism
#define Set_Normalized_First_Bit	einfo__entities__set_normalized_first_bit
#define Set_Normalized_Position		einfo__entities__set_normalized_position
#define Set_RM_Size			einfo__entities__set_rm_size

extern void Set_Alignment		(Entity_Id, Uint);
extern void Set_Component_Bit_Offset	(Entity_Id, Uint);
extern void Set_Component_Size		(Entity_Id, Uint);
extern void Set_Esize			(Entity_Id, Uint);
extern void Set_Mechanism		(Entity_Id, Mechanism_Type);
extern void Set_Normalized_First_Bit	(Entity_Id, Uint);
extern void Set_Normalized_Position	(Entity_Id, Uint);
extern void Set_RM_Size			(Entity_Id, Uint);

#define Is_Base_Type		einfo__utils__is_base_type
#define Is_Entity_Name		einfo__utils__is_entity_name

extern Boolean Is_Base_Type	(Entity_Id);
extern Boolean Is_Entity_Name	(Node_Id);

#define Get_Attribute_Definition_Clause	einfo__utils__get_attribute_definition_clause

extern Node_Id Get_Attribute_Definition_Clause (Entity_Id, unsigned char);

/* errout: */

#define Error_Msg_N		errout__error_msg_n
#define Error_Msg_NE		errout__error_msg_ne
#define Set_Identifier_Casing	errout__set_identifier_casing

extern void Error_Msg_N			(String_Pointer, Node_Id);
extern void Error_Msg_NE		(String_Pointer, Node_Id, Entity_Id);
extern void Set_Identifier_Casing	(void *, const void *);

/* err_vars: */

#define Error_Msg_Uint_1	err_vars__error_msg_uint_1
#define Error_Msg_Uint_2	err_vars__error_msg_uint_2

extern Uint Error_Msg_Uint_1;
extern Uint Error_Msg_Uint_2;

/* exp_ch11:  */

#define Get_Local_Raise_Call_Entity	exp_ch11__get_local_raise_call_entity
#define Get_RT_Exception_Entity		exp_ch11__get_rt_exception_entity
#define Get_RT_Exception_Name		exp_ch11__get_rt_exception_name
#define Warn_If_No_Local_Raise		exp_ch11__warn_if_no_local_raise

extern Entity_Id Get_Local_Raise_Call_Entity	(void);
extern Entity_Id Get_RT_Exception_Entity	(int);
extern void Get_RT_Exception_Name		(enum RT_Exception_Code);
extern void Warn_If_No_Local_Raise		(int);

/* exp_code:  */

#define Asm_Input_Constraint	exp_code__asm_input_constraint
#define Asm_Input_Value		exp_code__asm_input_value
#define Asm_Output_Constraint	exp_code__asm_output_constraint
#define Asm_Output_Variable	exp_code__asm_output_variable
#define Asm_Template		exp_code__asm_template
#define Clobber_Get_Next	exp_code__clobber_get_next
#define Clobber_Setup		exp_code__clobber_setup
#define Is_Asm_Volatile		exp_code__is_asm_volatile
#define Next_Asm_Input		exp_code__next_asm_input
#define Next_Asm_Output		exp_code__next_asm_output
#define Setup_Asm_Inputs	exp_code__setup_asm_inputs
#define Setup_Asm_Outputs	exp_code__setup_asm_outputs

extern Node_Id Asm_Input_Constraint	(void);
extern Node_Id Asm_Input_Value		(void);
extern Node_Id Asm_Output_Constraint	(void);
extern Node_Id Asm_Output_Variable	(void);
extern Node_Id Asm_Template		(Node_Id);
extern void *Clobber_Get_Next		(void);
extern void Clobber_Setup		(Node_Id);
extern Boolean Is_Asm_Volatile		(Node_Id);
extern void Next_Asm_Input		(void);
extern void Next_Asm_Output		(void);
extern void Setup_Asm_Inputs		(Node_Id);
extern void Setup_Asm_Outputs		(Node_Id);

/* exp_dbug:  */

#define Get_Encoded_Name	exp_dbug__get_encoded_name
#define Get_External_Name	exp_dbug__get_external_name
#define Get_Variant_Encoding 	exp_dbug__get_variant_encoding

extern void Get_Encoded_Name		(Entity_Id);
extern void Get_External_Name		(Entity_Id, Boolean, String_Pointer);
extern void Get_Variant_Encoding	(Entity_Id);

/* exp_tss: */

#define Is_Init_Proc	exp_tss__is_init_proc
#define Is_Rep_To_Pos	exp_tss__is_rep_to_pos

extern Boolean Is_Init_Proc		(Entity_Id);
extern Boolean Is_Rep_To_Pos		(Entity_Id);

/* exp_util: */

#define Find_Interface_Tag		exp_util__find_interface_tag
#define Is_Fully_Repped_Tagged_Type	exp_util__is_fully_repped_tagged_type
#define Is_Related_To_Func_Return	exp_util__is_related_to_func_return
#define Is_Secondary_Stack_Thunk	exp_util__is_secondary_stack_thunk
#define Thunk_Target			exp_util__thunk_target

extern Entity_Id Find_Interface_Tag		(Entity_Id, Entity_Id);
extern Boolean Is_Fully_Repped_Tagged_Type	(Entity_Id);
extern Boolean Is_Related_To_Func_Return	(Entity_Id);
extern Boolean Is_Secondary_Stack_Thunk		(Entity_Id);
extern Entity_Id Thunk_Target 			(Entity_Id);

/* lib: */

#define Cunit 				lib__cunit
#define Ident_String			lib__ident_string
#define In_Extended_Main_Code_Unit	lib__in_extended_main_code_unit

extern Node_Id Cunit				(Unit_Number_Type);
extern Node_Id Ident_String			(Unit_Number_Type);
extern Boolean In_Extended_Main_Code_Unit	(Entity_Id);

/* opt: */

#define Ada_Version			opt__ada_version
#define Back_End_Inlining		opt__back_end_inlining
#define Debug_Generated_Code		opt__debug_generated_code
#define Enable_128bit_Types		opt__enable_128bit_types
#define Exception_Extra_Info		opt__exception_extra_info
#define Exception_Locations_Suppressed	opt__exception_locations_suppressed
#define Generate_SCO_Instance_Table	opt__generate_sco_instance_table
#define GNAT_Mode			opt__gnat_mode
#define List_Representation_Info	opt__list_representation_info
#define No_Strict_Aliasing_CP		opt__no_strict_aliasing
#define Suppress_Checks			opt__suppress_checks
#define Unnest_Subprogram_Mode		opt__unnest_subprogram_mode

typedef enum {
  Ada_83, Ada_95, Ada_2005, Ada_2012, Ada_2022
} Ada_Version_Type;
// Ada_With_Core_Extensions and Ada_With_All_Extensions (see opt.ads) are not
// used on the C side for now. If we decide to use them, we should import
// All_Extensions_Allowed and Core_Extensions_Allowed functions.

extern Ada_Version_Type Ada_Version;
extern Boolean Back_End_Inlining;
extern Boolean Debug_Generated_Code;
extern Boolean Enable_128bit_Types;
extern Boolean Exception_Extra_Info;
extern Boolean Exception_Locations_Suppressed;
extern Boolean Generate_SCO_Instance_Table;
extern Boolean GNAT_Mode;
extern Int List_Representation_Info;
extern Boolean No_Strict_Aliasing_CP;
extern Boolean Suppress_Checks;
extern Boolean Unnest_Subprogram_Mode;

#define ZCX_Exceptions		opt__zcx_exceptions
#define SJLJ_Exceptions		opt__sjlj_exceptions

extern Boolean ZCX_Exceptions		(void);
extern Boolean SJLJ_Exceptions		(void);

/* restrict: */

#define Check_Elaboration_Code_Allowed	\
  restrict__check_elaboration_code_allowed
#define Check_Implicit_Dynamic_Code_Allowed	\
  restrict__check_implicit_dynamic_code_allowed
#define Check_No_Implicit_Heap_Alloc	\
  restrict__check_no_implicit_heap_alloc
#define Check_No_Implicit_Protected_Alloc	\
  restrict__check_no_implicit_protected_alloc
#define Check_No_Implicit_Task_Alloc	\
  restrict__check_no_implicit_task_alloc
#define Check_Restriction_No_Dependence_On_System \
  restrict__check_restriction_no_dependence_on_system
#define No_Exception_Handlers_Set	\
  restrict__no_exception_handlers_set
#define No_Exception_Propagation_Active	\
  restrict__no_exception_propagation_active

extern void Check_Elaboration_Code_Allowed	(Node_Id);
extern void Check_Implicit_Dynamic_Code_Allowed	(Node_Id);
extern void Check_No_Implicit_Heap_Alloc	(Node_Id);
extern void Check_No_Implicit_Protected_Alloc	(Node_Id);
extern void Check_No_Implicit_Task_Alloc	(Node_Id);
extern void Check_Restriction_No_Dependence_On_System (Name_Id, Node_Id);
extern Boolean No_Exception_Handlers_Set	(void);
extern Boolean No_Exception_Propagation_Active	(void);

/* sem_aggr:  */

#define Is_Single_Aggregate	sem_aggr__is_single_aggregate

extern Boolean Is_Single_Aggregate	(Node_Id);

/* sem_aux:  */

#define Ancestor_Subtype		sem_aux__ancestor_subtype
#define Constant_Value			sem_aux__constant_value
#define First_Discriminant		sem_aux__first_discriminant
#define First_Stored_Discriminant	sem_aux__first_stored_discriminant
#define First_Subtype			sem_aux__first_subtype
#define Is_By_Reference_Type		sem_aux__is_by_reference_type
#define Is_Derived_Type			sem_aux__is_derived_type

extern Entity_Id Ancestor_Subtype		(Entity_Id);
extern Node_Id Constant_Value			(Entity_Id);
extern Entity_Id First_Discriminant		(Entity_Id);
extern Entity_Id First_Stored_Discriminant	(Entity_Id);
extern Entity_Id First_Subtype			(Entity_Id);
extern Boolean Is_By_Reference_Type		(Entity_Id);
extern Boolean Is_Derived_Type			(Entity_Id);

/* sem_eval: */

#define Compile_Time_Known_Value	sem_eval__compile_time_known_value
#define Is_Null_Range			sem_eval__is_null_range

extern Boolean Compile_Time_Known_Value	(Node_Id);
extern Boolean Is_Null_Range 		(Node_Id, Node_Id);

/* sem_util: */

#define Defining_Entity			sem_util__defining_entity
#define First_Actual			sem_util__first_actual
#define Has_Storage_Model_Type_Aspect	sem_util__storage_model_support__has_storage_model_type_aspect
#define Has_Designated_Storage_Model_Aspect sem_util__storage_model_support__has_designated_storage_model_aspect
#define Is_Expression_Function		sem_util__is_expression_function
#define Is_Variable_Size_Record 	sem_util__is_variable_size_record
#define Needs_Secondary_Stack		sem_util__needs_secondary_stack
#define Next_Actual			sem_util__next_actual
#define Storage_Model_Object 		sem_util__storage_model_support__storage_model_object
#define Storage_Model_Copy_From 	sem_util__storage_model_support__storage_model_copy_from
#define Storage_Model_Copy_To 		sem_util__storage_model_support__storage_model_copy_to

extern Entity_Id Defining_Entity		(Node_Id);
extern Node_Id First_Actual			(Node_Id);
extern Boolean Has_Storage_Model_Type_Aspect	(Entity_Id);
extern Boolean Has_Designated_Storage_Model_Aspect (Entity_Id);
extern Boolean Is_Expression_Function		(Entity_Id);
extern Boolean Is_Variable_Size_Record 		(Entity_Id);
extern Boolean Needs_Secondary_Stack		(Entity_Id);
extern Node_Id Next_Actual			(Node_Id);
extern Entity_Id Storage_Model_Object		(Entity_Id);
extern Entity_Id Storage_Model_Copy_From	(Entity_Id);
extern Entity_Id Storage_Model_Copy_To 		(Entity_Id);

/* sinfo: */

#define Spec_Lib_Unit                   sinfo__utils__spec_lib_unit
#define Body_Lib_Unit                   sinfo__utils__body_lib_unit
#define Subunit_Parent                  sinfo__utils__subunit_parent
#define Stub_Subunit                    sinfo__utils__stub_subunit
#define Withed_Lib_Unit                 sinfo__utils__withed_lib_unit
#define End_Location			sinfo__utils__end_location
#define Set_Has_No_Elaboration_Code	sinfo__nodes__set_has_no_elaboration_code
#define Set_Present_Expr		sinfo__nodes__set_present_expr

extern Node_Id Spec_Lib_Unit            (Node_Id);
extern Node_Id Body_Lib_Unit            (Node_Id);
extern Node_Id Subunit_Parent           (Node_Id);
extern Node_Id Stub_Subunit             (Node_Id);
extern Node_Id Withed_Lib_Unit          (Node_Id);
extern Source_Ptr End_Location 		(Node_Id);
extern void Set_Has_No_Elaboration_Code	(Node_Id, Boolean);
extern void Set_Present_Expr		(Node_Id, Uint);

/* sinput: */

struct c_array {
  const char *pointer;
  int length;
};

#define C_Source_Buffer 	sinput__c_source_buffer
#define Debug_Source_Name	sinput__debug_source_name
#define Get_Column_Number	sinput__get_column_number
#define Get_Logical_Line_Number	sinput__get_logical_line_number
#define Get_Source_File_Index	sinput__get_source_file_index

extern struct c_array C_Source_Buffer 		(Source_File_Index);
extern File_Name_Type Debug_Source_Name		(Source_File_Index);
extern Column_Number_Type Get_Column_Number	(Source_Ptr);
extern Line_Number_Type Get_Logical_Line_Number	(Source_Ptr);
extern Source_File_Index Get_Source_File_Index	(Source_Ptr);

/* targparm: */

#define Machine_Overflows_On_Target	\
  targparm__machine_overflows_on_target
#define Signed_Zeros_On_Target	\
  targparm__signed_zeros_on_target
#define Stack_Check_Limits_On_Target	\
  targparm__stack_check_limits_on_target
#define Stack_Check_Probes_On_Target	\
  targparm__stack_check_probes_on_target

extern Boolean Machine_Overflows_On_Target;
extern Boolean Signed_Zeros_On_Target;
extern Boolean Stack_Check_Limits_On_Target;
extern Boolean Stack_Check_Probes_On_Target;

/* warnsw: */

#define Get_Warn_On_Questionable_Layout	warnsw__get_warn_on_questionable_layout

extern Boolean Get_Warn_On_Questionable_Layout (void);

// The following corresponds to Ada code in Einfo.Utils.

typedef Boolean   B;
typedef Entity_Id E;
typedef Node_Id   N;

#define Address_Clause einfo__utils__address_clause
extern N Address_Clause (E Id);

#define Alignment_Clause einfo__utils__alignment_clause
extern N Alignment_Clause (E Id);

#define Base_Type einfo__utils__base_type
extern E Base_Type (E Id);

#define Declaration_Node einfo__utils__declaration_node
extern N Declaration_Node (E Id);

#define Designated_Type einfo__utils__designated_type
extern E Designated_Type (E Id);

#define First_Formal einfo__utils__first_formal
extern E First_Formal (E Id);

#define First_Formal_With_Extras einfo__utils__first_formal_with_extras
extern E First_Formal_With_Extras (E Id);

#define Has_Foreign_Convention einfo__utils__has_foreign_convention
extern B Has_Foreign_Convention (E Id);

#define Implementation_Base_Type einfo__utils__implementation_base_type
extern E Implementation_Base_Type (E Id);

#define Is_Boolean_Type einfo__utils__is_boolean_type
extern B Is_Boolean_Type (E Id);

#define Next_Discriminant einfo__utils__next_discriminant
extern E Next_Discriminant (E Id);

#define Next_Formal einfo__utils__next_formal
extern E Next_Formal (E Id);

#define Next_Formal_With_Extras einfo__utils__next_formal_with_extras
extern E Next_Formal_With_Extras (E Id);

#define Number_Dimensions einfo__utils__number_dimensions
extern Pos Number_Dimensions (E Id);

#define Object_Size_Clause einfo__utils__object_size_clause
extern N Object_Size_Clause (E Id);

#define Root_Type einfo__utils__root_type
extern E Root_Type (E Id);

#define Size_Clause einfo__utils__size_clause
extern N Size_Clause (E Id);

#define Type_High_Bound einfo__utils__type_high_bound
extern N Type_High_Bound (E Id);

#define Type_Low_Bound einfo__utils__type_low_bound
extern N Type_Low_Bound (E Id);

#define Underlying_Type einfo__utils__underlying_type
extern E Underlying_Type (E Id);

#define Known_Alignment einfo__utils__known_alignment
extern B Known_Alignment (Entity_Id E);

#define Known_Component_Size einfo__utils__known_component_size
extern B Known_Component_Size (Entity_Id E);

#define Known_Esize einfo__utils__known_esize
extern B Known_Esize (Entity_Id E);

#define Known_Normalized_Position einfo__utils__known_normalized_position
extern B Known_Normalized_Position (Entity_Id E);

#define Known_RM_Size einfo__utils__known_rm_size
extern B Known_RM_Size (Entity_Id E);

#define Copy_Alignment einfo__utils__copy_alignment
extern B Copy_Alignment(Entity_Id To, Entity_Id From);

#define Copy_Esize einfo__utils__copy_esize
extern B Copy_Esize(Entity_Id To, Entity_Id From);

#define Copy_RM_Size einfo__utils__copy_rm_size
extern B Copy_RM_Size(Entity_Id To, Entity_Id From);

#define Is_Discrete_Or_Fixed_Point_Type einfo__utils__is_discrete_or_fixed_point_type
extern B Is_Discrete_Or_Fixed_Point_Type (E Id);

#define Is_Floating_Point_Type einfo__utils__is_floating_point_type
extern B Is_Floating_Point_Type (E Id);

#define Is_Record_Type einfo__utils__is_record_type
extern B Is_Record_Type (E Id);

#define Is_Full_Access einfo__utils__is_full_access
extern B Is_Full_Access (E Id);

#define Next_Index einfo__utils__next_index
extern Node_Id Next_Index (Node_Id Id);

#define Next_Literal einfo__utils__next_literal
extern E Next_Literal (E Id);

#define Next_Stored_Discriminant einfo__utils__next_stored_discriminant
extern E Next_Stored_Discriminant (E Id);

// The following is needed because Convention in Sem_Util is a renaming
// of Basic_Convention.

static inline Convention_Id
Convention (N Node)
{
  extern Byte einfo__entities__basic_convention (N Node);
  return (Convention_Id) einfo__entities__basic_convention (Node);
}

// See comments regarding Entity_Or_Associated_Node in Sinfo.Utils.

#define Entity sinfo__nodes__entity_or_associated_node
extern Entity_Id Entity (N Node);

// See comments regarding Renamed_Or_Alias in Einfo.Utils

#define Alias einfo__entities__renamed_or_alias
extern Node_Id Alias (N Node);

#define Renamed_Entity einfo__entities__renamed_or_alias
extern Node_Id Renamed_Entity (N Node);

#define Renamed_Object einfo__entities__renamed_or_alias
extern Node_Id Renamed_Object (N Node);

#ifdef __cplusplus
}
#endif
