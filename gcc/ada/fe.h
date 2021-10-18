/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                    FE                                    *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2021, Free Software Foundation, Inc.         *
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

#define Is_Entity_Name		einfo__utils__is_entity_name

extern Boolean Is_Entity_Name		(Node_Id);

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

extern Boolean Is_Init_Proc		(Entity_Id);

/* exp_util: */

#define Is_Fully_Repped_Tagged_Type	exp_util__is_fully_repped_tagged_type
#define Find_Interface_Tag		exp_util__find_interface_tag

extern Boolean Is_Fully_Repped_Tagged_Type      (Entity_Id);
extern Entity_Id Find_Interface_Tag		(Entity_Id, Entity_Id);

/* lib: */

#define Cunit 				lib__cunit
#define Ident_String			lib__ident_string
#define In_Extended_Main_Code_Unit	lib__in_extended_main_code_unit

extern Node_Id Cunit				(Unit_Number_Type);
extern Node_Id Ident_String			(Unit_Number_Type);
extern Boolean In_Extended_Main_Code_Unit	(Entity_Id);

/* opt: */

#define Ada_Version			opt__ada_version
#define Assume_No_Invalid_Values	opt__assume_no_invalid_values
#define Back_End_Inlining		opt__back_end_inlining
#define Debug_Generated_Code		opt__debug_generated_code
#define Enable_128bit_Types		opt__enable_128bit_types
#define Exception_Extra_Info		opt__exception_extra_info
#define Exception_Locations_Suppressed	opt__exception_locations_suppressed
#define Exception_Mechanism		opt__exception_mechanism
#define Generate_SCO_Instance_Table	opt__generate_sco_instance_table
#define GNAT_Mode			opt__gnat_mode
#define List_Representation_Info	opt__list_representation_info
#define No_Strict_Aliasing_CP		opt__no_strict_aliasing
#define Suppress_Checks			opt__suppress_checks

typedef enum {
  Ada_83, Ada_95, Ada_2005, Ada_2012, Ada_2022, Ada_With_Extensions
} Ada_Version_Type;

typedef enum {
  Front_End_SJLJ, Back_End_ZCX, Back_End_SJLJ
} Exception_Mechanism_Type;

extern Ada_Version_Type Ada_Version;
extern Boolean Assume_No_Invalid_Values;
extern Boolean Back_End_Inlining;
extern Boolean Debug_Generated_Code;
extern Boolean Enable_128bit_Types;
extern Boolean Exception_Extra_Info;
extern Boolean Exception_Locations_Suppressed;
extern Exception_Mechanism_Type Exception_Mechanism;
extern Boolean Generate_SCO_Instance_Table;
extern Boolean GNAT_Mode;
extern Int List_Representation_Info;
extern Boolean No_Strict_Aliasing_CP;
extern Boolean Suppress_Checks;

#define ZCX_Exceptions		opt__zcx_exceptions
#define SJLJ_Exceptions		opt__sjlj_exceptions
#define Front_End_Exceptions	opt__front_end_exceptions
#define Back_End_Exceptions	opt__back_end_exceptions

extern Boolean ZCX_Exceptions		(void);
extern Boolean SJLJ_Exceptions		(void);
extern Boolean Front_End_Exceptions	(void);
extern Boolean Back_End_Exceptions	(void);

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
#define No_Exception_Handlers_Set	\
  restrict__no_exception_handlers_set
#define No_Exception_Propagation_Active	\
  restrict__no_exception_propagation_active

extern void Check_Elaboration_Code_Allowed	(Node_Id);
extern void Check_Implicit_Dynamic_Code_Allowed	(Node_Id);
extern void Check_No_Implicit_Heap_Alloc	(Node_Id);
extern void Check_No_Implicit_Protected_Alloc	(Node_Id);
extern void Check_No_Implicit_Task_Alloc	(Node_Id);
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

extern Boolean Compile_Time_Known_Value	(Node_Id);

/* sem_util: */

#define Defining_Entity			sem_util__defining_entity
#define First_Actual			sem_util__first_actual
#define Is_Variable_Size_Record 	sem_util__is_variable_size_record
#define Next_Actual			sem_util__next_actual
#define Requires_Transient_Scope	sem_util__requires_transient_scope

extern Entity_Id Defining_Entity	(Node_Id);
extern Node_Id First_Actual		(Node_Id);
extern Boolean Is_Variable_Size_Record 	(Entity_Id Id);
extern Node_Id Next_Actual		(Node_Id);
extern Boolean Requires_Transient_Scope	(Entity_Id);

/* sinfo: */

#define End_Location			sinfo__utils__end_location
#define Set_Has_No_Elaboration_Code	sinfo__nodes__set_has_no_elaboration_code
#define Set_Present_Expr		sinfo__nodes__set_present_expr

extern Source_Ptr End_Location 		(Node_Id);
extern void Set_Has_No_Elaboration_Code	(Node_Id, Boolean);
extern void Set_Present_Expr		(Node_Id, Uint);

/* sinput: */

#define Debug_Source_Name	sinput__debug_source_name
#define Get_Column_Number	sinput__get_column_number
#define Get_Logical_Line_Number	sinput__get_logical_line_number
#define Get_Source_File_Index	sinput__get_source_file_index

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

#define Warn_On_Questionable_Layout	warnsw__warn_on_questionable_layout

extern Boolean Warn_On_Questionable_Layout;

// The following corresponds to Ada code in Einfo.Utils.

typedef Boolean B;
typedef Component_Alignment_Kind C;
typedef Entity_Id E;
typedef Mechanism_Type M;
typedef Node_Id N;
typedef Uint U;
typedef Ureal R;
typedef Elist_Id L;
typedef List_Id S;

#define Is_Access_Object_Type einfo__utils__is_access_object_type
B Is_Access_Object_Type               (E Id);

#define Is_Named_Access_Type einfo__utils__is_named_access_type
B Is_Named_Access_Type                (E Id);

#define Address_Clause einfo__utils__address_clause
N Address_Clause                      (E Id);

#define Aft_Value einfo__utils__aft_value
U Aft_Value                           (E Id);

#define Alignment_Clause einfo__utils__alignment_clause
N Alignment_Clause                    (E Id);

#define Base_Type einfo__utils__base_type
E Base_Type                           (E Id);

#define Declaration_Node einfo__utils__declaration_node
N Declaration_Node                    (E Id);

#define Designated_Type einfo__utils__designated_type
E Designated_Type                     (E Id);

#define First_Component einfo__utils__first_component
E First_Component                     (E Id);

#define First_Component_Or_Discriminant einfo__utils__first_component_or_discriminant
E First_Component_Or_Discriminant     (E Id);

#define First_Formal einfo__utils__first_formal
E First_Formal                        (E Id);

#define First_Formal_With_Extras einfo__utils__first_formal_with_extras
E First_Formal_With_Extras            (E Id);

#define Has_Attach_Handler einfo__utils__has_attach_handler
B Has_Attach_Handler                  (E Id);

#define Has_Entries einfo__utils__has_entries
B Has_Entries                         (E Id);

#define Has_Foreign_Convention einfo__utils__has_foreign_convention
B Has_Foreign_Convention              (E Id);

#define Has_Interrupt_Handler einfo__utils__has_interrupt_handler
B Has_Interrupt_Handler               (E Id);

#define Has_Non_Limited_View einfo__utils__has_non_limited_view
B Has_Non_Limited_View                (E Id);

#define Has_Non_Null_Abstract_State einfo__utils__has_non_null_abstract_state
B Has_Non_Null_Abstract_State         (E Id);

#define Has_Non_Null_Visible_Refinement einfo__utils__has_non_null_visible_refinement
B Has_Non_Null_Visible_Refinement     (E Id);

#define Has_Null_Abstract_State einfo__utils__has_null_abstract_state
B Has_Null_Abstract_State             (E Id);

#define Has_Null_Visible_Refinement einfo__utils__has_null_visible_refinement
B Has_Null_Visible_Refinement         (E Id);

#define Implementation_Base_Type einfo__utils__implementation_base_type
E Implementation_Base_Type            (E Id);

#define Is_Base_Type einfo__utils__is_base_type
B Is_Base_Type                        (E Id);

#define Is_Boolean_Type einfo__utils__is_boolean_type
B Is_Boolean_Type                     (E Id);

#define Is_Constant_Object einfo__utils__is_constant_object
B Is_Constant_Object                  (E Id);

#define Is_Controlled einfo__utils__is_controlled
B Is_Controlled                       (E Id);

#define Is_Discriminal einfo__utils__is_discriminal
B Is_Discriminal                      (E Id);

#define Is_Dynamic_Scope einfo__utils__is_dynamic_scope
B Is_Dynamic_Scope                    (E Id);

#define Is_Elaboration_Target einfo__utils__is_elaboration_target
B Is_Elaboration_Target               (E Id);

#define Is_External_State einfo__utils__is_external_state
B Is_External_State                   (E Id);

#define Is_Finalizer einfo__utils__is_finalizer
B Is_Finalizer                        (E Id);

#define Is_Null_State einfo__utils__is_null_state
B Is_Null_State                       (E Id);

#define Is_Package_Or_Generic_Package einfo__utils__is_package_or_generic_package
B Is_Package_Or_Generic_Package       (E Id);

#define Is_Packed_Array einfo__utils__is_packed_array
B Is_Packed_Array                     (E Id);

#define Is_Prival einfo__utils__is_prival
B Is_Prival                           (E Id);

#define Is_Protected_Component einfo__utils__is_protected_component
B Is_Protected_Component              (E Id);

#define Is_Protected_Interface einfo__utils__is_protected_interface
B Is_Protected_Interface              (E Id);

#define Is_Protected_Record_Type einfo__utils__is_protected_record_type
B Is_Protected_Record_Type            (E Id);

#define Is_Relaxed_Initialization_State einfo__utils__is_relaxed_initialization_state
B Is_Relaxed_Initialization_State     (E Id);

#define Is_Standard_Character_Type einfo__utils__is_standard_character_type
B Is_Standard_Character_Type          (E Id);

#define Is_Standard_String_Type einfo__utils__is_standard_string_type
B Is_Standard_String_Type             (E Id);

#define Is_String_Type einfo__utils__is_string_type
B Is_String_Type                      (E Id);

#define Is_Synchronized_Interface einfo__utils__is_synchronized_interface
B Is_Synchronized_Interface           (E Id);

#define Is_Synchronized_State einfo__utils__is_synchronized_state
B Is_Synchronized_State               (E Id);

#define Is_Task_Interface einfo__utils__is_task_interface
B Is_Task_Interface                   (E Id);

#define Is_Task_Record_Type einfo__utils__is_task_record_type
B Is_Task_Record_Type                 (E Id);

#define Is_Wrapper_Package einfo__utils__is_wrapper_package
B Is_Wrapper_Package                  (E Id);

#define Last_Formal einfo__utils__last_formal
E Last_Formal                         (E Id);

#define Machine_Emax_Value einfo__utils__machine_emax_value
U Machine_Emax_Value                  (E Id);

#define Machine_Emin_Value einfo__utils__machine_emin_value
U Machine_Emin_Value                  (E Id);

#define Machine_Mantissa_Value einfo__utils__machine_mantissa_value
U Machine_Mantissa_Value              (E Id);

#define Machine_Radix_Value einfo__utils__machine_radix_value
U Machine_Radix_Value                 (E Id);

#define Model_Emin_Value einfo__utils__model_emin_value
U Model_Emin_Value                    (E Id);

#define Model_Epsilon_Value einfo__utils__model_epsilon_value
R Model_Epsilon_Value                 (E Id);

#define Model_Mantissa_Value einfo__utils__model_mantissa_value
U Model_Mantissa_Value                (E Id);

#define Model_Small_Value einfo__utils__model_small_value
R Model_Small_Value                   (E Id);

#define Next_Component einfo__utils__next_component
E Next_Component                      (E Id);

#define Next_Component_Or_Discriminant einfo__utils__next_component_or_discriminant
E Next_Component_Or_Discriminant      (E Id);

#define Next_Discriminant einfo__utils__next_discriminant
E Next_Discriminant                   (E Id);

#define Next_Formal einfo__utils__next_formal
E Next_Formal                         (E Id);

#define Next_Formal_With_Extras einfo__utils__next_formal_with_extras
E Next_Formal_With_Extras             (E Id);

#define Number_Dimensions einfo__utils__number_dimensions
Pos Number_Dimensions                   (E Id);

#define Number_Entries einfo__utils__number_entries
Nat Number_Entries                      (E Id);

#define Number_Formals einfo__utils__number_formals
Pos Number_Formals                      (E Id);

#define Object_Size_Clause einfo__utils__object_size_clause
N Object_Size_Clause                  (E Id);

#define Partial_Refinement_Constituents einfo__utils__partial_refinement_constituents
L Partial_Refinement_Constituents     (E Id);

#define Primitive_Operations einfo__utils__primitive_operations
L Primitive_Operations                (E Id);

#define Root_Type einfo__utils__root_type
E Root_Type                           (E Id);

#define Safe_Emax_Value einfo__utils__safe_emax_value
U Safe_Emax_Value                     (E Id);

#define Safe_First_Value einfo__utils__safe_first_value
R Safe_First_Value                    (E Id);

#define Safe_Last_Value einfo__utils__safe_last_value
R Safe_Last_Value                     (E Id);

#define Scope_Depth einfo__utils__scope_depth
U Scope_Depth                         (E Id);

#define Scope_Depth_Set einfo__utils__scope_depth_set
B Scope_Depth_Set                     (E Id);

#define Size_Clause einfo__utils__size_clause
N Size_Clause                         (E Id);

#define Stream_Size_Clause einfo__utils__stream_size_clause
N Stream_Size_Clause                  (E Id);

#define Type_High_Bound einfo__utils__type_high_bound
N Type_High_Bound                     (E Id);

#define Type_Low_Bound einfo__utils__type_low_bound
N Type_Low_Bound                      (E Id);

#define Underlying_Type einfo__utils__underlying_type
E Underlying_Type                     (E Id);

#define Known_Alignment einfo__utils__known_alignment
B Known_Alignment                       (Entity_Id E);

#define Known_Component_Bit_Offset einfo__utils__known_component_bit_offset
B Known_Component_Bit_Offset            (Entity_Id E);

#define Known_Component_Size einfo__utils__known_component_size
B Known_Component_Size                  (Entity_Id E);

#define Known_Esize einfo__utils__known_esize
B Known_Esize                           (Entity_Id E);

#define Known_Normalized_First_Bit einfo__utils__known_normalized_first_bit
B Known_Normalized_First_Bit            (Entity_Id E);

#define Known_Normalized_Position einfo__utils__known_normalized_position
B Known_Normalized_Position             (Entity_Id E);

#define Known_Normalized_Position_Max einfo__utils__known_normalized_position_max
B Known_Normalized_Position_Max         (Entity_Id E);

#define Known_RM_Size einfo__utils__known_rm_size
B Known_RM_Size                         (Entity_Id E);

#define Copy_Alignment einfo__utils__copy_alignment
B Copy_Alignment(Entity_Id To, Entity_Id From);

#define Copy_Esize einfo__utils__copy_esize
B Copy_Esize(Entity_Id To, Entity_Id From);

#define Copy_RM_Size einfo__utils__copy_rm_size
B Copy_RM_Size(Entity_Id To, Entity_Id From);

#define Is_Discrete_Or_Fixed_Point_Type einfo__utils__is_discrete_or_fixed_point_type
B Is_Discrete_Or_Fixed_Point_Type     (E Id);

#define Is_Floating_Point_Type einfo__utils__is_floating_point_type
B Is_Floating_Point_Type                      (E Id);

#define Is_Record_Type einfo__utils__is_record_type
B Is_Record_Type                      (E Id);

#define Has_DIC einfo__utils__has_dic
B Has_DIC (E Id);

#define Has_Invariants einfo__utils__has_invariants
B Has_Invariants (E Id);

#define Is_Full_Access einfo__utils__is_full_access
B Is_Full_Access (E Id);

#define Next_Index einfo__utils__next_index
Node_Id Next_Index (Node_Id Id);

#define Next_Literal einfo__utils__next_literal
E Next_Literal (E Id);

#define Next_Stored_Discriminant einfo__utils__next_stored_discriminant
E Next_Stored_Discriminant (E Id);

#define Parameter_Mode einfo__utils__parameter_mode
// Parameter_Mode really returns Formal_Kind, but that is not visible, because
// fe.h is included before einfo.h.
Entity_Kind Parameter_Mode (E Id);

#define Is_List_Member einfo__utils__is_list_member
B Is_List_Member (N Node);

#define List_Containing einfo__utils__list_containing
S List_Containing (N Node);

// The following is needed because Convention in Sem_Util is a renaming
// of Basic_Convention.

#define Convention einfo__entities__basic_convention
Convention_Id Convention (N Node);

// See comments regarding Entity_Or_Associated_Node in Sinfo.Utils.

#define Entity sinfo__nodes__entity_or_associated_node
Entity_Id Entity (N Node);

// See comments regarding Renamed_Or_Alias in Einfo.Utils

#define Alias einfo__entities__renamed_or_alias

#define Renamed_Entity einfo__entities__renamed_or_alias
Node_Id Renamed_Entity (N Node);

#define Renamed_Object einfo__entities__renamed_or_alias
Node_Id Renamed_Object (N Node);

#ifdef __cplusplus
}
#endif
