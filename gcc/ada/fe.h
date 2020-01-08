/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                    FE                                    *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2019, Free Software Foundation, Inc.         *
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

extern int Compiler_Abort (String_Pointer, String_Pointer, Boolean) ATTRIBUTE_NORETURN;

/* debug: */

#define Debug_Flag_Dot_R	debug__debug_flag_dot_r
#define Debug_Flag_NN		debug__debug_flag_nn

extern Boolean Debug_Flag_Dot_R;
extern Boolean Debug_Flag_NN;

/* einfo: */

#define Set_Alignment			einfo__set_alignment
#define Set_Component_Bit_Offset	einfo__set_component_bit_offset
#define Set_Component_Size		einfo__set_component_size
#define Set_Esize			einfo__set_esize
#define Set_Mechanism			einfo__set_mechanism
#define Set_Normalized_First_Bit	einfo__set_normalized_first_bit
#define Set_Normalized_Position		einfo__set_normalized_position
#define Set_RM_Size			einfo__set_rm_size

extern void Set_Alignment		(Entity_Id, Uint);
extern void Set_Component_Bit_Offset	(Entity_Id, Uint);
extern void Set_Component_Size		(Entity_Id, Uint);
extern void Set_Esize			(Entity_Id, Uint);
extern void Set_Mechanism		(Entity_Id, Mechanism_Type);
extern void Set_Normalized_First_Bit	(Entity_Id, Uint);
extern void Set_Normalized_Position	(Entity_Id, Uint);
extern void Set_RM_Size			(Entity_Id, Uint);

#define Is_Entity_Name		einfo__is_entity_name

extern Boolean Is_Entity_Name		(Node_Id);

#define Get_Attribute_Definition_Clause	einfo__get_attribute_definition_clause

extern Node_Id Get_Attribute_Definition_Clause (Entity_Id, unsigned char);

/* errout: */

#define Error_Msg_N		errout__error_msg_n
#define Error_Msg_NE		errout__error_msg_ne
#define Set_Identifier_Casing	errout__set_identifier_casing

extern void Error_Msg_N			(String_Pointer, Node_Id);
extern void Error_Msg_NE		(String_Pointer, Node_Id, Entity_Id);
extern void Set_Identifier_Casing	(Char *, const Char *);

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
extern void Get_RT_Exception_Name		(int);
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
extern char *Clobber_Get_Next		(void);
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
#define Back_End_Inlining		opt__back_end_inlining
#define Debug_Generated_Code		opt__debug_generated_code
#define Exception_Extra_Info		opt__exception_extra_info
#define Exception_Locations_Suppressed	opt__exception_locations_suppressed
#define Exception_Mechanism		opt__exception_mechanism
#define Generate_SCO_Instance_Table	opt__generate_sco_instance_table
#define GNAT_Mode			opt__gnat_mode
#define List_Representation_Info	opt__list_representation_info
#define No_Strict_Aliasing_CP		opt__no_strict_aliasing
#define Suppress_Checks			opt__suppress_checks

typedef enum {
  Ada_83, Ada_95, Ada_2005, Ada_2012, Ada_2020
} Ada_Version_Type;

typedef enum {
  Front_End_SJLJ, Back_End_ZCX, Back_End_SJLJ
} Exception_Mechanism_Type;

extern Ada_Version_Type Ada_Version;
extern Boolean Back_End_Inlining;
extern Boolean Debug_Generated_Code;
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

extern void Check_Elaboration_Code_Allowed	(Node_Id);
extern void Check_Implicit_Dynamic_Code_Allowed	(Node_Id);
extern void Check_No_Implicit_Heap_Alloc	(Node_Id);
extern void Check_No_Implicit_Protected_Alloc	(Node_Id);
extern void Check_No_Implicit_Task_Alloc	(Node_Id);
extern Boolean No_Exception_Handlers_Set	(void);

/* sem_aggr:  */

#define Is_Others_Aggregate	sem_aggr__is_others_aggregate

extern Boolean Is_Others_Aggregate	(Node_Id);

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
#define Is_OK_Static_Expression		sem_eval__is_ok_static_expression

extern Boolean Compile_Time_Known_Value	(Node_Id);
extern Boolean Is_OK_Static_Expression	(Node_Id);

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

#define End_Location			sinfo__end_location
#define Set_Has_No_Elaboration_Code	sinfo__set_has_no_elaboration_code
#define Set_Present_Expr		sinfo__set_present_expr

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

#ifdef __cplusplus
}
#endif
