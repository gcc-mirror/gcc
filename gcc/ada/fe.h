/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                    FE                                    *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2009, Free Software Foundation, Inc.         *
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

/* This file contains definitions to access front-end functions and
   variables used by gigi.  */

/* comperr:  */

#define Compiler_Abort comperr__compiler_abort
extern int Compiler_Abort (Fat_Pointer, int, Fat_Pointer) ATTRIBUTE_NORETURN;

/* csets: */

#define Fold_Lower(C) csets__fold_lower[C]
#define Fold_Upper(C) csets__fold_upper[C]
extern char Fold_Lower[], Fold_Upper[];

/* debug: */

#define Debug_Flag_NN debug__debug_flag_nn
extern Boolean Debug_Flag_NN;

/* einfo: We will be setting Esize for types, Component_Bit_Offset for fields,
   Alignment for types and objects, Component_Size for array types, and
   Present_Expr for N_Variant nodes.  */

#define Set_Alignment			einfo__set_alignment
#define Set_Component_Bit_Offset	einfo__set_component_bit_offset
#define Set_Component_Size		einfo__set_component_size
#define Set_Esize			einfo__set_esize
#define Set_Mechanism			einfo__set_mechanism
#define Set_RM_Size			einfo__set_rm_size
#define Set_Present_Expr		sinfo__set_present_expr

extern void Set_Alignment		(Entity_Id, Uint);
extern void Set_Component_Bit_Offset	(Entity_Id, Uint);
extern void Set_Component_Size		(Entity_Id, Uint);
extern void Set_Esize			(Entity_Id, Uint);
extern void Set_Mechanism		(Entity_Id, Mechanism_Type);
extern void Set_RM_Size			(Entity_Id, Uint);
extern void Set_Present_Expr		(Node_Id, Uint);

/* Test if the node N is the name of an entity (i.e. is an identifier,
   expanded name, or an attribute reference that returns an entity).  */
#define Is_Entity_Name einfo__is_entity_name
extern Boolean Is_Entity_Name		(Node_Id);

#define Get_Attribute_Definition_Clause einfo__get_attribute_definition_clause
extern Node_Id Get_Attribute_Definition_Clause (Entity_Id, char);

/* errout: */

#define Error_Msg_N               errout__error_msg_n
#define Error_Msg_NE              errout__error_msg_ne
#define Set_Identifier_Casing     errout__set_identifier_casing

extern void Error_Msg_N	          (Fat_Pointer, Node_Id);
extern void Error_Msg_NE          (Fat_Pointer, Node_Id, Entity_Id);
extern void Set_Identifier_Casing (Char *, const Char *);

/* err_vars: */

#define Error_Msg_Node_2     err_vars__error_msg_node_2
#define Error_Msg_Uint_1     err_vars__error_msg_uint_1
#define Error_Msg_Uint_2     err_vars__error_msg_uint_2

extern Entity_Id             Error_Msg_Node_2;
extern Uint                  Error_Msg_Uint_1;
extern Uint                  Error_Msg_Uint_2;

/* exp_ch11:  */

#define Get_Local_Raise_Call_Entity exp_ch11__get_local_raise_call_entity
#define Get_RT_Exception_Entity exp_ch11__get_rt_exception_entity

extern Entity_Id Get_Local_Raise_Call_Entity (void);
extern Entity_Id Get_RT_Exception_Entity (int);

/* exp_code:  */

#define Asm_Input_Constraint exp_code__asm_input_constraint
#define Asm_Input_Value exp_code__asm_input_value
#define Asm_Output_Constraint exp_code__asm_output_constraint
#define Asm_Output_Variable exp_code__asm_output_variable
#define Asm_Template exp_code__asm_template
#define Clobber_Get_Next exp_code__clobber_get_next
#define Clobber_Setup exp_code__clobber_setup
#define Is_Asm_Volatile exp_code__is_asm_volatile
#define Next_Asm_Input exp_code__next_asm_input
#define Next_Asm_Output exp_code__next_asm_output
#define Setup_Asm_Inputs exp_code__setup_asm_inputs
#define Setup_Asm_Outputs exp_code__setup_asm_outputs

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

#define Get_Encoded_Name exp_dbug__get_encoded_name
#define Get_External_Name exp_dbug__get_external_name
#define Get_External_Name_With_Suffix exp_dbug__get_external_name_with_suffix

extern void Get_Encoded_Name			(Entity_Id);
extern void Get_External_Name			(Entity_Id, Boolean);
extern void Get_External_Name_With_Suffix	(Entity_Id, Fat_Pointer);

/* exp_util: */

#define Is_Fully_Repped_Tagged_Type exp_util__is_fully_repped_tagged_type

extern Boolean Is_Fully_Repped_Tagged_Type      (Entity_Id);

/* lib: */

#define Cunit 				lib__cunit
#define Ident_String			lib__ident_string
#define In_Extended_Main_Code_Unit	lib__in_extended_main_code_unit
#define In_Same_Source_Unit             lib__in_same_source_unit

extern Node_Id Cunit				(Unit_Number_Type);
extern Node_Id Ident_String			(Unit_Number_Type);
extern Boolean In_Extended_Main_Code_Unit	(Entity_Id);
extern Boolean In_Same_Source_Unit              (Node_Id, Node_Id);

/* opt: */

#define Global_Discard_Names           opt__global_discard_names
#define Exception_Locations_Suppressed opt__exception_locations_suppressed
#define Exception_Mechanism            opt__exception_mechanism
#define Back_Annotate_Rep_Info         opt__back_annotate_rep_info

typedef enum {Setjmp_Longjmp, Back_End_Exceptions} Exception_Mechanism_Type;

extern Boolean Global_Discard_Names;
extern Boolean Exception_Locations_Suppressed;
extern Exception_Mechanism_Type Exception_Mechanism;
extern Boolean Back_Annotate_Rep_Info;

/* restrict: */

#define No_Exception_Handlers_Set      restrict__no_exception_handlers_set
#define Check_No_Implicit_Heap_Alloc   restrict__check_no_implicit_heap_alloc
#define Check_Elaboration_Code_Allowed restrict__check_elaboration_code_allowed
#define Check_Implicit_Dynamic_Code_Allowed restrict__check_implicit_dynamic_code_allowed

extern Boolean No_Exception_Handlers_Set   (void);
extern void Check_No_Implicit_Heap_Alloc   (Node_Id);
extern void Check_Elaboration_Code_Allowed (Node_Id);
extern void Check_Implicit_Dynamic_Code_Allowed (Node_Id);

/* sem_aux:  */

#define Ancestor_Subtype               sem_aux__ancestor_subtype
#define First_Discriminant             sem_aux__first_discriminant
#define First_Stored_Discriminant      sem_aux__first_stored_discriminant
#define First_Subtype                  sem_aux__first_subtype
#define Is_By_Reference_Type           sem_aux__is_by_reference_type
#define Is_Derived_Type                sem_aux__is_derived_type

extern Entity_Id  Ancestor_Subtype             (Entity_Id);
extern Entity_Id  First_Discriminant           (Entity_Id);
extern Entity_Id  First_Stored_Discriminant    (Entity_Id);
extern Entity_Id  First_Subtype                (Entity_Id);
extern Boolean    Is_By_Reference_Type         (Entity_Id);
extern Boolean    Is_Derived_Type              (Entity_Id);

/* sem_elim: */

#define Eliminate_Error_Msg    sem_elim__eliminate_error_msg

extern void Eliminate_Error_Msg (Node_Id, Entity_Id);

/* sem_eval: */

#define Compile_Time_Known_Value	sem_eval__compile_time_known_value
#define Expr_Value			sem_eval__expr_value
#define Expr_Value_S			sem_eval__expr_value_s
#define Is_OK_Static_Expression		sem_eval__is_ok_static_expression
#define Is_OK_Static_Subtype		sem_eval__is_ok_static_subtype

extern Uint Expr_Value			(Node_Id);
extern Node_Id Expr_Value_S		(Node_Id);
extern Boolean Compile_Time_Known_Value (Node_Id);
extern Boolean Is_OK_Static_Expression  (Node_Id);
extern Boolean Is_OK_Static_Subtype	(Entity_Id);

/* sem_util: */

#define Defining_Entity			sem_util__defining_entity
#define First_Actual			sem_util__first_actual
#define Next_Actual			sem_util__next_actual
#define Requires_Transient_Scope	sem_util__requires_transient_scope

extern Entity_Id Defining_Entity	(Node_Id);
extern Node_Id First_Actual		(Node_Id);
extern Node_Id Next_Actual		(Node_Id);
extern Boolean Requires_Transient_Scope (Entity_Id);

/* sinfo: These functions aren't in sinfo.h since we don't make the
   setting functions, just the retrieval functions.  */

#define Set_Has_No_Elaboration_Code sinfo__set_has_no_elaboration_code
extern void Set_Has_No_Elaboration_Code	(Node_Id, Boolean);

/* targparm: */

#define Backend_Overflow_Checks_On_Target targparm__backend_overflow_checks_on_target
#define Stack_Check_Probes_On_Target targparm__stack_check_probes_on_target
#define Stack_Check_Limits_On_Target targparm__stack_check_limits_on_target

extern Boolean Backend_Overflow_Checks_On_Target;
extern Boolean Stack_Check_Probes_On_Target;
extern Boolean Stack_Check_Limits_On_Target;
