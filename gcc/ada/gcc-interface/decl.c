/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                 D E C L                                  *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2011, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have received a copy of the GNU General   *
 * Public License along with GCC; see the file COPYING3.  If not see        *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "toplev.h"
#include "ggc.h"
#include "target.h"
#include "tree-inline.h"

#include "ada.h"
#include "types.h"
#include "atree.h"
#include "elists.h"
#include "namet.h"
#include "nlists.h"
#include "repinfo.h"
#include "snames.h"
#include "stringt.h"
#include "uintp.h"
#include "fe.h"
#include "sinfo.h"
#include "einfo.h"
#include "ada-tree.h"
#include "gigi.h"

/* Convention_Stdcall should be processed in a specific way on 32 bits
   Windows targets only.  The macro below is a helper to avoid having to
   check for a Windows specific attribute throughout this unit.  */

#if TARGET_DLLIMPORT_DECL_ATTRIBUTES
#ifdef TARGET_64BIT
#define Has_Stdcall_Convention(E) \
  (!TARGET_64BIT && Convention (E) == Convention_Stdcall)
#else
#define Has_Stdcall_Convention(E) (Convention (E) == Convention_Stdcall)
#endif
#else
#define Has_Stdcall_Convention(E) 0
#endif

/* Stack realignment is necessary for functions with foreign conventions when
   the ABI doesn't mandate as much as what the compiler assumes - that is, up
   to PREFERRED_STACK_BOUNDARY.

   Such realignment can be requested with a dedicated function type attribute
   on the targets that support it.  We define FOREIGN_FORCE_REALIGN_STACK to
   characterize the situations where the attribute should be set.  We rely on
   compiler configuration settings for 'main' to decide.  */

#ifdef MAIN_STACK_BOUNDARY
#define FOREIGN_FORCE_REALIGN_STACK \
  (MAIN_STACK_BOUNDARY < PREFERRED_STACK_BOUNDARY)
#else
#define FOREIGN_FORCE_REALIGN_STACK 0
#endif

struct incomplete
{
  struct incomplete *next;
  tree old_type;
  Entity_Id full_type;
};

/* These variables are used to defer recursively expanding incomplete types
   while we are processing an array, a record or a subprogram type.  */
static int defer_incomplete_level = 0;
static struct incomplete *defer_incomplete_list;

/* This variable is used to delay expanding From_With_Type types until the
   end of the spec.  */
static struct incomplete *defer_limited_with;

/* These variables are used to defer finalizing types.  The element of the
   list is the TYPE_DECL associated with the type.  */
static int defer_finalize_level = 0;
static VEC (tree,heap) *defer_finalize_list;

typedef struct subst_pair_d {
  tree discriminant;
  tree replacement;
} subst_pair;

DEF_VEC_O(subst_pair);
DEF_VEC_ALLOC_O(subst_pair,heap);

typedef struct variant_desc_d {
  /* The type of the variant.  */
  tree type;

  /* The associated field.  */
  tree field;

  /* The value of the qualifier.  */
  tree qual;

  /* The record associated with this variant.  */
  tree record;
} variant_desc;

DEF_VEC_O(variant_desc);
DEF_VEC_ALLOC_O(variant_desc,heap);

/* A hash table used to cache the result of annotate_value.  */
static GTY ((if_marked ("tree_int_map_marked_p"),
	     param_is (struct tree_int_map))) htab_t annotate_value_cache;

enum alias_set_op
{
  ALIAS_SET_COPY,
  ALIAS_SET_SUBSET,
  ALIAS_SET_SUPERSET
};

static void relate_alias_sets (tree, tree, enum alias_set_op);

static bool allocatable_size_p (tree, bool);
static void prepend_one_attribute_to (struct attrib **,
				      enum attr_type, tree, tree, Node_Id);
static void prepend_attributes (Entity_Id, struct attrib **);
static tree elaborate_expression (Node_Id, Entity_Id, tree, bool, bool, bool);
static bool is_variable_size (tree);
static tree elaborate_expression_1 (tree, Entity_Id, tree, bool, bool);
static tree elaborate_expression_2 (tree, Entity_Id, tree, bool, bool,
				    unsigned int);
static tree make_packable_type (tree, bool);
static tree gnat_to_gnu_component_type (Entity_Id, bool, bool);
static tree gnat_to_gnu_param (Entity_Id, Mechanism_Type, Entity_Id, bool,
			       bool *);
static tree gnat_to_gnu_field (Entity_Id, tree, int, bool, bool);
static bool same_discriminant_p (Entity_Id, Entity_Id);
static bool array_type_has_nonaliased_component (tree, Entity_Id);
static bool compile_time_known_address_p (Node_Id);
static bool cannot_be_superflat_p (Node_Id);
static bool constructor_address_p (tree);
static void components_to_record (tree, Node_Id, tree, int, bool, bool, bool,
				  bool, bool, bool, bool, tree, tree *);
static Uint annotate_value (tree);
static void annotate_rep (Entity_Id, tree);
static tree build_position_list (tree, bool, tree, tree, unsigned int, tree);
static VEC(subst_pair,heap) *build_subst_list (Entity_Id, Entity_Id, bool);
static VEC(variant_desc,heap) *build_variant_list (tree,
						   VEC(subst_pair,heap) *,
						   VEC(variant_desc,heap) *);
static tree validate_size (Uint, tree, Entity_Id, enum tree_code, bool, bool);
static void set_rm_size (Uint, tree, Entity_Id);
static tree make_type_from_size (tree, tree, bool);
static unsigned int validate_alignment (Uint, Entity_Id, unsigned int);
static unsigned int ceil_alignment (unsigned HOST_WIDE_INT);
static void check_ok_for_atomic (tree, Entity_Id, bool);
static tree create_field_decl_from (tree, tree, tree, tree, tree,
				    VEC(subst_pair,heap) *);
static tree create_rep_part (tree, tree, tree);
static tree get_rep_part (tree);
static tree create_variant_part_from (tree, VEC(variant_desc,heap) *, tree,
				      tree, VEC(subst_pair,heap) *);
static void copy_and_substitute_in_size (tree, tree, VEC(subst_pair,heap) *);
static void rest_of_type_decl_compilation_no_defer (tree);

/* The relevant constituents of a subprogram binding to a GCC builtin.  Used
   to pass around calls performing profile compatibility checks.  */

typedef struct {
  Entity_Id gnat_entity;  /* The Ada subprogram entity.  */
  tree ada_fntype;        /* The corresponding GCC type node.  */
  tree btin_fntype;       /* The GCC builtin function type node.  */
} intrin_binding_t;

static bool intrin_profiles_compatible_p (intrin_binding_t *);

/* Given GNAT_ENTITY, a GNAT defining identifier node, which denotes some Ada
   entity, return the equivalent GCC tree for that entity (a ..._DECL node)
   and associate the ..._DECL node with the input GNAT defining identifier.

   If GNAT_ENTITY is a variable or a constant declaration, GNU_EXPR gives its
   initial value (in GCC tree form).  This is optional for a variable.  For
   a renamed entity, GNU_EXPR gives the object being renamed.

   DEFINITION is nonzero if this call is intended for a definition.  This is
   used for separate compilation where it is necessary to know whether an
   external declaration or a definition must be created if the GCC equivalent
   was not created previously.  The value of 1 is normally used for a nonzero
   DEFINITION, but a value of 2 is used in special circumstances, defined in
   the code.  */

tree
gnat_to_gnu_entity (Entity_Id gnat_entity, tree gnu_expr, int definition)
{
  /* Contains the kind of the input GNAT node.  */
  const Entity_Kind kind = Ekind (gnat_entity);
  /* True if this is a type.  */
  const bool is_type = IN (kind, Type_Kind);
  /* True if debug info is requested for this entity.  */
  const bool debug_info_p = Needs_Debug_Info (gnat_entity);
  /* True if this entity is to be considered as imported.  */
  const bool imported_p
    = (Is_Imported (gnat_entity) && No (Address_Clause (gnat_entity)));
  /* For a type, contains the equivalent GNAT node to be used in gigi.  */
  Entity_Id gnat_equiv_type = Empty;
  /* Temporary used to walk the GNAT tree.  */
  Entity_Id gnat_temp;
  /* Contains the GCC DECL node which is equivalent to the input GNAT node.
     This node will be associated with the GNAT node by calling at the end
     of the `switch' statement.  */
  tree gnu_decl = NULL_TREE;
  /* Contains the GCC type to be used for the GCC node.  */
  tree gnu_type = NULL_TREE;
  /* Contains the GCC size tree to be used for the GCC node.  */
  tree gnu_size = NULL_TREE;
  /* Contains the GCC name to be used for the GCC node.  */
  tree gnu_entity_name;
  /* True if we have already saved gnu_decl as a GNAT association.  */
  bool saved = false;
  /* True if we incremented defer_incomplete_level.  */
  bool this_deferred = false;
  /* True if we incremented force_global.  */
  bool this_global = false;
  /* True if we should check to see if elaborated during processing.  */
  bool maybe_present = false;
  /* True if we made GNU_DECL and its type here.  */
  bool this_made_decl = false;
  /* Size and alignment of the GCC node, if meaningful.  */
  unsigned int esize = 0, align = 0;
  /* Contains the list of attributes directly attached to the entity.  */
  struct attrib *attr_list = NULL;

  /* Since a use of an Itype is a definition, process it as such if it
     is not in a with'ed unit.  */
  if (!definition
      && is_type
      && Is_Itype (gnat_entity)
      && !present_gnu_tree (gnat_entity)
      && In_Extended_Main_Code_Unit (gnat_entity))
    {
      /* Ensure that we are in a subprogram mentioned in the Scope chain of
	 this entity, our current scope is global, or we encountered a task
	 or entry (where we can't currently accurately check scoping).  */
      if (!current_function_decl
	  || DECL_ELABORATION_PROC_P (current_function_decl))
	{
	  process_type (gnat_entity);
	  return get_gnu_tree (gnat_entity);
	}

      for (gnat_temp = Scope (gnat_entity);
	   Present (gnat_temp);
	   gnat_temp = Scope (gnat_temp))
	{
	  if (Is_Type (gnat_temp))
	    gnat_temp = Underlying_Type (gnat_temp);

	  if (Ekind (gnat_temp) == E_Subprogram_Body)
	    gnat_temp
	      = Corresponding_Spec (Parent (Declaration_Node (gnat_temp)));

	  if (IN (Ekind (gnat_temp), Subprogram_Kind)
	      && Present (Protected_Body_Subprogram (gnat_temp)))
	    gnat_temp = Protected_Body_Subprogram (gnat_temp);

	  if (Ekind (gnat_temp) == E_Entry
	      || Ekind (gnat_temp) == E_Entry_Family
	      || Ekind (gnat_temp) == E_Task_Type
	      || (IN (Ekind (gnat_temp), Subprogram_Kind)
		  && present_gnu_tree (gnat_temp)
		  && (current_function_decl
		      == gnat_to_gnu_entity (gnat_temp, NULL_TREE, 0))))
	    {
	      process_type (gnat_entity);
	      return get_gnu_tree (gnat_entity);
	    }
	}

      /* This abort means the Itype has an incorrect scope, i.e. that its
	 scope does not correspond to the subprogram it is declared in.  */
      gcc_unreachable ();
    }

  /* If we've already processed this entity, return what we got last time.
     If we are defining the node, we should not have already processed it.
     In that case, we will abort below when we try to save a new GCC tree
     for this object.  We also need to handle the case of getting a dummy
     type when a Full_View exists.  */
  if ((!definition || (is_type && imported_p))
      && present_gnu_tree (gnat_entity))
    {
      gnu_decl = get_gnu_tree (gnat_entity);

      if (TREE_CODE (gnu_decl) == TYPE_DECL
	  && TYPE_IS_DUMMY_P (TREE_TYPE (gnu_decl))
	  && IN (kind, Incomplete_Or_Private_Kind)
	  && Present (Full_View (gnat_entity)))
	{
	  gnu_decl
	    = gnat_to_gnu_entity (Full_View (gnat_entity), NULL_TREE, 0);
	  save_gnu_tree (gnat_entity, NULL_TREE, false);
	  save_gnu_tree (gnat_entity, gnu_decl, false);
	}

      return gnu_decl;
    }

  /* If this is a numeric or enumeral type, or an access type, a nonzero
     Esize must be specified unless it was specified by the programmer.  */
  gcc_assert (!Unknown_Esize (gnat_entity)
	      || Has_Size_Clause (gnat_entity)
	      || (!IN (kind, Numeric_Kind)
		  && !IN (kind, Enumeration_Kind)
		  && (!IN (kind, Access_Kind)
		      || kind == E_Access_Protected_Subprogram_Type
		      || kind == E_Anonymous_Access_Protected_Subprogram_Type
		      || kind == E_Access_Subtype)));

  /* The RM size must be specified for all discrete and fixed-point types.  */
  gcc_assert (!(IN (kind, Discrete_Or_Fixed_Point_Kind)
		&& Unknown_RM_Size (gnat_entity)));

  /* If we get here, it means we have not yet done anything with this entity.
     If we are not defining it, it must be a type or an entity that is defined
     elsewhere or externally, otherwise we should have defined it already.  */
  gcc_assert (definition
	      || type_annotate_only
	      || is_type
	      || kind == E_Discriminant
	      || kind == E_Component
	      || kind == E_Label
	      || (kind == E_Constant && Present (Full_View (gnat_entity)))
	      || Is_Public (gnat_entity));

  /* Get the name of the entity and set up the line number and filename of
     the original definition for use in any decl we make.  */
  gnu_entity_name = get_entity_name (gnat_entity);
  Sloc_to_locus (Sloc (gnat_entity), &input_location);

  /* For cases when we are not defining (i.e., we are referencing from
     another compilation unit) public entities, show we are at global level
     for the purpose of computing scopes.  Don't do this for components or
     discriminants since the relevant test is whether or not the record is
     being defined.  Don't do this for constants either as we'll look into
     their defining expression in the local context.  */
  if (!definition
      && kind != E_Component
      && kind != E_Discriminant
      && kind != E_Constant
      && Is_Public (gnat_entity)
      && !Is_Statically_Allocated (gnat_entity))
    force_global++, this_global = true;

  /* Handle any attributes directly attached to the entity.  */
  if (Has_Gigi_Rep_Item (gnat_entity))
    prepend_attributes (gnat_entity, &attr_list);

  /* Do some common processing for types.  */
  if (is_type)
    {
      /* Compute the equivalent type to be used in gigi.  */
      gnat_equiv_type = Gigi_Equivalent_Type (gnat_entity);

      /* Machine_Attributes on types are expected to be propagated to
	 subtypes.  The corresponding Gigi_Rep_Items are only attached
	 to the first subtype though, so we handle the propagation here.  */
      if (Base_Type (gnat_entity) != gnat_entity
	  && !Is_First_Subtype (gnat_entity)
	  && Has_Gigi_Rep_Item (First_Subtype (Base_Type (gnat_entity))))
	prepend_attributes (First_Subtype (Base_Type (gnat_entity)),
			    &attr_list);

      /* Compute a default value for the size of the type.  */
      if (Known_Esize (gnat_entity)
	  && UI_Is_In_Int_Range (Esize (gnat_entity)))
	{
	  unsigned int max_esize;
	  esize = UI_To_Int (Esize (gnat_entity));

	  if (IN (kind, Float_Kind))
	    max_esize = fp_prec_to_size (LONG_DOUBLE_TYPE_SIZE);
	  else if (IN (kind, Access_Kind))
	    max_esize = POINTER_SIZE * 2;
	  else
	    max_esize = LONG_LONG_TYPE_SIZE;

	  if (esize > max_esize)
	   esize = max_esize;
	}
    }

  switch (kind)
    {
    case E_Constant:
      /* If this is a use of a deferred constant without address clause,
	 get its full definition.  */
      if (!definition
	  && No (Address_Clause (gnat_entity))
	  && Present (Full_View (gnat_entity)))
	{
	  gnu_decl
	    = gnat_to_gnu_entity (Full_View (gnat_entity), gnu_expr, 0);
	  saved = true;
	  break;
	}

      /* If we have an external constant that we are not defining, get the
	 expression that is was defined to represent.  We may throw it away
	 later if it is not a constant.  But do not retrieve the expression
	 if it is an allocator because the designated type might be dummy
	 at this point.  */
      if (!definition
	  && !No_Initialization (Declaration_Node (gnat_entity))
	  && Present (Expression (Declaration_Node (gnat_entity)))
	  && Nkind (Expression (Declaration_Node (gnat_entity)))
	     != N_Allocator)
	{
	  bool went_into_elab_proc = false;

	  /* The expression may contain N_Expression_With_Actions nodes and
	     thus object declarations from other units.  In this case, even
	     though the expression will eventually be discarded since not a
	     constant, the declarations would be stuck either in the global
	     varpool or in the current scope.  Therefore we force the local
	     context and create a fake scope that we'll zap at the end.  */
	  if (!current_function_decl)
	    {
	      current_function_decl = get_elaboration_procedure ();
	      went_into_elab_proc = true;
	    }
	  gnat_pushlevel ();

	  gnu_expr = gnat_to_gnu (Expression (Declaration_Node (gnat_entity)));

	  gnat_zaplevel ();
	  if (went_into_elab_proc)
	    current_function_decl = NULL_TREE;
	}

      /* Ignore deferred constant definitions without address clause since
	 they are processed fully in the front-end.  If No_Initialization
	 is set, this is not a deferred constant but a constant whose value
	 is built manually.  And constants that are renamings are handled
	 like variables.  */
      if (definition
	  && !gnu_expr
	  && No (Address_Clause (gnat_entity))
	  && !No_Initialization (Declaration_Node (gnat_entity))
	  && No (Renamed_Object (gnat_entity)))
	{
	  gnu_decl = error_mark_node;
	  saved = true;
	  break;
	}

      /* Ignore constant definitions already marked with the error node.  See
	 the N_Object_Declaration case of gnat_to_gnu for the rationale.  */
      if (definition
	  && gnu_expr
	  && present_gnu_tree (gnat_entity)
	  && get_gnu_tree (gnat_entity) == error_mark_node)
	{
	  maybe_present = true;
	  break;
	}

      goto object;

    case E_Exception:
      /* We used to special case VMS exceptions here to directly map them to
	 their associated condition code.  Since this code had to be masked
	 dynamically to strip off the severity bits, this caused trouble in
	 the GCC/ZCX case because the "type" pointers we store in the tables
	 have to be static.  We now don't special case here anymore, and let
	 the regular processing take place, which leaves us with a regular
	 exception data object for VMS exceptions too.  The condition code
	 mapping is taken care of by the front end and the bitmasking by the
	 run-time library.  */
      goto object;

    case E_Discriminant:
    case E_Component:
      {
	/* The GNAT record where the component was defined.  */
	Entity_Id gnat_record = Underlying_Type (Scope (gnat_entity));

	/* If the variable is an inherited record component (in the case of
	   extended record types), just return the inherited entity, which
	   must be a FIELD_DECL.  Likewise for discriminants.
	   For discriminants of untagged records which have explicit
	   stored discriminants, return the entity for the corresponding
	   stored discriminant.  Also use Original_Record_Component
	   if the record has a private extension.  */
	if (Present (Original_Record_Component (gnat_entity))
	    && Original_Record_Component (gnat_entity) != gnat_entity)
	  {
	    gnu_decl
	      = gnat_to_gnu_entity (Original_Record_Component (gnat_entity),
				    gnu_expr, definition);
	    saved = true;
	    break;
	  }

	/* If the enclosing record has explicit stored discriminants,
	   then it is an untagged record.  If the Corresponding_Discriminant
	   is not empty then this must be a renamed discriminant and its
	   Original_Record_Component must point to the corresponding explicit
	   stored discriminant (i.e. we should have taken the previous
	   branch).  */
	else if (Present (Corresponding_Discriminant (gnat_entity))
		 && Is_Tagged_Type (gnat_record))
	  {
	    /* A tagged record has no explicit stored discriminants.  */
	    gcc_assert (First_Discriminant (gnat_record)
		       == First_Stored_Discriminant (gnat_record));
	    gnu_decl
	      = gnat_to_gnu_entity (Corresponding_Discriminant (gnat_entity),
				    gnu_expr, definition);
	    saved = true;
	    break;
	  }

	else if (Present (CR_Discriminant (gnat_entity))
		 && type_annotate_only)
	  {
	    gnu_decl = gnat_to_gnu_entity (CR_Discriminant (gnat_entity),
					   gnu_expr, definition);
	    saved = true;
	    break;
	  }

	/* If the enclosing record has explicit stored discriminants, then
	   it is an untagged record.  If the Corresponding_Discriminant
	   is not empty then this must be a renamed discriminant and its
	   Original_Record_Component must point to the corresponding explicit
	   stored discriminant (i.e. we should have taken the first
	   branch).  */
	else if (Present (Corresponding_Discriminant (gnat_entity))
		 && (First_Discriminant (gnat_record)
		     != First_Stored_Discriminant (gnat_record)))
	  gcc_unreachable ();

	/* Otherwise, if we are not defining this and we have no GCC type
	   for the containing record, make one for it.  Then we should
	   have made our own equivalent.  */
	else if (!definition && !present_gnu_tree (gnat_record))
	  {
	    /* ??? If this is in a record whose scope is a protected
	       type and we have an Original_Record_Component, use it.
	       This is a workaround for major problems in protected type
	       handling.  */
	    Entity_Id Scop = Scope (Scope (gnat_entity));
	    if ((Is_Protected_Type (Scop)
		 || (Is_Private_Type (Scop)
		     && Present (Full_View (Scop))
		     && Is_Protected_Type (Full_View (Scop))))
		&& Present (Original_Record_Component (gnat_entity)))
	      {
		gnu_decl
		  = gnat_to_gnu_entity (Original_Record_Component
					(gnat_entity),
					gnu_expr, 0);
		saved = true;
		break;
	      }

	    gnat_to_gnu_entity (Scope (gnat_entity), NULL_TREE, 0);
	    gnu_decl = get_gnu_tree (gnat_entity);
	    saved = true;
	    break;
	  }

	else
	  /* Here we have no GCC type and this is a reference rather than a
	     definition.  This should never happen.  Most likely the cause is
	     reference before declaration in the gnat tree for gnat_entity.  */
	  gcc_unreachable ();
      }

    case E_Loop_Parameter:
    case E_Out_Parameter:
    case E_Variable:

      /* Simple variables, loop variables, Out parameters and exceptions.  */
    object:
      {
	bool const_flag
	  = ((kind == E_Constant || kind == E_Variable)
	     && Is_True_Constant (gnat_entity)
	     && !Treat_As_Volatile (gnat_entity)
	     && (((Nkind (Declaration_Node (gnat_entity))
		   == N_Object_Declaration)
		  && Present (Expression (Declaration_Node (gnat_entity))))
		 || Present (Renamed_Object (gnat_entity))
		 || imported_p));
	bool inner_const_flag = const_flag;
	bool static_p = Is_Statically_Allocated (gnat_entity);
	bool mutable_p = false;
	bool used_by_ref = false;
	tree gnu_ext_name = NULL_TREE;
	tree renamed_obj = NULL_TREE;
	tree gnu_object_size;

	if (Present (Renamed_Object (gnat_entity)) && !definition)
	  {
	    if (kind == E_Exception)
	      gnu_expr = gnat_to_gnu_entity (Renamed_Entity (gnat_entity),
					     NULL_TREE, 0);
	    else
	      gnu_expr = gnat_to_gnu (Renamed_Object (gnat_entity));
	  }

	/* Get the type after elaborating the renamed object.  */
	gnu_type = gnat_to_gnu_type (Etype (gnat_entity));

	/* If this is a standard exception definition, then use the standard
	   exception type.  This is necessary to make sure that imported and
	   exported views of exceptions are properly merged in LTO mode.  */
	if (TREE_CODE (TYPE_NAME (gnu_type)) == TYPE_DECL
	    && DECL_NAME (TYPE_NAME (gnu_type)) == exception_data_name_id)
	  gnu_type = except_type_node;

	/* For a debug renaming declaration, build a debug-only entity.  */
	if (Present (Debug_Renaming_Link (gnat_entity)))
	  {
	    /* Force a non-null value to make sure the symbol is retained.  */
	    tree value = build1 (INDIRECT_REF, gnu_type,
				 build1 (NOP_EXPR,
					 build_pointer_type (gnu_type),
					 integer_minus_one_node));
	    gnu_decl = build_decl (input_location,
				   VAR_DECL, gnu_entity_name, gnu_type);
	    SET_DECL_VALUE_EXPR (gnu_decl, value);
	    DECL_HAS_VALUE_EXPR_P (gnu_decl) = 1;
	    gnat_pushdecl (gnu_decl, gnat_entity);
	    break;
	  }

	/* If this is a loop variable, its type should be the base type.
	   This is because the code for processing a loop determines whether
	   a normal loop end test can be done by comparing the bounds of the
	   loop against those of the base type, which is presumed to be the
	   size used for computation.  But this is not correct when the size
	   of the subtype is smaller than the type.  */
	if (kind == E_Loop_Parameter)
	  gnu_type = get_base_type (gnu_type);

	/* Reject non-renamed objects whose type is an unconstrained array or
	   any object whose type is a dummy type or void.  */
	if ((TREE_CODE (gnu_type) == UNCONSTRAINED_ARRAY_TYPE
	     && No (Renamed_Object (gnat_entity)))
	    || TYPE_IS_DUMMY_P (gnu_type)
	    || TREE_CODE (gnu_type) == VOID_TYPE)
	  {
	    gcc_assert (type_annotate_only);
	    if (this_global)
	      force_global--;
	    return error_mark_node;
	  }

	/* If an alignment is specified, use it if valid.  Note that exceptions
	   are objects but don't have an alignment.  We must do this before we
	   validate the size, since the alignment can affect the size.  */
	if (kind != E_Exception && Known_Alignment (gnat_entity))
	  {
	    gcc_assert (Present (Alignment (gnat_entity)));

	    align = validate_alignment (Alignment (gnat_entity), gnat_entity,
					TYPE_ALIGN (gnu_type));

	    /* No point in changing the type if there is an address clause
	       as the final type of the object will be a reference type.  */
	    if (Present (Address_Clause (gnat_entity)))
	      align = 0;
	    else
	      {
		tree orig_type = gnu_type;

		gnu_type
		  = maybe_pad_type (gnu_type, NULL_TREE, align, gnat_entity,
				    false, false, definition, true);

		/* If a padding record was made, declare it now since it will
		   never be declared otherwise.  This is necessary to ensure
		   that its subtrees are properly marked.  */
		if (gnu_type != orig_type && !DECL_P (TYPE_NAME (gnu_type)))
		  create_type_decl (TYPE_NAME (gnu_type), gnu_type, NULL, true,
				    debug_info_p, gnat_entity);
	      }
	  }

	/* If we are defining the object, see if it has a Size and validate it
	   if so.  If we are not defining the object and a Size clause applies,
	   simply retrieve the value.  We don't want to ignore the clause and
	   it is expected to have been validated already.  Then get the new
	   type, if any.  */
	if (definition)
	  gnu_size = validate_size (Esize (gnat_entity), gnu_type,
				    gnat_entity, VAR_DECL, false,
				    Has_Size_Clause (gnat_entity));
	else if (Has_Size_Clause (gnat_entity))
	  gnu_size = UI_To_gnu (Esize (gnat_entity), bitsizetype);

	if (gnu_size)
	  {
	    gnu_type
	      = make_type_from_size (gnu_type, gnu_size,
				     Has_Biased_Representation (gnat_entity));

	    if (operand_equal_p (TYPE_SIZE (gnu_type), gnu_size, 0))
	      gnu_size = NULL_TREE;
	  }

	/* If this object has self-referential size, it must be a record with
	   a default discriminant.  We are supposed to allocate an object of
	   the maximum size in this case, unless it is a constant with an
	   initializing expression, in which case we can get the size from
	   that.  Note that the resulting size may still be a variable, so
	   this may end up with an indirect allocation.  */
	if (No (Renamed_Object (gnat_entity))
	    && CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_type)))
	  {
	    if (gnu_expr && kind == E_Constant)
	      {
		tree size = TYPE_SIZE (TREE_TYPE (gnu_expr));
		if (CONTAINS_PLACEHOLDER_P (size))
		  {
		    /* If the initializing expression is itself a constant,
		       despite having a nominal type with self-referential
		       size, we can get the size directly from it.  */
		    if (TREE_CODE (gnu_expr) == COMPONENT_REF
			&& TYPE_IS_PADDING_P
			   (TREE_TYPE (TREE_OPERAND (gnu_expr, 0)))
			&& TREE_CODE (TREE_OPERAND (gnu_expr, 0)) == VAR_DECL
			&& (TREE_READONLY (TREE_OPERAND (gnu_expr, 0))
			    || DECL_READONLY_ONCE_ELAB
			       (TREE_OPERAND (gnu_expr, 0))))
		      gnu_size = DECL_SIZE (TREE_OPERAND (gnu_expr, 0));
		    else
		      gnu_size
			= SUBSTITUTE_PLACEHOLDER_IN_EXPR (size, gnu_expr);
		  }
		else
		  gnu_size = size;
	      }
	    /* We may have no GNU_EXPR because No_Initialization is
	       set even though there's an Expression.  */
	    else if (kind == E_Constant
		     && (Nkind (Declaration_Node (gnat_entity))
			 == N_Object_Declaration)
		     && Present (Expression (Declaration_Node (gnat_entity))))
	      gnu_size
		= TYPE_SIZE (gnat_to_gnu_type
			     (Etype
			      (Expression (Declaration_Node (gnat_entity)))));
	    else
	      {
		gnu_size = max_size (TYPE_SIZE (gnu_type), true);
		mutable_p = true;
	      }
	  }

	/* If the size is zero byte, make it one byte since some linkers have
	   troubles with zero-sized objects.  If the object will have a
	   template, that will make it nonzero so don't bother.  Also avoid
	   doing that for an object renaming or an object with an address
	   clause, as we would lose useful information on the view size
	   (e.g. for null array slices) and we are not allocating the object
	   here anyway.  */
	if (((gnu_size
	      && integer_zerop (gnu_size)
	      && !TREE_OVERFLOW (gnu_size))
	     || (TYPE_SIZE (gnu_type)
		 && integer_zerop (TYPE_SIZE (gnu_type))
		 && !TREE_OVERFLOW (TYPE_SIZE (gnu_type))))
	    && (!Is_Constr_Subt_For_UN_Aliased (Etype (gnat_entity))
		|| !Is_Array_Type (Etype (gnat_entity)))
	    && No (Renamed_Object (gnat_entity))
	    && No (Address_Clause (gnat_entity)))
	  gnu_size = bitsize_unit_node;

	/* If this is an object with no specified size and alignment, and
	   if either it is atomic or we are not optimizing alignment for
	   space and it is composite and not an exception, an Out parameter
	   or a reference to another object, and the size of its type is a
	   constant, set the alignment to the smallest one which is not
	   smaller than the size, with an appropriate cap.  */
	if (!gnu_size && align == 0
	    && (Is_Atomic (gnat_entity)
		|| (!Optimize_Alignment_Space (gnat_entity)
		    && kind != E_Exception
		    && kind != E_Out_Parameter
		    && Is_Composite_Type (Etype (gnat_entity))
		    && !Is_Constr_Subt_For_UN_Aliased (Etype (gnat_entity))
		    && !Is_Exported (gnat_entity)
		    && !imported_p
		    && No (Renamed_Object (gnat_entity))
		    && No (Address_Clause (gnat_entity))))
	    && TREE_CODE (TYPE_SIZE (gnu_type)) == INTEGER_CST)
	  {
	    unsigned int size_cap, align_cap;

	    /* No point in promoting the alignment if this doesn't prevent
	       BLKmode access to the object, in particular block copy, as
	       this will for example disable the NRV optimization for it.
	       No point in jumping through all the hoops needed in order
	       to support BIGGEST_ALIGNMENT if we don't really have to.
	       So we cap to the smallest alignment that corresponds to
	       a known efficient memory access pattern of the target.  */
	    if (Is_Atomic (gnat_entity))
	      {
		size_cap = UINT_MAX;
		align_cap = BIGGEST_ALIGNMENT;
	      }
	    else
	      {
		size_cap = MAX_FIXED_MODE_SIZE;
		align_cap = get_mode_alignment (ptr_mode);
	      }

	    if (!host_integerp (TYPE_SIZE (gnu_type), 1)
		|| compare_tree_int (TYPE_SIZE (gnu_type), size_cap) > 0)
	      align = 0;
	    else if (compare_tree_int (TYPE_SIZE (gnu_type), align_cap) > 0)
	      align = align_cap;
	    else
	      align = ceil_alignment (tree_low_cst (TYPE_SIZE (gnu_type), 1));

	    /* But make sure not to under-align the object.  */
	    if (align <= TYPE_ALIGN (gnu_type))
	      align = 0;

	    /* And honor the minimum valid atomic alignment, if any.  */
#ifdef MINIMUM_ATOMIC_ALIGNMENT
	    else if (align < MINIMUM_ATOMIC_ALIGNMENT)
	      align = MINIMUM_ATOMIC_ALIGNMENT;
#endif
	  }

	/* If the object is set to have atomic components, find the component
	   type and validate it.

	   ??? Note that we ignore Has_Volatile_Components on objects; it's
	   not at all clear what to do in that case.  */
	if (Has_Atomic_Components (gnat_entity))
	  {
	    tree gnu_inner = (TREE_CODE (gnu_type) == ARRAY_TYPE
			      ? TREE_TYPE (gnu_type) : gnu_type);

	    while (TREE_CODE (gnu_inner) == ARRAY_TYPE
		   && TYPE_MULTI_ARRAY_P (gnu_inner))
	      gnu_inner = TREE_TYPE (gnu_inner);

	    check_ok_for_atomic (gnu_inner, gnat_entity, true);
	  }

	/* Now check if the type of the object allows atomic access.  Note
	   that we must test the type, even if this object has size and
	   alignment to allow such access, because we will be going inside
	   the padded record to assign to the object.  We could fix this by
	   always copying via an intermediate value, but it's not clear it's
	   worth the effort.  */
	if (Is_Atomic (gnat_entity))
	  check_ok_for_atomic (gnu_type, gnat_entity, false);

	/* If this is an aliased object with an unconstrained nominal subtype,
	   make a type that includes the template.  */
	if (Is_Constr_Subt_For_UN_Aliased (Etype (gnat_entity))
	    && Is_Array_Type (Etype (gnat_entity))
	    && !type_annotate_only)
	  {
	    tree gnu_fat
	      = TREE_TYPE (gnat_to_gnu_type (Base_Type (Etype (gnat_entity))));
	    gnu_type
	      = build_unc_object_type_from_ptr (gnu_fat, gnu_type,
						concat_name (gnu_entity_name,
							     "UNC"),
						debug_info_p);
	  }

#ifdef MINIMUM_ATOMIC_ALIGNMENT
	/* If the size is a constant and no alignment is specified, force
	   the alignment to be the minimum valid atomic alignment.  The
	   restriction on constant size avoids problems with variable-size
	   temporaries; if the size is variable, there's no issue with
	   atomic access.  Also don't do this for a constant, since it isn't
	   necessary and can interfere with constant replacement.  Finally,
	   do not do it for Out parameters since that creates an
	   size inconsistency with In parameters.  */
	if (align == 0 && MINIMUM_ATOMIC_ALIGNMENT > TYPE_ALIGN (gnu_type)
	    && !FLOAT_TYPE_P (gnu_type)
	    && !const_flag && No (Renamed_Object (gnat_entity))
	    && !imported_p && No (Address_Clause (gnat_entity))
	    && kind != E_Out_Parameter
	    && (gnu_size ? TREE_CODE (gnu_size) == INTEGER_CST
		: TREE_CODE (TYPE_SIZE (gnu_type)) == INTEGER_CST))
	  align = MINIMUM_ATOMIC_ALIGNMENT;
#endif

	/* Make a new type with the desired size and alignment, if needed.
	   But do not take into account alignment promotions to compute the
	   size of the object.  */
	gnu_object_size = gnu_size ? gnu_size : TYPE_SIZE (gnu_type);
	if (gnu_size || align > 0)
	  {
	    tree orig_type = gnu_type;

	    gnu_type = maybe_pad_type (gnu_type, gnu_size, align, gnat_entity,
				       false, false, definition,
				       gnu_size ? true : false);

	    /* If a padding record was made, declare it now since it will
	       never be declared otherwise.  This is necessary to ensure
	       that its subtrees are properly marked.  */
	    if (gnu_type != orig_type && !DECL_P (TYPE_NAME (gnu_type)))
	      create_type_decl (TYPE_NAME (gnu_type), gnu_type, NULL, true,
				debug_info_p, gnat_entity);
	  }

	/* If this is a renaming, avoid as much as possible to create a new
	   object.  However, in several cases, creating it is required.
	   This processing needs to be applied to the raw expression so
	   as to make it more likely to rename the underlying object.  */
	if (Present (Renamed_Object (gnat_entity)))
	  {
	    bool create_normal_object = false;

	    /* If the renamed object had padding, strip off the reference
	       to the inner object and reset our type.  */
	    if ((TREE_CODE (gnu_expr) == COMPONENT_REF
		 && TYPE_IS_PADDING_P (TREE_TYPE (TREE_OPERAND (gnu_expr, 0))))
		/* Strip useless conversions around the object.  */
		|| gnat_useless_type_conversion (gnu_expr))
	      {
		gnu_expr = TREE_OPERAND (gnu_expr, 0);
		gnu_type = TREE_TYPE (gnu_expr);
	      }

	    /* Case 1: If this is a constant renaming stemming from a function
	       call, treat it as a normal object whose initial value is what
	       is being renamed.  RM 3.3 says that the result of evaluating a
	       function call is a constant object.  As a consequence, it can
	       be the inner object of a constant renaming.  In this case, the
	       renaming must be fully instantiated, i.e. it cannot be a mere
	       reference to (part of) an existing object.  */
	    if (const_flag)
	      {
	        tree inner_object = gnu_expr;
		while (handled_component_p (inner_object))
		  inner_object = TREE_OPERAND (inner_object, 0);
		if (TREE_CODE (inner_object) == CALL_EXPR)
		  create_normal_object = true;
	      }

	    /* Otherwise, see if we can proceed with a stabilized version of
	       the renamed entity or if we need to make a new object.  */
	    if (!create_normal_object)
	      {
		tree maybe_stable_expr = NULL_TREE;
		bool stable = false;

		/* Case 2: If the renaming entity need not be materialized and
		   the renamed expression is something we can stabilize, use
		   that for the renaming.  At the global level, we can only do
		   this if we know no SAVE_EXPRs need be made, because the
		   expression we return might be used in arbitrary conditional
		   branches so we must force the evaluation of the SAVE_EXPRs
		   immediately and this requires a proper function context.
		   Note that an external constant is at the global level.  */
		if (!Materialize_Entity (gnat_entity)
		    && (!((!definition && kind == E_Constant)
			  || global_bindings_p ())
			|| (staticp (gnu_expr)
			    && !TREE_SIDE_EFFECTS (gnu_expr))))
		  {
		    maybe_stable_expr
		      = gnat_stabilize_reference (gnu_expr, true, &stable);

		    if (stable)
		      {
			/* ??? No DECL_EXPR is created so we need to mark
			   the expression manually lest it is shared.  */
			if ((!definition && kind == E_Constant)
			    || global_bindings_p ())
			  MARK_VISITED (maybe_stable_expr);
			gnu_decl = maybe_stable_expr;
			save_gnu_tree (gnat_entity, gnu_decl, true);
			saved = true;
			annotate_object (gnat_entity, gnu_type, NULL_TREE,
					 false, false);
			/* This assertion will fail if the renamed object
			   isn't aligned enough as to make it possible to
			   honor the alignment set on the renaming.  */
			if (align)
			  {
			    unsigned int renamed_align
			      = DECL_P (gnu_decl)
				? DECL_ALIGN (gnu_decl)
				: TYPE_ALIGN (TREE_TYPE (gnu_decl));
			    gcc_assert (renamed_align >= align);
			  }
			break;
		      }

		    /* The stabilization failed.  Keep maybe_stable_expr
		       untouched here to let the pointer case below know
		       about that failure.  */
		  }

		/* Case 3: If this is a constant renaming and creating a
		   new object is allowed and cheap, treat it as a normal
		   object whose initial value is what is being renamed.  */
		if (const_flag
		    && !Is_Composite_Type
		        (Underlying_Type (Etype (gnat_entity))))
		  ;

		/* Case 4: Make this into a constant pointer to the object we
		   are to rename and attach the object to the pointer if it is
		   something we can stabilize.

		   From the proper scope, attached objects will be referenced
		   directly instead of indirectly via the pointer to avoid
		   subtle aliasing problems with non-addressable entities.
		   They have to be stable because we must not evaluate the
		   variables in the expression every time the renaming is used.
		   The pointer is called a "renaming" pointer in this case.

		   In the rare cases where we cannot stabilize the renamed
		   object, we just make a "bare" pointer, and the renamed
		   entity is always accessed indirectly through it.  */
		else
		  {
		    gnu_type = build_reference_type (gnu_type);
		    inner_const_flag = TREE_READONLY (gnu_expr);
		    const_flag = true;

		    /* If the previous attempt at stabilizing failed, there
		       is no point in trying again and we reuse the result
		       without attaching it to the pointer.  In this case it
		       will only be used as the initializing expression of
		       the pointer and thus needs no special treatment with
		       regard to multiple evaluations.  */
		    if (maybe_stable_expr)
		      ;

		    /* Otherwise, try to stabilize and attach the expression
		       to the pointer if the stabilization succeeds.

		       Note that this might introduce SAVE_EXPRs and we don't
		       check whether we're at the global level or not.  This
		       is fine since we are building a pointer initializer and
		       neither the pointer nor the initializing expression can
		       be accessed before the pointer elaboration has taken
		       place in a correct program.

		       These SAVE_EXPRs will be evaluated at the right place
		       by either the evaluation of the initializer for the
		       non-global case or the elaboration code for the global
		       case, and will be attached to the elaboration procedure
		       in the latter case.  */
		    else
	 	     {
			maybe_stable_expr
			  = gnat_stabilize_reference (gnu_expr, true, &stable);

			if (stable)
			  renamed_obj = maybe_stable_expr;

			/* Attaching is actually performed downstream, as soon
			   as we have a VAR_DECL for the pointer we make.  */
		      }

		    gnu_expr = build_unary_op (ADDR_EXPR, gnu_type,
					       maybe_stable_expr);

		    gnu_size = NULL_TREE;
		    used_by_ref = true;
		  }
	      }
	  }

	/* Make a volatile version of this object's type if we are to make
	   the object volatile.  We also interpret 13.3(19) conservatively
	   and disallow any optimizations for such a non-constant object.  */
	if ((Treat_As_Volatile (gnat_entity)
	     || (!const_flag
		 && gnu_type != except_type_node
		 && (Is_Exported (gnat_entity)
		     || imported_p
		     || Present (Address_Clause (gnat_entity)))))
	    && !TYPE_VOLATILE (gnu_type))
	  gnu_type = build_qualified_type (gnu_type,
					   (TYPE_QUALS (gnu_type)
					    | TYPE_QUAL_VOLATILE));

	/* If we are defining an aliased object whose nominal subtype is
	   unconstrained, the object is a record that contains both the
	   template and the object.  If there is an initializer, it will
	   have already been converted to the right type, but we need to
	   create the template if there is no initializer.  */
	if (definition
	    && !gnu_expr
	    && TREE_CODE (gnu_type) == RECORD_TYPE
	    && (TYPE_CONTAINS_TEMPLATE_P (gnu_type)
	        /* Beware that padding might have been introduced above.  */
		|| (TYPE_PADDING_P (gnu_type)
		    && TREE_CODE (TREE_TYPE (TYPE_FIELDS (gnu_type)))
		       == RECORD_TYPE
		    && TYPE_CONTAINS_TEMPLATE_P
		       (TREE_TYPE (TYPE_FIELDS (gnu_type))))))
	  {
	    tree template_field
	      = TYPE_PADDING_P (gnu_type)
		? TYPE_FIELDS (TREE_TYPE (TYPE_FIELDS (gnu_type)))
		: TYPE_FIELDS (gnu_type);
	    VEC(constructor_elt,gc) *v = VEC_alloc (constructor_elt, gc, 1);
	    tree t = build_template (TREE_TYPE (template_field),
				     TREE_TYPE (DECL_CHAIN (template_field)),
				     NULL_TREE);
	    CONSTRUCTOR_APPEND_ELT (v, template_field, t);
	    gnu_expr = gnat_build_constructor (gnu_type, v);
	  }

	/* Convert the expression to the type of the object except in the
	   case where the object's type is unconstrained or the object's type
	   is a padded record whose field is of self-referential size.  In
	   the former case, converting will generate unnecessary evaluations
	   of the CONSTRUCTOR to compute the size and in the latter case, we
	   want to only copy the actual data.  Also don't convert to a record
	   type with a variant part from a record type without one, to keep
	   the object simpler.  */
	if (gnu_expr
	    && TREE_CODE (gnu_type) != UNCONSTRAINED_ARRAY_TYPE
	    && !CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_type))
	    && !(TYPE_IS_PADDING_P (gnu_type)
		 && CONTAINS_PLACEHOLDER_P
		    (TYPE_SIZE (TREE_TYPE (TYPE_FIELDS (gnu_type)))))
	    && !(TREE_CODE (gnu_type) == RECORD_TYPE
		 && TREE_CODE (TREE_TYPE (gnu_expr)) == RECORD_TYPE
		 && get_variant_part (gnu_type) != NULL_TREE
		 && get_variant_part (TREE_TYPE (gnu_expr)) == NULL_TREE))
	  gnu_expr = convert (gnu_type, gnu_expr);

	/* If this is a pointer that doesn't have an initializing expression,
	   initialize it to NULL, unless the object is imported.  */
	if (definition
	    && (POINTER_TYPE_P (gnu_type) || TYPE_IS_FAT_POINTER_P (gnu_type))
	    && !gnu_expr
	    && !Is_Imported (gnat_entity))
	  gnu_expr = integer_zero_node;

	/* If we are defining the object and it has an Address clause, we must
	   either get the address expression from the saved GCC tree for the
	   object if it has a Freeze node, or elaborate the address expression
	   here since the front-end has guaranteed that the elaboration has no
	   effects in this case.  */
	if (definition && Present (Address_Clause (gnat_entity)))
	  {
	    Node_Id gnat_expr = Expression (Address_Clause (gnat_entity));
	    tree gnu_address
	      = present_gnu_tree (gnat_entity)
		? get_gnu_tree (gnat_entity) : gnat_to_gnu (gnat_expr);

	    save_gnu_tree (gnat_entity, NULL_TREE, false);

	    /* Ignore the size.  It's either meaningless or was handled
	       above.  */
	    gnu_size = NULL_TREE;
	    /* Convert the type of the object to a reference type that can
	       alias everything as per 13.3(19).  */
	    gnu_type
	      = build_reference_type_for_mode (gnu_type, ptr_mode, true);
	    gnu_address = convert (gnu_type, gnu_address);
	    used_by_ref = true;
	    const_flag
	      = !Is_Public (gnat_entity)
		|| compile_time_known_address_p (gnat_expr);

	    /* If this is a deferred constant, the initializer is attached to
	       the full view.  */
	    if (kind == E_Constant && Present (Full_View (gnat_entity)))
	      gnu_expr
		= gnat_to_gnu
		    (Expression (Declaration_Node (Full_View (gnat_entity))));

	    /* If we don't have an initializing expression for the underlying
	       variable, the initializing expression for the pointer is the
	       specified address.  Otherwise, we have to make a COMPOUND_EXPR
	       to assign both the address and the initial value.  */
	    if (!gnu_expr)
	      gnu_expr = gnu_address;
	    else
	      gnu_expr
		= build2 (COMPOUND_EXPR, gnu_type,
			  build_binary_op
			  (MODIFY_EXPR, NULL_TREE,
			   build_unary_op (INDIRECT_REF, NULL_TREE,
					   gnu_address),
			   gnu_expr),
			  gnu_address);
	  }

	/* If it has an address clause and we are not defining it, mark it
	   as an indirect object.  Likewise for Stdcall objects that are
	   imported.  */
	if ((!definition && Present (Address_Clause (gnat_entity)))
	    || (Is_Imported (gnat_entity)
		&& Has_Stdcall_Convention (gnat_entity)))
	  {
	    /* Convert the type of the object to a reference type that can
	       alias everything as per 13.3(19).  */
	    gnu_type
	      = build_reference_type_for_mode (gnu_type, ptr_mode, true);
	    gnu_size = NULL_TREE;

	    /* No point in taking the address of an initializing expression
	       that isn't going to be used.  */
	    gnu_expr = NULL_TREE;

	    /* If it has an address clause whose value is known at compile
	       time, make the object a CONST_DECL.  This will avoid a
	       useless dereference.  */
	    if (Present (Address_Clause (gnat_entity)))
	      {
		Node_Id gnat_address
		  = Expression (Address_Clause (gnat_entity));

		if (compile_time_known_address_p (gnat_address))
		  {
		    gnu_expr = gnat_to_gnu (gnat_address);
		    const_flag = true;
		  }
	      }

	    used_by_ref = true;
	  }

	/* If we are at top level and this object is of variable size,
	   make the actual type a hidden pointer to the real type and
	   make the initializer be a memory allocation and initialization.
	   Likewise for objects we aren't defining (presumed to be
	   external references from other packages), but there we do
	   not set up an initialization.

	   If the object's size overflows, make an allocator too, so that
	   Storage_Error gets raised.  Note that we will never free
	   such memory, so we presume it never will get allocated.  */
	if (!allocatable_size_p (TYPE_SIZE_UNIT (gnu_type),
				 global_bindings_p ()
				 || !definition
				 || static_p)
	    || (gnu_size && !allocatable_size_p (gnu_size,
						 global_bindings_p ()
						 || !definition
						 || static_p)))
	  {
	    gnu_type = build_reference_type (gnu_type);
	    gnu_size = NULL_TREE;
	    used_by_ref = true;

	    /* In case this was a aliased object whose nominal subtype is
	       unconstrained, the pointer above will be a thin pointer and
	       build_allocator will automatically make the template.

	       If we have a template initializer only (that we made above),
	       pretend there is none and rely on what build_allocator creates
	       again anyway.  Otherwise (if we have a full initializer), get
	       the data part and feed that to build_allocator.

	       If we are elaborating a mutable object, tell build_allocator to
	       ignore a possibly simpler size from the initializer, if any, as
	       we must allocate the maximum possible size in this case.  */
	    if (definition && !imported_p)
	      {
		tree gnu_alloc_type = TREE_TYPE (gnu_type);

		if (TREE_CODE (gnu_alloc_type) == RECORD_TYPE
		    && TYPE_CONTAINS_TEMPLATE_P (gnu_alloc_type))
		  {
		    gnu_alloc_type
		      = TREE_TYPE (DECL_CHAIN (TYPE_FIELDS (gnu_alloc_type)));

		    if (TREE_CODE (gnu_expr) == CONSTRUCTOR
			&& 1 == VEC_length (constructor_elt,
					    CONSTRUCTOR_ELTS (gnu_expr)))
		      gnu_expr = 0;
		    else
		      gnu_expr
			= build_component_ref
			    (gnu_expr, NULL_TREE,
			     DECL_CHAIN (TYPE_FIELDS (TREE_TYPE (gnu_expr))),
			     false);
		  }

		if (TREE_CODE (TYPE_SIZE_UNIT (gnu_alloc_type)) == INTEGER_CST
		    && TREE_OVERFLOW (TYPE_SIZE_UNIT (gnu_alloc_type)))
		  post_error ("?`Storage_Error` will be raised at run time!",
			      gnat_entity);

		gnu_expr
		  = build_allocator (gnu_alloc_type, gnu_expr, gnu_type,
				     Empty, Empty, gnat_entity, mutable_p);
		const_flag = true;
	      }
	    else
	      {
		gnu_expr = NULL_TREE;
		const_flag = false;
	      }
	  }

	/* If this object would go into the stack and has an alignment larger
	   than the largest stack alignment the back-end can honor, resort to
	   a variable of "aligning type".  */
	if (!global_bindings_p () && !static_p && definition
	    && !imported_p && TYPE_ALIGN (gnu_type) > BIGGEST_ALIGNMENT)
	  {
	    /* Create the new variable.  No need for extra room before the
	       aligned field as this is in automatic storage.  */
	    tree gnu_new_type
	      = make_aligning_type (gnu_type, TYPE_ALIGN (gnu_type),
				    TYPE_SIZE_UNIT (gnu_type),
				    BIGGEST_ALIGNMENT, 0);
	    tree gnu_new_var
	      = create_var_decl (create_concat_name (gnat_entity, "ALIGN"),
				 NULL_TREE, gnu_new_type, NULL_TREE, false,
				 false, false, false, NULL, gnat_entity);

	    /* Initialize the aligned field if we have an initializer.  */
	    if (gnu_expr)
	      add_stmt_with_node
		(build_binary_op (MODIFY_EXPR, NULL_TREE,
				  build_component_ref
				  (gnu_new_var, NULL_TREE,
				   TYPE_FIELDS (gnu_new_type), false),
				  gnu_expr),
		 gnat_entity);

	    /* And setup this entity as a reference to the aligned field.  */
	    gnu_type = build_reference_type (gnu_type);
	    gnu_expr
	      = build_unary_op
		(ADDR_EXPR, gnu_type,
		 build_component_ref (gnu_new_var, NULL_TREE,
				      TYPE_FIELDS (gnu_new_type), false));

	    gnu_size = NULL_TREE;
	    used_by_ref = true;
	    const_flag = true;
	  }

	if (const_flag)
	  gnu_type = build_qualified_type (gnu_type, (TYPE_QUALS (gnu_type)
						      | TYPE_QUAL_CONST));

	/* Convert the expression to the type of the object except in the
	   case where the object's type is unconstrained or the object's type
	   is a padded record whose field is of self-referential size.  In
	   the former case, converting will generate unnecessary evaluations
	   of the CONSTRUCTOR to compute the size and in the latter case, we
	   want to only copy the actual data.  Also don't convert to a record
	   type with a variant part from a record type without one, to keep
	   the object simpler.  */
	if (gnu_expr
	    && TREE_CODE (gnu_type) != UNCONSTRAINED_ARRAY_TYPE
	    && !CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_type))
	    && !(TYPE_IS_PADDING_P (gnu_type)
		 && CONTAINS_PLACEHOLDER_P
		    (TYPE_SIZE (TREE_TYPE (TYPE_FIELDS (gnu_type)))))
	    && !(TREE_CODE (gnu_type) == RECORD_TYPE
		 && TREE_CODE (TREE_TYPE (gnu_expr)) == RECORD_TYPE
		 && get_variant_part (gnu_type) != NULL_TREE
		 && get_variant_part (TREE_TYPE (gnu_expr)) == NULL_TREE))
	  gnu_expr = convert (gnu_type, gnu_expr);

	/* If this name is external or there was a name specified, use it,
	   unless this is a VMS exception object since this would conflict
	   with the symbol we need to export in addition.  Don't use the
	   Interface_Name if there is an address clause (see CD30005).  */
	if (!Is_VMS_Exception (gnat_entity)
	    && ((Present (Interface_Name (gnat_entity))
		 && No (Address_Clause (gnat_entity)))
		|| (Is_Public (gnat_entity)
		    && (!Is_Imported (gnat_entity)
			|| Is_Exported (gnat_entity)))))
	  gnu_ext_name = create_concat_name (gnat_entity, NULL);

	/* If this is an aggregate constant initialized to a constant, force it
	   to be statically allocated.  This saves an initialization copy.  */
	if (!static_p
	    && const_flag
	    && gnu_expr && TREE_CONSTANT (gnu_expr)
	    && AGGREGATE_TYPE_P (gnu_type)
	    && host_integerp (TYPE_SIZE_UNIT (gnu_type), 1)
	    && !(TYPE_IS_PADDING_P (gnu_type)
		 && !host_integerp (TYPE_SIZE_UNIT
				    (TREE_TYPE (TYPE_FIELDS (gnu_type))), 1)))
	  static_p = true;

	/* Now create the variable or the constant and set various flags.  */
	gnu_decl
	  = create_var_decl (gnu_entity_name, gnu_ext_name, gnu_type,
			     gnu_expr, const_flag, Is_Public (gnat_entity),
			     imported_p || !definition, static_p, attr_list,
			     gnat_entity);
	DECL_BY_REF_P (gnu_decl) = used_by_ref;
	DECL_POINTS_TO_READONLY_P (gnu_decl) = used_by_ref && inner_const_flag;
	DECL_CAN_NEVER_BE_NULL_P (gnu_decl) = Can_Never_Be_Null (gnat_entity);

	/* If we are defining an Out parameter and optimization isn't enabled,
	   create a fake PARM_DECL for debugging purposes and make it point to
	   the VAR_DECL.  Suppress debug info for the latter but make sure it
	   will live on the stack so that it can be accessed from within the
	   debugger through the PARM_DECL.  */
	if (kind == E_Out_Parameter && definition && !optimize && debug_info_p)
	  {
	    tree param = create_param_decl (gnu_entity_name, gnu_type, false);
	    gnat_pushdecl (param, gnat_entity);
	    SET_DECL_VALUE_EXPR (param, gnu_decl);
	    DECL_HAS_VALUE_EXPR_P (param) = 1;
	    DECL_IGNORED_P (gnu_decl) = 1;
	    TREE_ADDRESSABLE (gnu_decl) = 1;
	  }

	/* If this is a loop parameter, set the corresponding flag.  */
	else if (kind == E_Loop_Parameter)
	  DECL_LOOP_PARM_P (gnu_decl) = 1;

	/* If this is a renaming pointer, attach the renamed object to it and
	   register it if we are at the global level.  Note that an external
	   constant is at the global level.  */
	else if (TREE_CODE (gnu_decl) == VAR_DECL && renamed_obj)
	  {
	    SET_DECL_RENAMED_OBJECT (gnu_decl, renamed_obj);
	    if ((!definition && kind == E_Constant) || global_bindings_p ())
	      {
		DECL_RENAMING_GLOBAL_P (gnu_decl) = 1;
		record_global_renaming_pointer (gnu_decl);
	      }
	  }

	/* If this is a constant and we are defining it or it generates a real
	   symbol at the object level and we are referencing it, we may want
	   or need to have a true variable to represent it:
	     - if optimization isn't enabled, for debugging purposes,
	     - if the constant is public and not overlaid on something else,
	     - if its address is taken,
	     - if either itself or its type is aliased.  */
	if (TREE_CODE (gnu_decl) == CONST_DECL
	    && (definition || Sloc (gnat_entity) > Standard_Location)
	    && ((!optimize && debug_info_p)
		|| (Is_Public (gnat_entity)
		    && No (Address_Clause (gnat_entity)))
		|| Address_Taken (gnat_entity)
		|| Is_Aliased (gnat_entity)
		|| Is_Aliased (Etype (gnat_entity))))
	  {
	    tree gnu_corr_var
	      = create_true_var_decl (gnu_entity_name, gnu_ext_name, gnu_type,
				      gnu_expr, true, Is_Public (gnat_entity),
				      !definition, static_p, attr_list,
				      gnat_entity);

	    SET_DECL_CONST_CORRESPONDING_VAR (gnu_decl, gnu_corr_var);

	    /* As debugging information will be generated for the variable,
	       do not generate debugging information for the constant.  */
	    if (debug_info_p)
	      DECL_IGNORED_P (gnu_decl) = 1;
	    else
	      DECL_IGNORED_P (gnu_corr_var) = 1;
	  }

	/* If this is a constant, even if we don't need a true variable, we
	   may need to avoid returning the initializer in every case.  That
	   can happen for the address of a (constant) constructor because,
	   upon dereferencing it, the constructor will be reinjected in the
	   tree, which may not be valid in every case; see lvalue_required_p
	   for more details.  */
	if (TREE_CODE (gnu_decl) == CONST_DECL)
	  DECL_CONST_ADDRESS_P (gnu_decl) = constructor_address_p (gnu_expr);

	/* If this object is declared in a block that contains a block with an
	   exception handler, and we aren't using the GCC exception mechanism,
	   we must force this variable in memory in order to avoid an invalid
	   optimization.  */
	if (Exception_Mechanism != Back_End_Exceptions
	    && Has_Nested_Block_With_Handler (Scope (gnat_entity)))
	  TREE_ADDRESSABLE (gnu_decl) = 1;

	/* If we are defining an object with variable size or an object with
	   fixed size that will be dynamically allocated, and we are using the
	   setjmp/longjmp exception mechanism, update the setjmp buffer.  */
	if (definition
	    && Exception_Mechanism == Setjmp_Longjmp
	    && get_block_jmpbuf_decl ()
	    && DECL_SIZE_UNIT (gnu_decl)
	    && (TREE_CODE (DECL_SIZE_UNIT (gnu_decl)) != INTEGER_CST
		|| (flag_stack_check == GENERIC_STACK_CHECK
		    && compare_tree_int (DECL_SIZE_UNIT (gnu_decl),
					 STACK_CHECK_MAX_VAR_SIZE) > 0)))
	  add_stmt_with_node (build_call_n_expr
			      (update_setjmp_buf_decl, 1,
			       build_unary_op (ADDR_EXPR, NULL_TREE,
					       get_block_jmpbuf_decl ())),
			      gnat_entity);

	/* Back-annotate Esize and Alignment of the object if not already
	   known.  Note that we pick the values of the type, not those of
	   the object, to shield ourselves from low-level platform-dependent
	   adjustments like alignment promotion.  This is both consistent with
	   all the treatment above, where alignment and size are set on the
	   type of the object and not on the object directly, and makes it
	   possible to support all confirming representation clauses.  */
	annotate_object (gnat_entity, TREE_TYPE (gnu_decl), gnu_object_size,
			 used_by_ref, false);
      }
      break;

    case E_Void:
      /* Return a TYPE_DECL for "void" that we previously made.  */
      gnu_decl = TYPE_NAME (void_type_node);
      break;

    case E_Enumeration_Type:
      /* A special case: for the types Character and Wide_Character in
	 Standard, we do not list all the literals.  So if the literals
	 are not specified, make this an unsigned type.  */
      if (No (First_Literal (gnat_entity)))
	{
	  gnu_type = make_unsigned_type (esize);
	  TYPE_NAME (gnu_type) = gnu_entity_name;

	  /* Set TYPE_STRING_FLAG for Character and Wide_Character types.
	     This is needed by the DWARF-2 back-end to distinguish between
	     unsigned integer types and character types.  */
	  TYPE_STRING_FLAG (gnu_type) = 1;
	  break;
	}

      {
	/* We have a list of enumeral constants in First_Literal.  We make a
	   CONST_DECL for each one and build into GNU_LITERAL_LIST the list to
	   be placed into TYPE_FIELDS.  Each node in the list is a TREE_LIST
	   whose TREE_VALUE is the literal name and whose TREE_PURPOSE is the
	   value of the literal.  But when we have a regular boolean type, we
	   simplify this a little by using a BOOLEAN_TYPE.  */
	bool is_boolean = Is_Boolean_Type (gnat_entity)
			  && !Has_Non_Standard_Rep (gnat_entity);
	tree gnu_literal_list = NULL_TREE;
	Entity_Id gnat_literal;

	if (Is_Unsigned_Type (gnat_entity))
	  gnu_type = make_unsigned_type (esize);
	else
	  gnu_type = make_signed_type (esize);

	TREE_SET_CODE (gnu_type, is_boolean ? BOOLEAN_TYPE : ENUMERAL_TYPE);

	for (gnat_literal = First_Literal (gnat_entity);
	     Present (gnat_literal);
	     gnat_literal = Next_Literal (gnat_literal))
	  {
	    tree gnu_value
	      = UI_To_gnu (Enumeration_Rep (gnat_literal), gnu_type);
	    tree gnu_literal
	      = create_var_decl (get_entity_name (gnat_literal), NULL_TREE,
				 gnu_type, gnu_value, true, false, false,
				 false, NULL, gnat_literal);
	    /* Do not generate debug info for individual enumerators.  */
	    DECL_IGNORED_P (gnu_literal) = 1;
	    save_gnu_tree (gnat_literal, gnu_literal, false);
	    gnu_literal_list = tree_cons (DECL_NAME (gnu_literal),
					  gnu_value, gnu_literal_list);
	  }

	if (!is_boolean)
	  TYPE_VALUES (gnu_type) = nreverse (gnu_literal_list);

	/* Note that the bounds are updated at the end of this function
	   to avoid an infinite recursion since they refer to the type.  */
      }
      goto discrete_type;

    case E_Signed_Integer_Type:
    case E_Ordinary_Fixed_Point_Type:
    case E_Decimal_Fixed_Point_Type:
      /* For integer types, just make a signed type the appropriate number
	 of bits.  */
      gnu_type = make_signed_type (esize);
      goto discrete_type;

    case E_Modular_Integer_Type:
      {
	/* For modular types, make the unsigned type of the proper number
	   of bits and then set up the modulus, if required.  */
	tree gnu_modulus, gnu_high = NULL_TREE;

	/* Packed array types are supposed to be subtypes only.  */
	gcc_assert (!Is_Packed_Array_Type (gnat_entity));

	gnu_type = make_unsigned_type (esize);

	/* Get the modulus in this type.  If it overflows, assume it is because
	   it is equal to 2**Esize.  Note that there is no overflow checking
	   done on unsigned type, so we detect the overflow by looking for
	   a modulus of zero, which is otherwise invalid.  */
	gnu_modulus = UI_To_gnu (Modulus (gnat_entity), gnu_type);

	if (!integer_zerop (gnu_modulus))
	  {
	    TYPE_MODULAR_P (gnu_type) = 1;
	    SET_TYPE_MODULUS (gnu_type, gnu_modulus);
	    gnu_high = fold_build2 (MINUS_EXPR, gnu_type, gnu_modulus,
				    convert (gnu_type, integer_one_node));
	  }

	/* If the upper bound is not maximal, make an extra subtype.  */
	if (gnu_high
	    && !tree_int_cst_equal (gnu_high, TYPE_MAX_VALUE (gnu_type)))
	  {
	    tree gnu_subtype = make_unsigned_type (esize);
	    SET_TYPE_RM_MAX_VALUE (gnu_subtype, gnu_high);
	    TREE_TYPE (gnu_subtype) = gnu_type;
	    TYPE_EXTRA_SUBTYPE_P (gnu_subtype) = 1;
	    TYPE_NAME (gnu_type) = create_concat_name (gnat_entity, "UMT");
	    gnu_type = gnu_subtype;
	  }
      }
      goto discrete_type;

    case E_Signed_Integer_Subtype:
    case E_Enumeration_Subtype:
    case E_Modular_Integer_Subtype:
    case E_Ordinary_Fixed_Point_Subtype:
    case E_Decimal_Fixed_Point_Subtype:

      /* For integral subtypes, we make a new INTEGER_TYPE.  Note that we do
	 not want to call create_range_type since we would like each subtype
	 node to be distinct.  ??? Historically this was in preparation for
	 when memory aliasing is implemented, but that's obsolete now given
	 the call to relate_alias_sets below.

	 The TREE_TYPE field of the INTEGER_TYPE points to the base type;
	 this fact is used by the arithmetic conversion functions.

	 We elaborate the Ancestor_Subtype if it is not in the current unit
	 and one of our bounds is non-static.  We do this to ensure consistent
	 naming in the case where several subtypes share the same bounds, by
	 elaborating the first such subtype first, thus using its name.  */

      if (!definition
	  && Present (Ancestor_Subtype (gnat_entity))
	  && !In_Extended_Main_Code_Unit (Ancestor_Subtype (gnat_entity))
	  && (!Compile_Time_Known_Value (Type_Low_Bound (gnat_entity))
	      || !Compile_Time_Known_Value (Type_High_Bound (gnat_entity))))
	gnat_to_gnu_entity (Ancestor_Subtype (gnat_entity), gnu_expr, 0);

      /* Set the precision to the Esize except for bit-packed arrays.  */
      if (Is_Packed_Array_Type (gnat_entity)
	  && Is_Bit_Packed_Array (Original_Array_Type (gnat_entity)))
	esize = UI_To_Int (RM_Size (gnat_entity));

      /* This should be an unsigned type if the base type is unsigned or
	 if the lower bound is constant and non-negative or if the type
	 is biased.  */
      if (Is_Unsigned_Type (Etype (gnat_entity))
	  || Is_Unsigned_Type (gnat_entity)
	  || Has_Biased_Representation (gnat_entity))
	gnu_type = make_unsigned_type (esize);
      else
	gnu_type = make_signed_type (esize);
      TREE_TYPE (gnu_type) = get_unpadded_type (Etype (gnat_entity));

      SET_TYPE_RM_MIN_VALUE
	(gnu_type,
	 convert (TREE_TYPE (gnu_type),
		  elaborate_expression (Type_Low_Bound (gnat_entity),
					gnat_entity, get_identifier ("L"),
					definition, true,
					Needs_Debug_Info (gnat_entity))));

      SET_TYPE_RM_MAX_VALUE
	(gnu_type,
	 convert (TREE_TYPE (gnu_type),
		  elaborate_expression (Type_High_Bound (gnat_entity),
					gnat_entity, get_identifier ("U"),
					definition, true,
					Needs_Debug_Info (gnat_entity))));

      /* One of the above calls might have caused us to be elaborated,
	 so don't blow up if so.  */
      if (present_gnu_tree (gnat_entity))
	{
	  maybe_present = true;
	  break;
	}

      TYPE_BIASED_REPRESENTATION_P (gnu_type)
	= Has_Biased_Representation (gnat_entity);

      /* Attach the TYPE_STUB_DECL in case we have a parallel type.  */
      TYPE_STUB_DECL (gnu_type)
	= create_type_stub_decl (gnu_entity_name, gnu_type);

      /* Inherit our alias set from what we're a subtype of.  Subtypes
	 are not different types and a pointer can designate any instance
	 within a subtype hierarchy.  */
      relate_alias_sets (gnu_type, TREE_TYPE (gnu_type), ALIAS_SET_COPY);

      /* For a packed array, make the original array type a parallel type.  */
      if (debug_info_p
	  && Is_Packed_Array_Type (gnat_entity)
	  && present_gnu_tree (Original_Array_Type (gnat_entity)))
	add_parallel_type (TYPE_STUB_DECL (gnu_type),
			   gnat_to_gnu_type
			   (Original_Array_Type (gnat_entity)));

    discrete_type:

      /* We have to handle clauses that under-align the type specially.  */
      if ((Present (Alignment_Clause (gnat_entity))
	   || (Is_Packed_Array_Type (gnat_entity)
	       && Present
		  (Alignment_Clause (Original_Array_Type (gnat_entity)))))
	  && UI_Is_In_Int_Range (Alignment (gnat_entity)))
	{
	  align = UI_To_Int (Alignment (gnat_entity)) * BITS_PER_UNIT;
	  if (align >= TYPE_ALIGN (gnu_type))
	    align = 0;
	}

      /* If the type we are dealing with represents a bit-packed array,
	 we need to have the bits left justified on big-endian targets
	 and right justified on little-endian targets.  We also need to
	 ensure that when the value is read (e.g. for comparison of two
	 such values), we only get the good bits, since the unused bits
	 are uninitialized.  Both goals are accomplished by wrapping up
	 the modular type in an enclosing record type.  */
      if (Is_Packed_Array_Type (gnat_entity)
	  && Is_Bit_Packed_Array (Original_Array_Type (gnat_entity)))
	{
	  tree gnu_field_type, gnu_field;

	  /* Set the RM size before wrapping up the original type.  */
	  SET_TYPE_RM_SIZE (gnu_type,
			    UI_To_gnu (RM_Size (gnat_entity), bitsizetype));
	  TYPE_PACKED_ARRAY_TYPE_P (gnu_type) = 1;

	  /* Create a stripped-down declaration, mainly for debugging.  */
	  create_type_decl (gnu_entity_name, gnu_type, NULL, true,
			    debug_info_p, gnat_entity);

	  /* Now save it and build the enclosing record type.  */
	  gnu_field_type = gnu_type;

	  gnu_type = make_node (RECORD_TYPE);
	  TYPE_NAME (gnu_type) = create_concat_name (gnat_entity, "JM");
	  TYPE_PACKED (gnu_type) = 1;
	  TYPE_SIZE (gnu_type) = TYPE_SIZE (gnu_field_type);
	  TYPE_SIZE_UNIT (gnu_type) = TYPE_SIZE_UNIT (gnu_field_type);
	  SET_TYPE_ADA_SIZE (gnu_type, TYPE_RM_SIZE (gnu_field_type));

	  /* Propagate the alignment of the modular type to the record type,
	     unless there is an alignment clause that under-aligns the type.
	     This means that bit-packed arrays are given "ceil" alignment for
	     their size by default, which may seem counter-intuitive but makes
	     it possible to overlay them on modular types easily.  */
	  TYPE_ALIGN (gnu_type)
	    = align > 0 ? align : TYPE_ALIGN (gnu_field_type);

	  relate_alias_sets (gnu_type, gnu_field_type, ALIAS_SET_COPY);

	  /* Don't declare the field as addressable since we won't be taking
	     its address and this would prevent create_field_decl from making
	     a bitfield.  */
	  gnu_field
	    = create_field_decl (get_identifier ("OBJECT"), gnu_field_type,
				 gnu_type, NULL_TREE, bitsize_zero_node, 1, 0);

	  /* Do not emit debug info until after the parallel type is added.  */
	  finish_record_type (gnu_type, gnu_field, 2, false);
	  compute_record_mode (gnu_type);
	  TYPE_JUSTIFIED_MODULAR_P (gnu_type) = 1;

	  if (debug_info_p)
	    {
	      /* Make the original array type a parallel type.  */
	      if (present_gnu_tree (Original_Array_Type (gnat_entity)))
		add_parallel_type (TYPE_STUB_DECL (gnu_type),
				   gnat_to_gnu_type
				   (Original_Array_Type (gnat_entity)));

	      rest_of_record_type_compilation (gnu_type);
	    }
	}

      /* If the type we are dealing with has got a smaller alignment than the
	 natural one, we need to wrap it up in a record type and under-align
	 the latter.  We reuse the padding machinery for this purpose.  */
      else if (align > 0)
	{
	  tree gnu_field_type, gnu_field;

	  /* Set the RM size before wrapping up the type.  */
	  SET_TYPE_RM_SIZE (gnu_type,
			    UI_To_gnu (RM_Size (gnat_entity), bitsizetype));

	  /* Create a stripped-down declaration, mainly for debugging.  */
	  create_type_decl (gnu_entity_name, gnu_type, NULL, true,
			    debug_info_p, gnat_entity);

	  /* Now save it and build the enclosing record type.  */
	  gnu_field_type = gnu_type;

	  gnu_type = make_node (RECORD_TYPE);
	  TYPE_NAME (gnu_type) = create_concat_name (gnat_entity, "PAD");
	  TYPE_PACKED (gnu_type) = 1;
	  TYPE_SIZE (gnu_type) = TYPE_SIZE (gnu_field_type);
	  TYPE_SIZE_UNIT (gnu_type) = TYPE_SIZE_UNIT (gnu_field_type);
	  SET_TYPE_ADA_SIZE (gnu_type, TYPE_RM_SIZE (gnu_field_type));
	  TYPE_ALIGN (gnu_type) = align;
	  relate_alias_sets (gnu_type, gnu_field_type, ALIAS_SET_COPY);

	  /* Don't declare the field as addressable since we won't be taking
	     its address and this would prevent create_field_decl from making
	     a bitfield.  */
	  gnu_field
	    = create_field_decl (get_identifier ("F"), gnu_field_type,
				 gnu_type, NULL_TREE, bitsize_zero_node, 1, 0);

	  finish_record_type (gnu_type, gnu_field, 2, debug_info_p);
	  compute_record_mode (gnu_type);
	  TYPE_PADDING_P (gnu_type) = 1;
	}

      break;

    case E_Floating_Point_Type:
      /* If this is a VAX floating-point type, use an integer of the proper
	 size.  All the operations will be handled with ASM statements.  */
      if (Vax_Float (gnat_entity))
	{
	  gnu_type = make_signed_type (esize);
	  TYPE_VAX_FLOATING_POINT_P (gnu_type) = 1;
	  SET_TYPE_DIGITS_VALUE (gnu_type,
				 UI_To_gnu (Digits_Value (gnat_entity),
					    sizetype));
	  break;
	}

      /* The type of the Low and High bounds can be our type if this is
	 a type from Standard, so set them at the end of the function.  */
      gnu_type = make_node (REAL_TYPE);
      TYPE_PRECISION (gnu_type) = fp_size_to_prec (esize);
      layout_type (gnu_type);
      break;

    case E_Floating_Point_Subtype:
      if (Vax_Float (gnat_entity))
	{
	  gnu_type = gnat_to_gnu_type (Etype (gnat_entity));
	  break;
	}

      {
	if (!definition
	    && Present (Ancestor_Subtype (gnat_entity))
	    && !In_Extended_Main_Code_Unit (Ancestor_Subtype (gnat_entity))
	    && (!Compile_Time_Known_Value (Type_Low_Bound (gnat_entity))
		|| !Compile_Time_Known_Value (Type_High_Bound (gnat_entity))))
	  gnat_to_gnu_entity (Ancestor_Subtype (gnat_entity),
			      gnu_expr, 0);

	gnu_type = make_node (REAL_TYPE);
	TREE_TYPE (gnu_type) = get_unpadded_type (Etype (gnat_entity));
	TYPE_PRECISION (gnu_type) = fp_size_to_prec (esize);
	TYPE_GCC_MIN_VALUE (gnu_type)
	  = TYPE_GCC_MIN_VALUE (TREE_TYPE (gnu_type));
	TYPE_GCC_MAX_VALUE (gnu_type)
	  = TYPE_GCC_MAX_VALUE (TREE_TYPE (gnu_type));
	layout_type (gnu_type);

	SET_TYPE_RM_MIN_VALUE
	  (gnu_type,
	   convert (TREE_TYPE (gnu_type),
		    elaborate_expression (Type_Low_Bound (gnat_entity),
					  gnat_entity, get_identifier ("L"),
					  definition, true,
					  Needs_Debug_Info (gnat_entity))));

	SET_TYPE_RM_MAX_VALUE
	  (gnu_type,
	   convert (TREE_TYPE (gnu_type),
		    elaborate_expression (Type_High_Bound (gnat_entity),
					  gnat_entity, get_identifier ("U"),
					  definition, true,
					  Needs_Debug_Info (gnat_entity))));

	/* One of the above calls might have caused us to be elaborated,
	   so don't blow up if so.  */
	if (present_gnu_tree (gnat_entity))
	  {
	    maybe_present = true;
	    break;
	  }

	/* Inherit our alias set from what we're a subtype of, as for
	   integer subtypes.  */
	relate_alias_sets (gnu_type, TREE_TYPE (gnu_type), ALIAS_SET_COPY);
      }
    break;

      /* Array and String Types and Subtypes

	 Unconstrained array types are represented by E_Array_Type and
	 constrained array types are represented by E_Array_Subtype.  There
	 are no actual objects of an unconstrained array type; all we have
	 are pointers to that type.

	 The following fields are defined on array types and subtypes:

		Component_Type     Component type of the array.
		Number_Dimensions  Number of dimensions (an int).
		First_Index	   Type of first index.  */

    case E_String_Type:
    case E_Array_Type:
      {
	const bool convention_fortran_p
	  = (Convention (gnat_entity) == Convention_Fortran);
	const int ndim = Number_Dimensions (gnat_entity);
	tree gnu_template_type = make_node (RECORD_TYPE);
	tree gnu_ptr_template = build_pointer_type (gnu_template_type);
	tree gnu_template_reference, gnu_template_fields, gnu_fat_type;
	tree *gnu_index_types = XALLOCAVEC (tree, ndim);
	tree *gnu_temp_fields = XALLOCAVEC (tree, ndim);
	tree gnu_max_size = size_one_node, gnu_max_size_unit, tem, t;
	Entity_Id gnat_index, gnat_name;
	int index;

	/* We complete an existing dummy fat pointer type in place.  This both
	   avoids further complex adjustments in update_pointer_to and yields
	   better debugging information in DWARF by leveraging the support for
	   incomplete declarations of "tagged" types in the DWARF back-end.  */
	gnu_type = get_dummy_type (gnat_entity);
	if (gnu_type && TYPE_POINTER_TO (gnu_type))
	  {
	    gnu_fat_type = TYPE_MAIN_VARIANT (TYPE_POINTER_TO (gnu_type));
	    TYPE_NAME (gnu_fat_type) = NULL_TREE;
	    /* Save the contents of the dummy type for update_pointer_to.  */
	    TYPE_POINTER_TO (gnu_type) = copy_type (gnu_fat_type);
	  }
	else
	  gnu_fat_type = make_node (RECORD_TYPE);

	/* Make a node for the array.  If we are not defining the array
	   suppress expanding incomplete types.  */
	gnu_type = make_node (UNCONSTRAINED_ARRAY_TYPE);

	if (!definition)
	  {
	    defer_incomplete_level++;
	    this_deferred = true;
	  }

	/* Build the fat pointer type.  Use a "void *" object instead of
	   a pointer to the array type since we don't have the array type
	   yet (it will reference the fat pointer via the bounds).  */
	tem
	  = create_field_decl (get_identifier ("P_ARRAY"), ptr_void_type_node,
			       gnu_fat_type, NULL_TREE, NULL_TREE, 0, 0);
	DECL_CHAIN (tem)
	  = create_field_decl (get_identifier ("P_BOUNDS"), gnu_ptr_template,
			       gnu_fat_type, NULL_TREE, NULL_TREE, 0, 0);

	if (COMPLETE_TYPE_P (gnu_fat_type))
	  {
	    /* We are going to lay it out again so reset the alias set.  */
	    alias_set_type alias_set = TYPE_ALIAS_SET (gnu_fat_type);
	    TYPE_ALIAS_SET (gnu_fat_type) = -1;
	    finish_fat_pointer_type (gnu_fat_type, tem);
	    TYPE_ALIAS_SET (gnu_fat_type) = alias_set;
	    for (t = gnu_fat_type; t; t = TYPE_NEXT_VARIANT (t))
	      {
		TYPE_FIELDS (t) = tem;
		SET_TYPE_UNCONSTRAINED_ARRAY (t, gnu_type);
	      }
	  }
	else
	  {
	    finish_fat_pointer_type (gnu_fat_type, tem);
	    SET_TYPE_UNCONSTRAINED_ARRAY (gnu_fat_type, gnu_type);
	  }

	/* Build a reference to the template from a PLACEHOLDER_EXPR that
	   is the fat pointer.  This will be used to access the individual
	   fields once we build them.  */
	tem = build3 (COMPONENT_REF, gnu_ptr_template,
		      build0 (PLACEHOLDER_EXPR, gnu_fat_type),
		      DECL_CHAIN (TYPE_FIELDS (gnu_fat_type)), NULL_TREE);
	gnu_template_reference
	  = build_unary_op (INDIRECT_REF, gnu_template_type, tem);
	TREE_READONLY (gnu_template_reference) = 1;
	TREE_THIS_NOTRAP (gnu_template_reference) = 1;

	/* Now create the GCC type for each index and add the fields for that
	   index to the template.  */
	for (index = (convention_fortran_p ? ndim - 1 : 0),
	     gnat_index = First_Index (gnat_entity);
	     0 <= index && index < ndim;
	     index += (convention_fortran_p ? - 1 : 1),
	     gnat_index = Next_Index (gnat_index))
	  {
	    char field_name[16];
	    tree gnu_index_base_type
	      = get_unpadded_type (Base_Type (Etype (gnat_index)));
	    tree gnu_lb_field, gnu_hb_field, gnu_orig_min, gnu_orig_max;
	    tree gnu_min, gnu_max, gnu_high;

	    /* Make the FIELD_DECLs for the low and high bounds of this
	       type and then make extractions of these fields from the
	       template.  */
	    sprintf (field_name, "LB%d", index);
	    gnu_lb_field = create_field_decl (get_identifier (field_name),
					      gnu_index_base_type,
					      gnu_template_type, NULL_TREE,
					      NULL_TREE, 0, 0);
	    Sloc_to_locus (Sloc (gnat_entity),
			   &DECL_SOURCE_LOCATION (gnu_lb_field));

	    field_name[0] = 'U';
	    gnu_hb_field = create_field_decl (get_identifier (field_name),
					      gnu_index_base_type,
					      gnu_template_type, NULL_TREE,
					      NULL_TREE, 0, 0);
	    Sloc_to_locus (Sloc (gnat_entity),
			   &DECL_SOURCE_LOCATION (gnu_hb_field));

	    gnu_temp_fields[index] = chainon (gnu_lb_field, gnu_hb_field);

	    /* We can't use build_component_ref here since the template type
	       isn't complete yet.  */
	    gnu_orig_min = build3 (COMPONENT_REF, gnu_index_base_type,
				   gnu_template_reference, gnu_lb_field,
				   NULL_TREE);
	    gnu_orig_max = build3 (COMPONENT_REF, gnu_index_base_type,
				   gnu_template_reference, gnu_hb_field,
				   NULL_TREE);
	    TREE_READONLY (gnu_orig_min) = TREE_READONLY (gnu_orig_max) = 1;

	    gnu_min = convert (sizetype, gnu_orig_min);
	    gnu_max = convert (sizetype, gnu_orig_max);

	    /* Compute the size of this dimension.  See the E_Array_Subtype
	       case below for the rationale.  */
	    gnu_high
	      = build3 (COND_EXPR, sizetype,
			build2 (GE_EXPR, boolean_type_node,
				gnu_orig_max, gnu_orig_min),
			gnu_max,
			size_binop (MINUS_EXPR, gnu_min, size_one_node));

	    /* Make a range type with the new range in the Ada base type.
	       Then make an index type with the size range in sizetype.  */
	    gnu_index_types[index]
	      = create_index_type (gnu_min, gnu_high,
				   create_range_type (gnu_index_base_type,
						      gnu_orig_min,
						      gnu_orig_max),
				   gnat_entity);

	    /* Update the maximum size of the array in elements.  */
	    if (gnu_max_size)
	      {
		tree gnu_index_type = get_unpadded_type (Etype (gnat_index));
		tree gnu_min
		  = convert (sizetype, TYPE_MIN_VALUE (gnu_index_type));
		tree gnu_max
		  = convert (sizetype, TYPE_MAX_VALUE (gnu_index_type));
		tree gnu_this_max
		  = size_binop (MAX_EXPR,
				size_binop (PLUS_EXPR, size_one_node,
					    size_binop (MINUS_EXPR,
							gnu_max, gnu_min)),
				size_zero_node);

		if (TREE_CODE (gnu_this_max) == INTEGER_CST
		    && TREE_OVERFLOW (gnu_this_max))
		  gnu_max_size = NULL_TREE;
		else
		  gnu_max_size
		    = size_binop (MULT_EXPR, gnu_max_size, gnu_this_max);
	      }

	    TYPE_NAME (gnu_index_types[index])
	      = create_concat_name (gnat_entity, field_name);
	  }

	/* Install all the fields into the template.  */
	TYPE_NAME (gnu_template_type)
	  = create_concat_name (gnat_entity, "XUB");
	gnu_template_fields = NULL_TREE;
	for (index = 0; index < ndim; index++)
	  gnu_template_fields
	    = chainon (gnu_template_fields, gnu_temp_fields[index]);
	finish_record_type (gnu_template_type, gnu_template_fields, 0,
			    debug_info_p);
	TYPE_READONLY (gnu_template_type) = 1;

	/* Now make the array of arrays and update the pointer to the array
	   in the fat pointer.  Note that it is the first field.  */
	tem
	  = gnat_to_gnu_component_type (gnat_entity, definition, debug_info_p);

	/* If Component_Size is not already specified, annotate it with the
	   size of the component.  */
	if (Unknown_Component_Size (gnat_entity))
	  Set_Component_Size (gnat_entity, annotate_value (TYPE_SIZE (tem)));

	/* Compute the maximum size of the array in units and bits.  */
	if (gnu_max_size)
	  {
	    gnu_max_size_unit = size_binop (MULT_EXPR, gnu_max_size,
					    TYPE_SIZE_UNIT (tem));
	    gnu_max_size = size_binop (MULT_EXPR,
				       convert (bitsizetype, gnu_max_size),
				       TYPE_SIZE (tem));
	  }
	else
	  gnu_max_size_unit = NULL_TREE;

	/* Now build the array type.  */
	for (index = ndim - 1; index >= 0; index--)
	  {
	    tem = build_nonshared_array_type (tem, gnu_index_types[index]);
	    TYPE_MULTI_ARRAY_P (tem) = (index > 0);
	    if (array_type_has_nonaliased_component (tem, gnat_entity))
	      TYPE_NONALIASED_COMPONENT (tem) = 1;
	  }

	/* If an alignment is specified, use it if valid.  But ignore it
	   for the original type of packed array types.  If the alignment
	   was requested with an explicit alignment clause, state so.  */
	if (No (Packed_Array_Type (gnat_entity))
	    && Known_Alignment (gnat_entity))
	  {
	    TYPE_ALIGN (tem)
	      = validate_alignment (Alignment (gnat_entity), gnat_entity,
				    TYPE_ALIGN (tem));
	    if (Present (Alignment_Clause (gnat_entity)))
	      TYPE_USER_ALIGN (tem) = 1;
	  }

	TYPE_CONVENTION_FORTRAN_P (tem) = convention_fortran_p;

	/* Adjust the type of the pointer-to-array field of the fat pointer
	   and record the aliasing relationships if necessary.  */
	TREE_TYPE (TYPE_FIELDS (gnu_fat_type)) = build_pointer_type (tem);
	if (TYPE_ALIAS_SET_KNOWN_P (gnu_fat_type))
	  record_component_aliases (gnu_fat_type);

	/* The result type is an UNCONSTRAINED_ARRAY_TYPE that indicates the
	   corresponding fat pointer.  */
	TREE_TYPE (gnu_type) = gnu_fat_type;
	TYPE_POINTER_TO (gnu_type) = gnu_fat_type;
	TYPE_REFERENCE_TO (gnu_type) = gnu_fat_type;
	SET_TYPE_MODE (gnu_type, BLKmode);
	TYPE_ALIGN (gnu_type) = TYPE_ALIGN (tem);

	/* If the maximum size doesn't overflow, use it.  */
	if (gnu_max_size
	    && TREE_CODE (gnu_max_size) == INTEGER_CST
	    && !TREE_OVERFLOW (gnu_max_size)
	    && TREE_CODE (gnu_max_size_unit) == INTEGER_CST
	    && !TREE_OVERFLOW (gnu_max_size_unit))
	  {
	    TYPE_SIZE (tem) = size_binop (MIN_EXPR, gnu_max_size,
					  TYPE_SIZE (tem));
	    TYPE_SIZE_UNIT (tem) = size_binop (MIN_EXPR, gnu_max_size_unit,
					       TYPE_SIZE_UNIT (tem));
	  }

	create_type_decl (create_concat_name (gnat_entity, "XUA"),
			  tem, NULL, !Comes_From_Source (gnat_entity),
			  debug_info_p, gnat_entity);

	/* Give the fat pointer type a name.  If this is a packed type, tell
	   the debugger how to interpret the underlying bits.  */
	if (Present (Packed_Array_Type (gnat_entity)))
	  gnat_name = Packed_Array_Type (gnat_entity);
	else
	  gnat_name = gnat_entity;
	create_type_decl (create_concat_name (gnat_name, "XUP"),
			  gnu_fat_type, NULL, !Comes_From_Source (gnat_entity),
			  debug_info_p, gnat_entity);

	/* Create the type to be used as what a thin pointer designates:
	   a record type for the object and its template with the fields
	   shifted to have the template at a negative offset.  */
	tem = build_unc_object_type (gnu_template_type, tem,
				     create_concat_name (gnat_name, "XUT"),
				     debug_info_p);
	shift_unc_components_for_thin_pointers (tem);

	SET_TYPE_UNCONSTRAINED_ARRAY (tem, gnu_type);
	TYPE_OBJECT_RECORD_TYPE (gnu_type) = tem;
      }
      break;

    case E_String_Subtype:
    case E_Array_Subtype:

      /* This is the actual data type for array variables.  Multidimensional
	 arrays are implemented as arrays of arrays.  Note that arrays which
	 have sparse enumeration subtypes as index components create sparse
	 arrays, which is obviously space inefficient but so much easier to
	 code for now.

	 Also note that the subtype never refers to the unconstrained array
	 type, which is somewhat at variance with Ada semantics.

	 First check to see if this is simply a renaming of the array type.
	 If so, the result is the array type.  */

      gnu_type = gnat_to_gnu_type (Etype (gnat_entity));
      if (!Is_Constrained (gnat_entity))
	;
      else
	{
	  Entity_Id gnat_index, gnat_base_index;
	  const bool convention_fortran_p
	    = (Convention (gnat_entity) == Convention_Fortran);
	  const int ndim = Number_Dimensions (gnat_entity);
	  tree gnu_base_type = gnu_type;
	  tree *gnu_index_types = XALLOCAVEC (tree, ndim);
	  tree gnu_max_size = size_one_node, gnu_max_size_unit;
	  bool need_index_type_struct = false;
	  int index;

	  /* First create the GCC type for each index and find out whether
	     special types are needed for debugging information.  */
	  for (index = (convention_fortran_p ? ndim - 1 : 0),
	       gnat_index = First_Index (gnat_entity),
	       gnat_base_index
		 = First_Index (Implementation_Base_Type (gnat_entity));
	       0 <= index && index < ndim;
	       index += (convention_fortran_p ? - 1 : 1),
	       gnat_index = Next_Index (gnat_index),
	       gnat_base_index = Next_Index (gnat_base_index))
	    {
	      tree gnu_index_type = get_unpadded_type (Etype (gnat_index));
	      tree gnu_orig_min = TYPE_MIN_VALUE (gnu_index_type);
	      tree gnu_orig_max = TYPE_MAX_VALUE (gnu_index_type);
	      tree gnu_min = convert (sizetype, gnu_orig_min);
	      tree gnu_max = convert (sizetype, gnu_orig_max);
	      tree gnu_base_index_type
		= get_unpadded_type (Etype (gnat_base_index));
	      tree gnu_base_orig_min = TYPE_MIN_VALUE (gnu_base_index_type);
	      tree gnu_base_orig_max = TYPE_MAX_VALUE (gnu_base_index_type);
	      tree gnu_high;

	      /* See if the base array type is already flat.  If it is, we
		 are probably compiling an ACATS test but it will cause the
		 code below to malfunction if we don't handle it specially.  */
	      if (TREE_CODE (gnu_base_orig_min) == INTEGER_CST
		  && TREE_CODE (gnu_base_orig_max) == INTEGER_CST
		  && tree_int_cst_lt (gnu_base_orig_max, gnu_base_orig_min))
		{
		  gnu_min = size_one_node;
		  gnu_max = size_zero_node;
		  gnu_high = gnu_max;
		}

	      /* Similarly, if one of the values overflows in sizetype and the
		 range is null, use 1..0 for the sizetype bounds.  */
	      else if (TREE_CODE (gnu_min) == INTEGER_CST
		       && TREE_CODE (gnu_max) == INTEGER_CST
		       && (TREE_OVERFLOW (gnu_min) || TREE_OVERFLOW (gnu_max))
		       && tree_int_cst_lt (gnu_orig_max, gnu_orig_min))
		{
		  gnu_min = size_one_node;
		  gnu_max = size_zero_node;
		  gnu_high = gnu_max;
		}

	      /* If the minimum and maximum values both overflow in sizetype,
		 but the difference in the original type does not overflow in
		 sizetype, ignore the overflow indication.  */
	      else if (TREE_CODE (gnu_min) == INTEGER_CST
		       && TREE_CODE (gnu_max) == INTEGER_CST
		       && TREE_OVERFLOW (gnu_min) && TREE_OVERFLOW (gnu_max)
		       && !TREE_OVERFLOW
			   (convert (sizetype,
				     fold_build2 (MINUS_EXPR, gnu_index_type,
						  gnu_orig_max,
						  gnu_orig_min))))
		{
		  TREE_OVERFLOW (gnu_min) = 0;
		  TREE_OVERFLOW (gnu_max) = 0;
		  gnu_high = gnu_max;
		}

	      /* Compute the size of this dimension in the general case.  We
		 need to provide GCC with an upper bound to use but have to
		 deal with the "superflat" case.  There are three ways to do
		 this.  If we can prove that the array can never be superflat,
		 we can just use the high bound of the index type.  */
	      else if ((Nkind (gnat_index) == N_Range
		        && cannot_be_superflat_p (gnat_index))
		       /* Packed Array Types are never superflat.  */
		       || Is_Packed_Array_Type (gnat_entity))
		gnu_high = gnu_max;

	      /* Otherwise, if the high bound is constant but the low bound is
		 not, we use the expression (hb >= lb) ? lb : hb + 1 for the
		 lower bound.  Note that the comparison must be done in the
		 original type to avoid any overflow during the conversion.  */
	      else if (TREE_CODE (gnu_max) == INTEGER_CST
		       && TREE_CODE (gnu_min) != INTEGER_CST)
		{
		  gnu_high = gnu_max;
		  gnu_min
		    = build_cond_expr (sizetype,
				       build_binary_op (GE_EXPR,
							boolean_type_node,
							gnu_orig_max,
							gnu_orig_min),
				       gnu_min,
				       size_binop (PLUS_EXPR, gnu_max,
						   size_one_node));
		}

	      /* Finally we use (hb >= lb) ? hb : lb - 1 for the upper bound
		 in all the other cases.  Note that, here as well as above,
		 the condition used in the comparison must be equivalent to
		 the condition (length != 0).  This is relied upon in order
		 to optimize array comparisons in compare_arrays.  */
	      else
		gnu_high
		  = build_cond_expr (sizetype,
				     build_binary_op (GE_EXPR,
						      boolean_type_node,
						      gnu_orig_max,
						      gnu_orig_min),
				     gnu_max,
				     size_binop (MINUS_EXPR, gnu_min,
						 size_one_node));

	      /* Reuse the index type for the range type.  Then make an index
		 type with the size range in sizetype.  */
	      gnu_index_types[index]
		= create_index_type (gnu_min, gnu_high, gnu_index_type,
				     gnat_entity);

	      /* Update the maximum size of the array in elements.  Here we
		 see if any constraint on the index type of the base type
		 can be used in the case of self-referential bound on the
		 index type of the subtype.  We look for a non-"infinite"
		 and non-self-referential bound from any type involved and
		 handle each bound separately.  */
	      if (gnu_max_size)
		{
		  tree gnu_base_min = convert (sizetype, gnu_base_orig_min);
		  tree gnu_base_max = convert (sizetype, gnu_base_orig_max);
		  tree gnu_base_index_base_type
		    = get_base_type (gnu_base_index_type);
		  tree gnu_base_base_min
		    = convert (sizetype,
			       TYPE_MIN_VALUE (gnu_base_index_base_type));
		  tree gnu_base_base_max
		    = convert (sizetype,
			       TYPE_MAX_VALUE (gnu_base_index_base_type));

		  if (!CONTAINS_PLACEHOLDER_P (gnu_min)
		      || !(TREE_CODE (gnu_base_min) == INTEGER_CST
			   && !TREE_OVERFLOW (gnu_base_min)))
		    gnu_base_min = gnu_min;

		  if (!CONTAINS_PLACEHOLDER_P (gnu_max)
		      || !(TREE_CODE (gnu_base_max) == INTEGER_CST
			   && !TREE_OVERFLOW (gnu_base_max)))
		    gnu_base_max = gnu_max;

		  if ((TREE_CODE (gnu_base_min) == INTEGER_CST
		       && TREE_OVERFLOW (gnu_base_min))
		      || operand_equal_p (gnu_base_min, gnu_base_base_min, 0)
		      || (TREE_CODE (gnu_base_max) == INTEGER_CST
			  && TREE_OVERFLOW (gnu_base_max))
		      || operand_equal_p (gnu_base_max, gnu_base_base_max, 0))
		    gnu_max_size = NULL_TREE;
		  else
		    {
		      tree gnu_this_max
			= size_binop (MAX_EXPR,
				      size_binop (PLUS_EXPR, size_one_node,
						  size_binop (MINUS_EXPR,
							      gnu_base_max,
							      gnu_base_min)),
				      size_zero_node);

		      if (TREE_CODE (gnu_this_max) == INTEGER_CST
			  && TREE_OVERFLOW (gnu_this_max))
			gnu_max_size = NULL_TREE;
		      else
			gnu_max_size
			  = size_binop (MULT_EXPR, gnu_max_size, gnu_this_max);
		    }
		}

	      /* We need special types for debugging information to point to
		 the index types if they have variable bounds, are not integer
		 types, are biased or are wider than sizetype.  */
	      if (!integer_onep (gnu_orig_min)
		  || TREE_CODE (gnu_orig_max) != INTEGER_CST
		  || TREE_CODE (gnu_index_type) != INTEGER_TYPE
		  || (TREE_TYPE (gnu_index_type)
		      && TREE_CODE (TREE_TYPE (gnu_index_type))
			 != INTEGER_TYPE)
		  || TYPE_BIASED_REPRESENTATION_P (gnu_index_type)
		  || compare_tree_int (rm_size (gnu_index_type),
				       TYPE_PRECISION (sizetype)) > 0)
		need_index_type_struct = true;
	    }

	  /* Then flatten: create the array of arrays.  For an array type
	     used to implement a packed array, get the component type from
	     the original array type since the representation clauses that
	     can affect it are on the latter.  */
	  if (Is_Packed_Array_Type (gnat_entity)
	      && !Is_Bit_Packed_Array (Original_Array_Type (gnat_entity)))
	    {
	      gnu_type = gnat_to_gnu_type (Original_Array_Type (gnat_entity));
	      for (index = ndim - 1; index >= 0; index--)
		gnu_type = TREE_TYPE (gnu_type);

	      /* One of the above calls might have caused us to be elaborated,
		 so don't blow up if so.  */
	      if (present_gnu_tree (gnat_entity))
		{
		  maybe_present = true;
		  break;
		}
	    }
	  else
	    {
	      gnu_type = gnat_to_gnu_component_type (gnat_entity, definition,
						     debug_info_p);

	      /* One of the above calls might have caused us to be elaborated,
		 so don't blow up if so.  */
	      if (present_gnu_tree (gnat_entity))
		{
		  maybe_present = true;
		  break;
		}
	    }

	  /* Compute the maximum size of the array in units and bits.  */
	  if (gnu_max_size)
	    {
	      gnu_max_size_unit = size_binop (MULT_EXPR, gnu_max_size,
					      TYPE_SIZE_UNIT (gnu_type));
	      gnu_max_size = size_binop (MULT_EXPR,
					 convert (bitsizetype, gnu_max_size),
					 TYPE_SIZE (gnu_type));
	    }
	  else
	    gnu_max_size_unit = NULL_TREE;

	  /* Now build the array type.  */
	  for (index = ndim - 1; index >= 0; index --)
	    {
	      gnu_type = build_nonshared_array_type (gnu_type,
						     gnu_index_types[index]);
	      TYPE_MULTI_ARRAY_P (gnu_type) = (index > 0);
	      if (array_type_has_nonaliased_component (gnu_type, gnat_entity))
		TYPE_NONALIASED_COMPONENT (gnu_type) = 1;
	    }

	  /* Attach the TYPE_STUB_DECL in case we have a parallel type.  */
	  TYPE_STUB_DECL (gnu_type)
	    = create_type_stub_decl (gnu_entity_name, gnu_type);

	  /* If we are at file level and this is a multi-dimensional array,
	     we need to make a variable corresponding to the stride of the
	     inner dimensions.   */
	  if (global_bindings_p () && ndim > 1)
	    {
	      tree gnu_st_name = get_identifier ("ST");
	      tree gnu_arr_type;

	      for (gnu_arr_type = TREE_TYPE (gnu_type);
		   TREE_CODE (gnu_arr_type) == ARRAY_TYPE;
		   gnu_arr_type = TREE_TYPE (gnu_arr_type),
		   gnu_st_name = concat_name (gnu_st_name, "ST"))
		{
		  tree eltype = TREE_TYPE (gnu_arr_type);

		  TYPE_SIZE (gnu_arr_type)
		    = elaborate_expression_1 (TYPE_SIZE (gnu_arr_type),
					      gnat_entity, gnu_st_name,
					      definition, false);

		  /* ??? For now, store the size as a multiple of the
		     alignment of the element type in bytes so that we
		     can see the alignment from the tree.  */
		  TYPE_SIZE_UNIT (gnu_arr_type)
		    = elaborate_expression_2 (TYPE_SIZE_UNIT (gnu_arr_type),
					      gnat_entity,
					      concat_name (gnu_st_name, "A_U"),
					      definition, false,
					      TYPE_ALIGN (eltype));

		  /* ??? create_type_decl is not invoked on the inner types so
		     the MULT_EXPR node built above will never be marked.  */
		  MARK_VISITED (TYPE_SIZE_UNIT (gnu_arr_type));
		}
	    }

	  /* If we need to write out a record type giving the names of the
	     bounds for debugging purposes, do it now and make the record
	     type a parallel type.  This is not needed for a packed array
	     since the bounds are conveyed by the original array type.  */
	  if (need_index_type_struct
	      && debug_info_p
	      && !Is_Packed_Array_Type (gnat_entity))
	    {
	      tree gnu_bound_rec = make_node (RECORD_TYPE);
	      tree gnu_field_list = NULL_TREE;
	      tree gnu_field;

	      TYPE_NAME (gnu_bound_rec)
		= create_concat_name (gnat_entity, "XA");

	      for (index = ndim - 1; index >= 0; index--)
		{
		  tree gnu_index = TYPE_INDEX_TYPE (gnu_index_types[index]);
		  tree gnu_index_name = TYPE_NAME (gnu_index);

		  if (TREE_CODE (gnu_index_name) == TYPE_DECL)
		    gnu_index_name = DECL_NAME (gnu_index_name);

		  /* Make sure to reference the types themselves, and not just
		     their names, as the debugger may fall back on them.  */
		  gnu_field = create_field_decl (gnu_index_name, gnu_index,
						 gnu_bound_rec, NULL_TREE,
						 NULL_TREE, 0, 0);
		  DECL_CHAIN (gnu_field) = gnu_field_list;
		  gnu_field_list = gnu_field;
		}

	      finish_record_type (gnu_bound_rec, gnu_field_list, 0, true);
	      add_parallel_type (TYPE_STUB_DECL (gnu_type), gnu_bound_rec);
	    }

	  /* If this is a packed array type, make the original array type a
	     parallel type.  Otherwise, do it for the base array type if it
	     isn't artificial to make sure it is kept in the debug info.  */
	  if (debug_info_p)
	    {
	      if (Is_Packed_Array_Type (gnat_entity)
		  && present_gnu_tree (Original_Array_Type (gnat_entity)))
		add_parallel_type (TYPE_STUB_DECL (gnu_type),
				   gnat_to_gnu_type
				   (Original_Array_Type (gnat_entity)));
	      else
		{
		  tree gnu_base_decl
		    = gnat_to_gnu_entity (Etype (gnat_entity), NULL_TREE, 0);
		  if (!DECL_ARTIFICIAL (gnu_base_decl))
		    add_parallel_type (TYPE_STUB_DECL (gnu_type),
				       TREE_TYPE (TREE_TYPE (gnu_base_decl)));
		}
	    }

	  TYPE_CONVENTION_FORTRAN_P (gnu_type) = convention_fortran_p;
	  TYPE_PACKED_ARRAY_TYPE_P (gnu_type)
	    = (Is_Packed_Array_Type (gnat_entity)
	       && Is_Bit_Packed_Array (Original_Array_Type (gnat_entity)));

	  /* If the size is self-referential and the maximum size doesn't
	     overflow, use it.  */
	  if (CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_type))
	      && gnu_max_size
	      && !(TREE_CODE (gnu_max_size) == INTEGER_CST
		   && TREE_OVERFLOW (gnu_max_size))
	      && !(TREE_CODE (gnu_max_size_unit) == INTEGER_CST
		   && TREE_OVERFLOW (gnu_max_size_unit)))
	    {
	      TYPE_SIZE (gnu_type) = size_binop (MIN_EXPR, gnu_max_size,
						 TYPE_SIZE (gnu_type));
	      TYPE_SIZE_UNIT (gnu_type)
		= size_binop (MIN_EXPR, gnu_max_size_unit,
			      TYPE_SIZE_UNIT (gnu_type));
	    }

	  /* Set our alias set to that of our base type.  This gives all
	     array subtypes the same alias set.  */
	  relate_alias_sets (gnu_type, gnu_base_type, ALIAS_SET_COPY);

	  /* If this is a packed type, make this type the same as the packed
	     array type, but do some adjusting in the type first.  */
	  if (Present (Packed_Array_Type (gnat_entity)))
	    {
	      Entity_Id gnat_index;
	      tree gnu_inner;

	      /* First finish the type we had been making so that we output
		 debugging information for it.  */
	      if (Treat_As_Volatile (gnat_entity))
		gnu_type
		  = build_qualified_type (gnu_type,
					  TYPE_QUALS (gnu_type)
					  | TYPE_QUAL_VOLATILE);

	      /* Make it artificial only if the base type was artificial too.
		 That's sort of "morally" true and will make it possible for
		 the debugger to look it up by name in DWARF, which is needed
		 in order to decode the packed array type.  */
	      gnu_decl
		= create_type_decl (gnu_entity_name, gnu_type, attr_list,
				    !Comes_From_Source (Etype (gnat_entity))
				    && !Comes_From_Source (gnat_entity),
				    debug_info_p, gnat_entity);

	      /* Save it as our equivalent in case the call below elaborates
		 this type again.  */
	      save_gnu_tree (gnat_entity, gnu_decl, false);

	      gnu_decl = gnat_to_gnu_entity (Packed_Array_Type (gnat_entity),
					     NULL_TREE, 0);
	      this_made_decl = true;
	      gnu_type = TREE_TYPE (gnu_decl);
	      save_gnu_tree (gnat_entity, NULL_TREE, false);

	      gnu_inner = gnu_type;
	      while (TREE_CODE (gnu_inner) == RECORD_TYPE
		     && (TYPE_JUSTIFIED_MODULAR_P (gnu_inner)
			 || TYPE_PADDING_P (gnu_inner)))
		gnu_inner = TREE_TYPE (TYPE_FIELDS (gnu_inner));

	      /* We need to attach the index type to the type we just made so
		 that the actual bounds can later be put into a template.  */
	      if ((TREE_CODE (gnu_inner) == ARRAY_TYPE
		   && !TYPE_ACTUAL_BOUNDS (gnu_inner))
		  || (TREE_CODE (gnu_inner) == INTEGER_TYPE
		      && !TYPE_HAS_ACTUAL_BOUNDS_P (gnu_inner)))
		{
		  if (TREE_CODE (gnu_inner) == INTEGER_TYPE)
		    {
		      /* The TYPE_ACTUAL_BOUNDS field is overloaded with the
			 TYPE_MODULUS for modular types so we make an extra
			 subtype if necessary.  */
		      if (TYPE_MODULAR_P (gnu_inner))
			{
			  tree gnu_subtype
			    = make_unsigned_type (TYPE_PRECISION (gnu_inner));
			  TREE_TYPE (gnu_subtype) = gnu_inner;
			  TYPE_EXTRA_SUBTYPE_P (gnu_subtype) = 1;
			  SET_TYPE_RM_MIN_VALUE (gnu_subtype,
						 TYPE_MIN_VALUE (gnu_inner));
			  SET_TYPE_RM_MAX_VALUE (gnu_subtype,
						 TYPE_MAX_VALUE (gnu_inner));
			  gnu_inner = gnu_subtype;
			}

		      TYPE_HAS_ACTUAL_BOUNDS_P (gnu_inner) = 1;

#ifdef ENABLE_CHECKING
		      /* Check for other cases of overloading.  */
		      gcc_assert (!TYPE_ACTUAL_BOUNDS (gnu_inner));
#endif
		    }

		  for (gnat_index = First_Index (gnat_entity);
		       Present (gnat_index);
		       gnat_index = Next_Index (gnat_index))
		    SET_TYPE_ACTUAL_BOUNDS
		      (gnu_inner,
		       tree_cons (NULL_TREE,
				  get_unpadded_type (Etype (gnat_index)),
				  TYPE_ACTUAL_BOUNDS (gnu_inner)));

		  if (Convention (gnat_entity) != Convention_Fortran)
		    SET_TYPE_ACTUAL_BOUNDS
		      (gnu_inner, nreverse (TYPE_ACTUAL_BOUNDS (gnu_inner)));

		  if (TREE_CODE (gnu_type) == RECORD_TYPE
		      && TYPE_JUSTIFIED_MODULAR_P (gnu_type))
		    TREE_TYPE (TYPE_FIELDS (gnu_type)) = gnu_inner;
		}
	    }

	  else
	    /* Abort if packed array with no Packed_Array_Type field set.  */
	    gcc_assert (!Is_Packed (gnat_entity));
	}
      break;

    case E_String_Literal_Subtype:
      /* Create the type for a string literal.  */
      {
	Entity_Id gnat_full_type
	  = (IN (Ekind (Etype (gnat_entity)), Private_Kind)
	     && Present (Full_View (Etype (gnat_entity)))
	     ? Full_View (Etype (gnat_entity)) : Etype (gnat_entity));
	tree gnu_string_type = get_unpadded_type (gnat_full_type);
	tree gnu_string_array_type
	  = TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_string_type))));
	tree gnu_string_index_type
	  = get_base_type (TREE_TYPE (TYPE_INDEX_TYPE
				      (TYPE_DOMAIN (gnu_string_array_type))));
	tree gnu_lower_bound
	  = convert (gnu_string_index_type,
		     gnat_to_gnu (String_Literal_Low_Bound (gnat_entity)));
	int length = UI_To_Int (String_Literal_Length (gnat_entity));
	tree gnu_length = ssize_int (length - 1);
	tree gnu_upper_bound
	  = build_binary_op (PLUS_EXPR, gnu_string_index_type,
			     gnu_lower_bound,
			     convert (gnu_string_index_type, gnu_length));
	tree gnu_index_type
	  = create_index_type (convert (sizetype, gnu_lower_bound),
			       convert (sizetype, gnu_upper_bound),
			       create_range_type (gnu_string_index_type,
						  gnu_lower_bound,
						  gnu_upper_bound),
			       gnat_entity);

	gnu_type
	  = build_nonshared_array_type (gnat_to_gnu_type
					(Component_Type (gnat_entity)),
					gnu_index_type);
	if (array_type_has_nonaliased_component (gnu_type, gnat_entity))
	  TYPE_NONALIASED_COMPONENT (gnu_type) = 1;
	relate_alias_sets (gnu_type, gnu_string_type, ALIAS_SET_COPY);
      }
      break;

    /* Record Types and Subtypes

       The following fields are defined on record types:

		Has_Discriminants	True if the record has discriminants
		First_Discriminant      Points to head of list of discriminants
		First_Entity		Points to head of list of fields
		Is_Tagged_Type		True if the record is tagged

       Implementation of Ada records and discriminated records:

       A record type definition is transformed into the equivalent of a C
       struct definition.  The fields that are the discriminants which are
       found in the Full_Type_Declaration node and the elements of the
       Component_List found in the Record_Type_Definition node.  The
       Component_List can be a recursive structure since each Variant of
       the Variant_Part of the Component_List has a Component_List.

       Processing of a record type definition comprises starting the list of
       field declarations here from the discriminants and the calling the
       function components_to_record to add the rest of the fields from the
       component list and return the gnu type node.  The function
       components_to_record will call itself recursively as it traverses
       the tree.  */

    case E_Record_Type:
      if (Has_Complex_Representation (gnat_entity))
	{
	  gnu_type
	    = build_complex_type
	      (get_unpadded_type
	       (Etype (Defining_Entity
		       (First (Component_Items
			       (Component_List
				(Type_Definition
				 (Declaration_Node (gnat_entity)))))))));

	  break;
	}

      {
	Node_Id full_definition = Declaration_Node (gnat_entity);
	Node_Id record_definition = Type_Definition (full_definition);
	Entity_Id gnat_field;
	tree gnu_field, gnu_field_list = NULL_TREE, gnu_get_parent;
	/* Set PACKED in keeping with gnat_to_gnu_field.  */
	int packed
	  = Is_Packed (gnat_entity)
	    ? 1
	    : Component_Alignment (gnat_entity) == Calign_Storage_Unit
	      ? -1
	      : (Known_Alignment (gnat_entity)
		 || (Strict_Alignment (gnat_entity)
		     && Known_RM_Size (gnat_entity)))
		? -2
		: 0;
	bool has_discr = Has_Discriminants (gnat_entity);
	bool has_rep = Has_Specified_Layout (gnat_entity);
	bool all_rep = has_rep;
	bool is_extension
	  = (Is_Tagged_Type (gnat_entity)
	     && Nkind (record_definition) == N_Derived_Type_Definition);
	bool is_unchecked_union = Is_Unchecked_Union (gnat_entity);

	/* See if all fields have a rep clause.  Stop when we find one
	   that doesn't.  */
	if (all_rep)
	  for (gnat_field = First_Entity (gnat_entity);
	       Present (gnat_field);
	       gnat_field = Next_Entity (gnat_field))
	    if ((Ekind (gnat_field) == E_Component
		 || Ekind (gnat_field) == E_Discriminant)
		&& No (Component_Clause (gnat_field)))
	      {
		all_rep = false;
		break;
	      }

	/* If this is a record extension, go a level further to find the
	   record definition.  Also, verify we have a Parent_Subtype.  */
	if (is_extension)
	  {
	    if (!type_annotate_only
		|| Present (Record_Extension_Part (record_definition)))
	      record_definition = Record_Extension_Part (record_definition);

	    gcc_assert (type_annotate_only
			|| Present (Parent_Subtype (gnat_entity)));
	  }

	/* Make a node for the record.  If we are not defining the record,
	   suppress expanding incomplete types.  */
	gnu_type = make_node (tree_code_for_record_type (gnat_entity));
	TYPE_NAME (gnu_type) = gnu_entity_name;
	TYPE_PACKED (gnu_type) = (packed != 0) || has_rep;

	if (!definition)
	  {
	    defer_incomplete_level++;
	    this_deferred = true;
	  }

	/* If both a size and rep clause was specified, put the size in
	   the record type now so that it can get the proper mode.  */
	if (has_rep && Known_RM_Size (gnat_entity))
	  TYPE_SIZE (gnu_type)
	    = UI_To_gnu (RM_Size (gnat_entity), bitsizetype);

	/* Always set the alignment here so that it can be used to
	   set the mode, if it is making the alignment stricter.  If
	   it is invalid, it will be checked again below.  If this is to
	   be Atomic, choose a default alignment of a word unless we know
	   the size and it's smaller.  */
	if (Known_Alignment (gnat_entity))
	  TYPE_ALIGN (gnu_type)
	    = validate_alignment (Alignment (gnat_entity), gnat_entity, 0);
	else if (Is_Atomic (gnat_entity))
	  TYPE_ALIGN (gnu_type)
	    = esize >= BITS_PER_WORD ? BITS_PER_WORD : ceil_alignment (esize);
	/* If a type needs strict alignment, the minimum size will be the
	   type size instead of the RM size (see validate_size).  Cap the
	   alignment, lest it causes this type size to become too large.  */
	else if (Strict_Alignment (gnat_entity)
		 && Known_RM_Size (gnat_entity))
	  {
	    unsigned int raw_size = UI_To_Int (RM_Size (gnat_entity));
	    unsigned int raw_align = raw_size & -raw_size;
	    if (raw_align < BIGGEST_ALIGNMENT)
	      TYPE_ALIGN (gnu_type) = raw_align;
	  }
	else
	  TYPE_ALIGN (gnu_type) = 0;

	/* If we have a Parent_Subtype, make a field for the parent.  If
	   this record has rep clauses, force the position to zero.  */
	if (Present (Parent_Subtype (gnat_entity)))
	  {
	    Entity_Id gnat_parent = Parent_Subtype (gnat_entity);
	    tree gnu_parent;

	    /* A major complexity here is that the parent subtype will
	       reference our discriminants in its Discriminant_Constraint
	       list.  But those must reference the parent component of this
	       record which is of the parent subtype we have not built yet!
	       To break the circle we first build a dummy COMPONENT_REF which
	       represents the "get to the parent" operation and initialize
	       each of those discriminants to a COMPONENT_REF of the above
	       dummy parent referencing the corresponding discriminant of the
	       base type of the parent subtype.  */
	    gnu_get_parent = build3 (COMPONENT_REF, void_type_node,
				     build0 (PLACEHOLDER_EXPR, gnu_type),
				     build_decl (input_location,
						 FIELD_DECL, NULL_TREE,
						 void_type_node),
				     NULL_TREE);

	    if (has_discr)
	      for (gnat_field = First_Stored_Discriminant (gnat_entity);
		   Present (gnat_field);
		   gnat_field = Next_Stored_Discriminant (gnat_field))
		if (Present (Corresponding_Discriminant (gnat_field)))
		  {
		    tree gnu_field
		      = gnat_to_gnu_field_decl (Corresponding_Discriminant
						(gnat_field));
		    save_gnu_tree
		      (gnat_field,
		       build3 (COMPONENT_REF, TREE_TYPE (gnu_field),
			       gnu_get_parent, gnu_field, NULL_TREE),
		       true);
		  }

	    /* Then we build the parent subtype.  If it has discriminants but
	       the type itself has unknown discriminants, this means that it
	       doesn't contain information about how the discriminants are
	       derived from those of the ancestor type, so it cannot be used
	       directly.  Instead it is built by cloning the parent subtype
	       of the underlying record view of the type, for which the above
	       derivation of discriminants has been made explicit.  */
	    if (Has_Discriminants (gnat_parent)
		&& Has_Unknown_Discriminants (gnat_entity))
	      {
		Entity_Id gnat_uview = Underlying_Record_View (gnat_entity);

		/* If we are defining the type, the underlying record
		   view must already have been elaborated at this point.
		   Otherwise do it now as its parent subtype cannot be
		   technically elaborated on its own.  */
		if (definition)
		  gcc_assert (present_gnu_tree (gnat_uview));
		else
		  gnat_to_gnu_entity (gnat_uview, NULL_TREE, 0);

		gnu_parent = gnat_to_gnu_type (Parent_Subtype (gnat_uview));

		/* Substitute the "get to the parent" of the type for that
		   of its underlying record view in the cloned type.  */
		for (gnat_field = First_Stored_Discriminant (gnat_uview);
		     Present (gnat_field);
		     gnat_field = Next_Stored_Discriminant (gnat_field))
		  if (Present (Corresponding_Discriminant (gnat_field)))
		    {
		      tree gnu_field = gnat_to_gnu_field_decl (gnat_field);
		      tree gnu_ref
			= build3 (COMPONENT_REF, TREE_TYPE (gnu_field),
				  gnu_get_parent, gnu_field, NULL_TREE);
		      gnu_parent
			= substitute_in_type (gnu_parent, gnu_field, gnu_ref);
		    }
	      }
	    else
	      gnu_parent = gnat_to_gnu_type (gnat_parent);

	    /* Finally we fix up both kinds of twisted COMPONENT_REF we have
	       initially built.  The discriminants must reference the fields
	       of the parent subtype and not those of its base type for the
	       placeholder machinery to properly work.  */
	    if (has_discr)
	      {
		/* The actual parent subtype is the full view.  */
		if (IN (Ekind (gnat_parent), Private_Kind))
		  {
		    if (Present (Full_View (gnat_parent)))
		      gnat_parent = Full_View (gnat_parent);
		    else
		      gnat_parent = Underlying_Full_View (gnat_parent);
		  }

		for (gnat_field = First_Stored_Discriminant (gnat_entity);
		     Present (gnat_field);
		     gnat_field = Next_Stored_Discriminant (gnat_field))
		  if (Present (Corresponding_Discriminant (gnat_field)))
		    {
		      Entity_Id field = Empty;
		      for (field = First_Stored_Discriminant (gnat_parent);
			   Present (field);
			   field = Next_Stored_Discriminant (field))
			if (same_discriminant_p (gnat_field, field))
			  break;
		      gcc_assert (Present (field));
		      TREE_OPERAND (get_gnu_tree (gnat_field), 1)
			= gnat_to_gnu_field_decl (field);
		    }
	      }

	    /* The "get to the parent" COMPONENT_REF must be given its
	       proper type...  */
	    TREE_TYPE (gnu_get_parent) = gnu_parent;

	    /* ...and reference the _Parent field of this record.  */
	    gnu_field
	      = create_field_decl (parent_name_id,
				   gnu_parent, gnu_type,
				   has_rep
				   ? TYPE_SIZE (gnu_parent) : NULL_TREE,
				   has_rep
				   ? bitsize_zero_node : NULL_TREE,
				   0, 1);
	    DECL_INTERNAL_P (gnu_field) = 1;
	    TREE_OPERAND (gnu_get_parent, 1) = gnu_field;
	    TYPE_FIELDS (gnu_type) = gnu_field;
	  }

	/* Make the fields for the discriminants and put them into the record
	   unless it's an Unchecked_Union.  */
	if (has_discr)
	  for (gnat_field = First_Stored_Discriminant (gnat_entity);
	       Present (gnat_field);
	       gnat_field = Next_Stored_Discriminant (gnat_field))
	    {
	      /* If this is a record extension and this discriminant is the
		 renaming of another discriminant, we've handled it above.  */
	      if (Present (Parent_Subtype (gnat_entity))
		  && Present (Corresponding_Discriminant (gnat_field)))
		continue;

	      gnu_field
		= gnat_to_gnu_field (gnat_field, gnu_type, packed, definition,
				     debug_info_p);

	      /* Make an expression using a PLACEHOLDER_EXPR from the
		 FIELD_DECL node just created and link that with the
		 corresponding GNAT defining identifier.  */
	      save_gnu_tree (gnat_field,
			     build3 (COMPONENT_REF, TREE_TYPE (gnu_field),
				     build0 (PLACEHOLDER_EXPR, gnu_type),
				     gnu_field, NULL_TREE),
			     true);

	      if (!is_unchecked_union)
		{
		  DECL_CHAIN (gnu_field) = gnu_field_list;
		  gnu_field_list = gnu_field;
		}
	    }

	/* Add the fields into the record type and finish it up.  */
	components_to_record (gnu_type, Component_List (record_definition),
			      gnu_field_list, packed, definition, false,
			      all_rep, is_unchecked_union, debug_info_p,
			      false, OK_To_Reorder_Components (gnat_entity),
			      all_rep ? NULL_TREE : bitsize_zero_node, NULL);

	/* If it is passed by reference, force BLKmode to ensure that objects
	   of this type will always be put in memory.  */
	if (Is_By_Reference_Type (gnat_entity))
	  SET_TYPE_MODE (gnu_type, BLKmode);

	/* We used to remove the associations of the discriminants and _Parent
	   for validity checking but we may need them if there's a Freeze_Node
	   for a subtype used in this record.  */
	TYPE_VOLATILE (gnu_type) = Treat_As_Volatile (gnat_entity);

	/* Fill in locations of fields.  */
	annotate_rep (gnat_entity, gnu_type);

	/* If there are any entities in the chain corresponding to components
	   that we did not elaborate, ensure we elaborate their types if they
	   are Itypes.  */
	for (gnat_temp = First_Entity (gnat_entity);
	     Present (gnat_temp);
	     gnat_temp = Next_Entity (gnat_temp))
	  if ((Ekind (gnat_temp) == E_Component
	       || Ekind (gnat_temp) == E_Discriminant)
	      && Is_Itype (Etype (gnat_temp))
	      && !present_gnu_tree (gnat_temp))
	    gnat_to_gnu_entity (Etype (gnat_temp), NULL_TREE, 0);

	/* If this is a record type associated with an exception definition,
	   equate its fields to those of the standard exception type.  This
	   will make it possible to convert between them.  */
	if (gnu_entity_name == exception_data_name_id)
	  {
	    tree gnu_std_field;
	    for (gnu_field = TYPE_FIELDS (gnu_type),
		 gnu_std_field = TYPE_FIELDS (except_type_node);
		 gnu_field;
		 gnu_field = DECL_CHAIN (gnu_field),
		 gnu_std_field = DECL_CHAIN (gnu_std_field))
	      SET_DECL_ORIGINAL_FIELD_TO_FIELD (gnu_field, gnu_std_field);
	    gcc_assert (!gnu_std_field);
	  }
      }
      break;

    case E_Class_Wide_Subtype:
      /* If an equivalent type is present, that is what we should use.
	 Otherwise, fall through to handle this like a record subtype
	 since it may have constraints.  */
      if (gnat_equiv_type != gnat_entity)
	{
	  gnu_decl = gnat_to_gnu_entity (gnat_equiv_type, NULL_TREE, 0);
	  maybe_present = true;
	  break;
	}

      /* ... fall through ... */

    case E_Record_Subtype:
      /* If Cloned_Subtype is Present it means this record subtype has
	 identical layout to that type or subtype and we should use
	 that GCC type for this one.  The front end guarantees that
	 the component list is shared.  */
      if (Present (Cloned_Subtype (gnat_entity)))
	{
	  gnu_decl = gnat_to_gnu_entity (Cloned_Subtype (gnat_entity),
					 NULL_TREE, 0);
	  maybe_present = true;
	  break;
	}

      /* Otherwise, first ensure the base type is elaborated.  Then, if we are
	 changing the type, make a new type with each field having the type of
	 the field in the new subtype but the position computed by transforming
	 every discriminant reference according to the constraints.  We don't
	 see any difference between private and non-private type here since
	 derivations from types should have been deferred until the completion
	 of the private type.  */
      else
	{
	  Entity_Id gnat_base_type = Implementation_Base_Type (gnat_entity);
	  tree gnu_base_type;

	  if (!definition)
	    {
	      defer_incomplete_level++;
	      this_deferred = true;
	    }

	  gnu_base_type = gnat_to_gnu_type (gnat_base_type);

	  if (present_gnu_tree (gnat_entity))
	    {
	      maybe_present = true;
	      break;
	    }

	  /* If this is a record subtype associated with a dispatch table,
	     strip the suffix.  This is necessary to make sure 2 different
	     subtypes associated with the imported and exported views of a
	     dispatch table are properly merged in LTO mode.  */
	  if (Is_Dispatch_Table_Entity (gnat_entity))
	    {
	      char *p;
	      Get_Encoded_Name (gnat_entity);
	      p = strchr (Name_Buffer, '_');
	      gcc_assert (p);
	      strcpy (p+2, "dtS");
	      gnu_entity_name = get_identifier (Name_Buffer);
	    }

	  /* When the subtype has discriminants and these discriminants affect
	     the initial shape it has inherited, factor them in.  But for an
	     Unchecked_Union (it must be an Itype), just return the type.
	     We can't just test Is_Constrained because private subtypes without
	     discriminants of types with discriminants with default expressions
	     are Is_Constrained but aren't constrained!  */
	  if (IN (Ekind (gnat_base_type), Record_Kind)
	      && !Is_Unchecked_Union (gnat_base_type)
	      && !Is_For_Access_Subtype (gnat_entity)
	      && Is_Constrained (gnat_entity)
	      && Has_Discriminants (gnat_entity)
	      && Present (Discriminant_Constraint (gnat_entity))
	      && Stored_Constraint (gnat_entity) != No_Elist)
	    {
	      VEC(subst_pair,heap) *gnu_subst_list
		= build_subst_list (gnat_entity, gnat_base_type, definition);
	      tree gnu_unpad_base_type, gnu_rep_part, gnu_variant_part, t;
	      tree gnu_pos_list, gnu_field_list = NULL_TREE;
	      bool selected_variant = false;
	      Entity_Id gnat_field;
	      VEC(variant_desc,heap) *gnu_variant_list;

	      gnu_type = make_node (RECORD_TYPE);
	      TYPE_NAME (gnu_type) = gnu_entity_name;

	      /* Set the size, alignment and alias set of the new type to
		 match that of the old one, doing required substitutions.  */
	      copy_and_substitute_in_size (gnu_type, gnu_base_type,
					   gnu_subst_list);

	      if (TYPE_IS_PADDING_P (gnu_base_type))
		gnu_unpad_base_type = TREE_TYPE (TYPE_FIELDS (gnu_base_type));
	      else
		gnu_unpad_base_type = gnu_base_type;

	      /* Look for a REP part in the base type.  */
	      gnu_rep_part = get_rep_part (gnu_unpad_base_type);

	      /* Look for a variant part in the base type.  */
	      gnu_variant_part = get_variant_part (gnu_unpad_base_type);

	      /* If there is a variant part, we must compute whether the
		 constraints statically select a particular variant.  If
		 so, we simply drop the qualified union and flatten the
		 list of fields.  Otherwise we'll build a new qualified
		 union for the variants that are still relevant.  */
	      if (gnu_variant_part)
		{
		  variant_desc *v;
		  unsigned ix;

		  gnu_variant_list
		    = build_variant_list (TREE_TYPE (gnu_variant_part),
					  gnu_subst_list, NULL);

		  /* If all the qualifiers are unconditionally true, the
		     innermost variant is statically selected.  */
		  selected_variant = true;
		  FOR_EACH_VEC_ELT_REVERSE (variant_desc, gnu_variant_list,
					    ix, v)
		    if (!integer_onep (v->qual))
		      {
			selected_variant = false;
			break;
		      }

		  /* Otherwise, create the new variants.  */
		  if (!selected_variant)
		    FOR_EACH_VEC_ELT_REVERSE (variant_desc, gnu_variant_list,
					      ix, v)
		      {
			tree old_variant = v->type;
			tree new_variant = make_node (RECORD_TYPE);
			TYPE_NAME (new_variant)
			  = DECL_NAME (TYPE_NAME (old_variant));
			copy_and_substitute_in_size (new_variant, old_variant,
						     gnu_subst_list);
			v->record = new_variant;
		      }
		}
	      else
		{
		  gnu_variant_list = NULL;
		  selected_variant = false;
		}

	      gnu_pos_list
		= build_position_list (gnu_unpad_base_type,
				       gnu_variant_list && !selected_variant,
				       size_zero_node, bitsize_zero_node,
				       BIGGEST_ALIGNMENT, NULL_TREE);

	      for (gnat_field = First_Entity (gnat_entity);
		   Present (gnat_field);
		   gnat_field = Next_Entity (gnat_field))
		if ((Ekind (gnat_field) == E_Component
		     || Ekind (gnat_field) == E_Discriminant)
		    && !(Present (Corresponding_Discriminant (gnat_field))
			 && Is_Tagged_Type (gnat_base_type))
		    && Underlying_Type (Scope (Original_Record_Component
					       (gnat_field)))
		       == gnat_base_type)
		  {
		    Name_Id gnat_name = Chars (gnat_field);
		    Entity_Id gnat_old_field
		      = Original_Record_Component (gnat_field);
		    tree gnu_old_field
		      = gnat_to_gnu_field_decl (gnat_old_field);
		    tree gnu_context = DECL_CONTEXT (gnu_old_field);
		    tree gnu_field, gnu_field_type, gnu_size;
		    tree gnu_cont_type, gnu_last = NULL_TREE;

		    /* If the type is the same, retrieve the GCC type from the
		       old field to take into account possible adjustments.  */
		    if (Etype (gnat_field) == Etype (gnat_old_field))
		      gnu_field_type = TREE_TYPE (gnu_old_field);
		    else
		      gnu_field_type = gnat_to_gnu_type (Etype (gnat_field));

		    /* If there was a component clause, the field types must be
		       the same for the type and subtype, so copy the data from
		       the old field to avoid recomputation here.  Also if the
		       field is justified modular and the optimization in
		       gnat_to_gnu_field was applied.  */
		    if (Present (Component_Clause (gnat_old_field))
			|| (TREE_CODE (gnu_field_type) == RECORD_TYPE
			    && TYPE_JUSTIFIED_MODULAR_P (gnu_field_type)
			    && TREE_TYPE (TYPE_FIELDS (gnu_field_type))
			       == TREE_TYPE (gnu_old_field)))
		      {
			gnu_size = DECL_SIZE (gnu_old_field);
			gnu_field_type = TREE_TYPE (gnu_old_field);
		      }

		    /* If the old field was packed and of constant size, we
		       have to get the old size here, as it might differ from
		       what the Etype conveys and the latter might overlap
		       onto the following field.  Try to arrange the type for
		       possible better packing along the way.  */
		    else if (DECL_PACKED (gnu_old_field)
			     && TREE_CODE (DECL_SIZE (gnu_old_field))
			        == INTEGER_CST)
		      {
			gnu_size = DECL_SIZE (gnu_old_field);
			if (RECORD_OR_UNION_TYPE_P (gnu_field_type)
			    && !TYPE_FAT_POINTER_P (gnu_field_type)
			    && host_integerp (TYPE_SIZE (gnu_field_type), 1))
			  gnu_field_type
			    = make_packable_type (gnu_field_type, true);
		      }

		    else
		      gnu_size = TYPE_SIZE (gnu_field_type);

		    /* If the context of the old field is the base type or its
		       REP part (if any), put the field directly in the new
		       type; otherwise look up the context in the variant list
		       and put the field either in the new type if there is a
		       selected variant or in one of the new variants.  */
		    if (gnu_context == gnu_unpad_base_type
		        || (gnu_rep_part
			    && gnu_context == TREE_TYPE (gnu_rep_part)))
		      gnu_cont_type = gnu_type;
		    else
		      {
			variant_desc *v;
			unsigned ix;

			t = NULL_TREE;
			FOR_EACH_VEC_ELT_REVERSE (variant_desc,
						  gnu_variant_list, ix, v)
			  if (v->type == gnu_context)
			    {
			      t = v->type;
			      break;
			    }
			if (t)
			  {
			    if (selected_variant)
			      gnu_cont_type = gnu_type;
			    else
			      gnu_cont_type = v->record;
			  }
			else
			  /* The front-end may pass us "ghost" components if
			     it fails to recognize that a constrained subtype
			     is statically constrained.  Discard them.  */
			  continue;
		      }

		    /* Now create the new field modeled on the old one.  */
		    gnu_field
		      = create_field_decl_from (gnu_old_field, gnu_field_type,
						gnu_cont_type, gnu_size,
						gnu_pos_list, gnu_subst_list);

		    /* Put it in one of the new variants directly.  */
		    if (gnu_cont_type != gnu_type)
		      {
			DECL_CHAIN (gnu_field) = TYPE_FIELDS (gnu_cont_type);
			TYPE_FIELDS (gnu_cont_type) = gnu_field;
		      }

		    /* To match the layout crafted in components_to_record,
		       if this is the _Tag or _Parent field, put it before
		       any other fields.  */
		    else if (gnat_name == Name_uTag
			     || gnat_name == Name_uParent)
		      gnu_field_list = chainon (gnu_field_list, gnu_field);

		    /* Similarly, if this is the _Controller field, put
		       it before the other fields except for the _Tag or
		       _Parent field.  */
		    else if (gnat_name == Name_uController && gnu_last)
		      {
			DECL_CHAIN (gnu_field) = DECL_CHAIN (gnu_last);
			DECL_CHAIN (gnu_last) = gnu_field;
		      }

		    /* Otherwise, if this is a regular field, put it after
		       the other fields.  */
		    else
		      {
			DECL_CHAIN (gnu_field) = gnu_field_list;
			gnu_field_list = gnu_field;
			if (!gnu_last)
			  gnu_last = gnu_field;
		      }

		    save_gnu_tree (gnat_field, gnu_field, false);
		  }

	      /* If there is a variant list and no selected variant, we need
		 to create the nest of variant parts from the old nest.  */
	      if (gnu_variant_list && !selected_variant)
		{
		  tree new_variant_part
		    = create_variant_part_from (gnu_variant_part,
						gnu_variant_list, gnu_type,
						gnu_pos_list, gnu_subst_list);
		  DECL_CHAIN (new_variant_part) = gnu_field_list;
		  gnu_field_list = new_variant_part;
		}

	      /* Now go through the entities again looking for Itypes that
		 we have not elaborated but should (e.g., Etypes of fields
		 that have Original_Components).  */
	      for (gnat_field = First_Entity (gnat_entity);
		   Present (gnat_field); gnat_field = Next_Entity (gnat_field))
		if ((Ekind (gnat_field) == E_Discriminant
		     || Ekind (gnat_field) == E_Component)
		    && !present_gnu_tree (Etype (gnat_field)))
		  gnat_to_gnu_entity (Etype (gnat_field), NULL_TREE, 0);

	      /* Do not emit debug info for the type yet since we're going to
		 modify it below.  */
	      gnu_field_list = nreverse (gnu_field_list);
	      finish_record_type (gnu_type, gnu_field_list, 2, false);

	      /* See the E_Record_Type case for the rationale.  */
	      if (Is_By_Reference_Type (gnat_entity))
		SET_TYPE_MODE (gnu_type, BLKmode);
	      else
		compute_record_mode (gnu_type);

	      TYPE_VOLATILE (gnu_type) = Treat_As_Volatile (gnat_entity);

	      /* Fill in locations of fields.  */
	      annotate_rep (gnat_entity, gnu_type);

	      /* If debugging information is being written for the type, write
		 a record that shows what we are a subtype of and also make a
		 variable that indicates our size, if still variable.  */
	      if (debug_info_p)
		{
		  tree gnu_subtype_marker = make_node (RECORD_TYPE);
		  tree gnu_unpad_base_name = TYPE_NAME (gnu_unpad_base_type);
		  tree gnu_size_unit = TYPE_SIZE_UNIT (gnu_type);

		  if (TREE_CODE (gnu_unpad_base_name) == TYPE_DECL)
		    gnu_unpad_base_name = DECL_NAME (gnu_unpad_base_name);

		  TYPE_NAME (gnu_subtype_marker)
		    = create_concat_name (gnat_entity, "XVS");
		  finish_record_type (gnu_subtype_marker,
				      create_field_decl (gnu_unpad_base_name,
							 build_reference_type
							 (gnu_unpad_base_type),
							 gnu_subtype_marker,
							 NULL_TREE, NULL_TREE,
							 0, 0),
				      0, true);

		  add_parallel_type (TYPE_STUB_DECL (gnu_type),
				     gnu_subtype_marker);

		  if (definition
		      && TREE_CODE (gnu_size_unit) != INTEGER_CST
		      && !CONTAINS_PLACEHOLDER_P (gnu_size_unit))
		    TYPE_SIZE_UNIT (gnu_subtype_marker)
		      = create_var_decl (create_concat_name (gnat_entity,
							     "XVZ"),
					 NULL_TREE, sizetype, gnu_size_unit,
					 false, false, false, false, NULL,
					 gnat_entity);
		}

	      VEC_free (variant_desc, heap, gnu_variant_list);
	      VEC_free (subst_pair, heap, gnu_subst_list);

	      /* Now we can finalize it.  */
	      rest_of_record_type_compilation (gnu_type);
	    }

	  /* Otherwise, go down all the components in the new type and make
	     them equivalent to those in the base type.  */
	  else
	    {
	      gnu_type = gnu_base_type;

	      for (gnat_temp = First_Entity (gnat_entity);
		   Present (gnat_temp);
		   gnat_temp = Next_Entity (gnat_temp))
		if ((Ekind (gnat_temp) == E_Discriminant
		     && !Is_Unchecked_Union (gnat_base_type))
		    || Ekind (gnat_temp) == E_Component)
		  save_gnu_tree (gnat_temp,
				 gnat_to_gnu_field_decl
				 (Original_Record_Component (gnat_temp)),
				 false);
	    }
	}
      break;

    case E_Access_Subprogram_Type:
      /* Use the special descriptor type for dispatch tables if needed,
	 that is to say for the Prim_Ptr of a-tags.ads and its clones.
	 Note that we are only required to do so for static tables in
	 order to be compatible with the C++ ABI, but Ada 2005 allows
	 to extend library level tagged types at the local level so
	 we do it in the non-static case as well.  */
      if (TARGET_VTABLE_USES_DESCRIPTORS
	  && Is_Dispatch_Table_Entity (gnat_entity))
	{
	    gnu_type = fdesc_type_node;
	    gnu_size = TYPE_SIZE (gnu_type);
	    break;
	}

      /* ... fall through ... */

    case E_Anonymous_Access_Subprogram_Type:
      /* If we are not defining this entity, and we have incomplete
	 entities being processed above us, make a dummy type and
	 fill it in later.  */
      if (!definition && defer_incomplete_level != 0)
	{
	  struct incomplete *p = XNEW (struct incomplete);

	  gnu_type
	    = build_pointer_type
	      (make_dummy_type (Directly_Designated_Type (gnat_entity)));
	  gnu_decl = create_type_decl (gnu_entity_name, gnu_type, attr_list,
				       !Comes_From_Source (gnat_entity),
				       debug_info_p, gnat_entity);
	  this_made_decl = true;
	  gnu_type = TREE_TYPE (gnu_decl);
	  save_gnu_tree (gnat_entity, gnu_decl, false);
	  saved = true;

	  p->old_type = TREE_TYPE (gnu_type);
	  p->full_type = Directly_Designated_Type (gnat_entity);
	  p->next = defer_incomplete_list;
	  defer_incomplete_list = p;
	  break;
	}

      /* ... fall through ... */

    case E_Allocator_Type:
    case E_Access_Type:
    case E_Access_Attribute_Type:
    case E_Anonymous_Access_Type:
    case E_General_Access_Type:
      {
	/* The designated type and its equivalent type for gigi.  */
	Entity_Id gnat_desig_type = Directly_Designated_Type (gnat_entity);
	Entity_Id gnat_desig_equiv = Gigi_Equivalent_Type (gnat_desig_type);
	/* Whether it comes from a limited with.  */
	bool is_from_limited_with
	  = (IN (Ekind (gnat_desig_equiv), Incomplete_Kind)
	     && From_With_Type (gnat_desig_equiv));
	/* The "full view" of the designated type.  If this is an incomplete
	   entity from a limited with, treat its non-limited view as the full
	   view.  Otherwise, if this is an incomplete or private type, use the
	   full view.  In the former case, we might point to a private type,
	   in which case, we need its full view.  Also, we want to look at the
	   actual type used for the representation, so this takes a total of
	   three steps.  */
	Entity_Id gnat_desig_full_direct_first
	  = (is_from_limited_with
	     ? Non_Limited_View (gnat_desig_equiv)
	     : (IN (Ekind (gnat_desig_equiv), Incomplete_Or_Private_Kind)
		? Full_View (gnat_desig_equiv) : Empty));
	Entity_Id gnat_desig_full_direct
	  = ((is_from_limited_with
	      && Present (gnat_desig_full_direct_first)
	      && IN (Ekind (gnat_desig_full_direct_first), Private_Kind))
	     ? Full_View (gnat_desig_full_direct_first)
	     : gnat_desig_full_direct_first);
	Entity_Id gnat_desig_full
	  = Gigi_Equivalent_Type (gnat_desig_full_direct);
	/* The type actually used to represent the designated type, either
	   gnat_desig_full or gnat_desig_equiv.  */
	Entity_Id gnat_desig_rep;
	/* True if this is a pointer to an unconstrained array.  */
	bool is_unconstrained_array;
	/* We want to know if we'll be seeing the freeze node for any
	   incomplete type we may be pointing to.  */
	bool in_main_unit
	  = (Present (gnat_desig_full)
	     ? In_Extended_Main_Code_Unit (gnat_desig_full)
	     : In_Extended_Main_Code_Unit (gnat_desig_type));
	/* True if we make a dummy type here.  */
	bool made_dummy = false;
	/* The mode to be used for the pointer type.  */
	enum machine_mode p_mode = mode_for_size (esize, MODE_INT, 0);
	/* The GCC type used for the designated type.  */
	tree gnu_desig_type = NULL_TREE;

	if (!targetm.valid_pointer_mode (p_mode))
	  p_mode = ptr_mode;

	/* If either the designated type or its full view is an unconstrained
	   array subtype, replace it with the type it's a subtype of.  This
	   avoids problems with multiple copies of unconstrained array types.
	   Likewise, if the designated type is a subtype of an incomplete
	   record type, use the parent type to avoid order of elaboration
	   issues.  This can lose some code efficiency, but there is no
	   alternative.  */
	if (Ekind (gnat_desig_equiv) == E_Array_Subtype
	    && !Is_Constrained (gnat_desig_equiv))
	  gnat_desig_equiv = Etype (gnat_desig_equiv);
	if (Present (gnat_desig_full)
	    && ((Ekind (gnat_desig_full) == E_Array_Subtype
		 && !Is_Constrained (gnat_desig_full))
		|| (Ekind (gnat_desig_full) == E_Record_Subtype
		    && Ekind (Etype (gnat_desig_full)) == E_Record_Type)))
	  gnat_desig_full = Etype (gnat_desig_full);

	/* Set the type that's actually the representation of the designated
	   type and also flag whether we have a unconstrained array.  */
	gnat_desig_rep
	  = Present (gnat_desig_full) ? gnat_desig_full : gnat_desig_equiv;
	is_unconstrained_array
	  = Is_Array_Type (gnat_desig_rep) && !Is_Constrained (gnat_desig_rep);

	/* If we are pointing to an incomplete type whose completion is an
	   unconstrained array, make dummy fat and thin pointer types to it.
	   Likewise if the type itself is dummy or an unconstrained array.  */
	if (is_unconstrained_array
	    && (Present (gnat_desig_full)
		|| (present_gnu_tree (gnat_desig_equiv)
		    && TYPE_IS_DUMMY_P
		       (TREE_TYPE (get_gnu_tree (gnat_desig_equiv))))
		|| (!in_main_unit
		    && defer_incomplete_level != 0
		    && !present_gnu_tree (gnat_desig_equiv))
		|| (in_main_unit
		    && is_from_limited_with
		    && Present (Freeze_Node (gnat_desig_equiv)))))
	  {
	    if (present_gnu_tree (gnat_desig_rep))
	      gnu_desig_type = TREE_TYPE (get_gnu_tree (gnat_desig_rep));
	    else
	      {
		gnu_desig_type = make_dummy_type (gnat_desig_rep);
		made_dummy = true;
	      }

	    /* If the call above got something that has a pointer, the pointer
	       is our type.  This could have happened either because the type
	       was elaborated or because somebody else executed the code.  */
	    if (!TYPE_POINTER_TO (gnu_desig_type))
	      build_dummy_unc_pointer_types (gnat_desig_equiv, gnu_desig_type);
	    gnu_type = TYPE_POINTER_TO (gnu_desig_type);
	  }

	/* If we already know what the full type is, use it.  */
	else if (Present (gnat_desig_full)
		 && present_gnu_tree (gnat_desig_full))
	  gnu_desig_type = TREE_TYPE (get_gnu_tree (gnat_desig_full));

	/* Get the type of the thing we are to point to and build a pointer to
	   it.  If it is a reference to an incomplete or private type with a
	   full view that is a record, make a dummy type node and get the
	   actual type later when we have verified it is safe.  */
	else if ((!in_main_unit
		  && !present_gnu_tree (gnat_desig_equiv)
		  && Present (gnat_desig_full)
		  && !present_gnu_tree (gnat_desig_full)
		  && Is_Record_Type (gnat_desig_full))
		 /* Likewise if we are pointing to a record or array and we are
		    to defer elaborating incomplete types.  We do this as this
		    access type may be the full view of a private type.  Note
		    that the unconstrained array case is handled above.  */
		 || ((!in_main_unit || imported_p)
		     && defer_incomplete_level != 0
		     && !present_gnu_tree (gnat_desig_equiv)
		     && (Is_Record_Type (gnat_desig_rep)
			 || Is_Array_Type (gnat_desig_rep)))
		 /* If this is a reference from a limited_with type back to our
		    main unit and there's a freeze node for it, either we have
		    already processed the declaration and made the dummy type,
		    in which case we just reuse the latter, or we have not yet,
		    in which case we make the dummy type and it will be reused
		    when the declaration is finally processed.  In both cases,
		    the pointer eventually created below will be automatically
		    adjusted when the freeze node is processed.  Note that the
		    unconstrained array case is handled above.  */
		 ||  (in_main_unit
		      && is_from_limited_with
		      && Present (Freeze_Node (gnat_desig_rep))))
	  {
	    gnu_desig_type = make_dummy_type (gnat_desig_equiv);
	    made_dummy = true;
	  }

	/* Otherwise handle the case of a pointer to itself.  */
	else if (gnat_desig_equiv == gnat_entity)
	  {
	    gnu_type
	      = build_pointer_type_for_mode (void_type_node, p_mode,
					     No_Strict_Aliasing (gnat_entity));
	    TREE_TYPE (gnu_type) = TYPE_POINTER_TO (gnu_type) = gnu_type;
	  }

	/* If expansion is disabled, the equivalent type of a concurrent type
	   is absent, so build a dummy pointer type.  */
	else if (type_annotate_only && No (gnat_desig_equiv))
	  gnu_type = ptr_void_type_node;

	/* Finally, handle the default case where we can just elaborate our
	   designated type.  */
	else
	  gnu_desig_type = gnat_to_gnu_type (gnat_desig_equiv);

	/* It is possible that a call to gnat_to_gnu_type above resolved our
	   type.  If so, just return it.  */
	if (present_gnu_tree (gnat_entity))
	  {
	    maybe_present = true;
	    break;
	  }

	/* If we have not done it yet, build the pointer type the usual way.  */
	if (!gnu_type)
	  {
	    /* Modify the designated type if we are pointing only to constant
	       objects, but don't do it for unconstrained arrays.  */
	    if (Is_Access_Constant (gnat_entity)
		&& TREE_CODE (gnu_desig_type) != UNCONSTRAINED_ARRAY_TYPE)
	      {
		gnu_desig_type
		  = build_qualified_type
		    (gnu_desig_type,
		     TYPE_QUALS (gnu_desig_type) | TYPE_QUAL_CONST);

		/* Some extra processing is required if we are building a
		   pointer to an incomplete type (in the GCC sense).  We might
		   have such a type if we just made a dummy, or directly out
		   of the call to gnat_to_gnu_type above if we are processing
		   an access type for a record component designating the
		   record type itself.  */
		if (TYPE_MODE (gnu_desig_type) == VOIDmode)
		  {
		    /* We must ensure that the pointer to variant we make will
		       be processed by update_pointer_to when the initial type
		       is completed.  Pretend we made a dummy and let further
		       processing act as usual.  */
		    made_dummy = true;

		    /* We must ensure that update_pointer_to will not retrieve
		       the dummy variant when building a properly qualified
		       version of the complete type.  We take advantage of the
		       fact that get_qualified_type is requiring TYPE_NAMEs to
		       match to influence build_qualified_type and then also
		       update_pointer_to here.  */
		    TYPE_NAME (gnu_desig_type)
		      = create_concat_name (gnat_desig_type, "INCOMPLETE_CST");
		  }
	      }

	    gnu_type
	      = build_pointer_type_for_mode (gnu_desig_type, p_mode,
					     No_Strict_Aliasing (gnat_entity));
	  }

	/* If we are not defining this object and we have made a dummy pointer,
	   save our current definition, evaluate the actual type, and replace
	   the tentative type we made with the actual one.  If we are to defer
	   actually looking up the actual type, make an entry in the deferred
	   list.  If this is from a limited with, we may have to defer to the
	   end of the current unit.  */
	if ((!in_main_unit || is_from_limited_with) && made_dummy)
	  {
	    tree gnu_old_desig_type;

	    if (TYPE_IS_FAT_POINTER_P (gnu_type))
	      {
		gnu_old_desig_type = TYPE_UNCONSTRAINED_ARRAY (gnu_type);
		if (esize == POINTER_SIZE)
		  gnu_type = build_pointer_type
			     (TYPE_OBJECT_RECORD_TYPE (gnu_old_desig_type));
	      }
	    else
	      gnu_old_desig_type = TREE_TYPE (gnu_type);

	    gnu_decl = create_type_decl (gnu_entity_name, gnu_type, attr_list,
					 !Comes_From_Source (gnat_entity),
					 debug_info_p, gnat_entity);
	    this_made_decl = true;
	    gnu_type = TREE_TYPE (gnu_decl);
	    save_gnu_tree (gnat_entity, gnu_decl, false);
	    saved = true;

	    /* Note that the call to gnat_to_gnu_type on gnat_desig_equiv might
	       update gnu_old_desig_type directly, in which case it will not be
	       a dummy type any more when we get into update_pointer_to.

	       This can happen e.g. when the designated type is a record type,
	       because their elaboration starts with an initial node from
	       make_dummy_type, which may be the same node as the one we got.

	       Besides, variants of this non-dummy type might have been created
	       along the way.  update_pointer_to is expected to properly take
	       care of those situations.  */
	    if (defer_incomplete_level == 0 && !is_from_limited_with)
	      {
		defer_finalize_level++;
		update_pointer_to (TYPE_MAIN_VARIANT (gnu_old_desig_type),
				   gnat_to_gnu_type (gnat_desig_equiv));
		defer_finalize_level--;
	      }
	    else
	      {
		struct incomplete *p = XNEW (struct incomplete);
		struct incomplete **head
		  = (is_from_limited_with
		     ? &defer_limited_with : &defer_incomplete_list);
		p->old_type = gnu_old_desig_type;
		p->full_type = gnat_desig_equiv;
		p->next = *head;
		*head = p;
	      }
	  }
      }
      break;

    case E_Access_Protected_Subprogram_Type:
    case E_Anonymous_Access_Protected_Subprogram_Type:
      if (type_annotate_only && No (gnat_equiv_type))
	gnu_type = ptr_void_type_node;
      else
	{
	  /* The run-time representation is the equivalent type.  */
	  gnu_type = gnat_to_gnu_type (gnat_equiv_type);
	  maybe_present = true;
	}

      if (Is_Itype (Directly_Designated_Type (gnat_entity))
	  && !present_gnu_tree (Directly_Designated_Type (gnat_entity))
	  && No (Freeze_Node (Directly_Designated_Type (gnat_entity)))
	  && !Is_Record_Type (Scope (Directly_Designated_Type (gnat_entity))))
	gnat_to_gnu_entity (Directly_Designated_Type (gnat_entity),
			    NULL_TREE, 0);

      break;

    case E_Access_Subtype:

      /* We treat this as identical to its base type; any constraint is
	 meaningful only to the front-end.

	 The designated type must be elaborated as well, if it does
	 not have its own freeze node.  Designated (sub)types created
	 for constrained components of records with discriminants are
	 not frozen by the front-end and thus not elaborated by gigi,
	 because their use may appear before the base type is frozen,
	 and because it is not clear that they are needed anywhere in
	 gigi.  With the current model, there is no correct place where
	 they could be elaborated.  */

      gnu_type = gnat_to_gnu_type (Etype (gnat_entity));
      if (Is_Itype (Directly_Designated_Type (gnat_entity))
	  && !present_gnu_tree (Directly_Designated_Type (gnat_entity))
	  && Is_Frozen (Directly_Designated_Type (gnat_entity))
	  && No (Freeze_Node (Directly_Designated_Type (gnat_entity))))
	{
	  /* If we are not defining this entity, and we have incomplete
	     entities being processed above us, make a dummy type and
	     elaborate it later.  */
	  if (!definition && defer_incomplete_level != 0)
	    {
	      struct incomplete *p = XNEW (struct incomplete);

	      p->old_type
		= make_dummy_type (Directly_Designated_Type (gnat_entity));
	      p->full_type = Directly_Designated_Type (gnat_entity);
	      p->next = defer_incomplete_list;
	      defer_incomplete_list = p;
	    }
	  else if (!IN (Ekind (Base_Type
			       (Directly_Designated_Type (gnat_entity))),
		        Incomplete_Or_Private_Kind))
	    gnat_to_gnu_entity (Directly_Designated_Type (gnat_entity),
				NULL_TREE, 0);
	}

      maybe_present = true;
      break;

    /* Subprogram Entities

       The following access functions are defined for subprograms:

		Etype       	Return type or Standard_Void_Type.
		First_Formal	The first formal parameter.
		Is_Imported     Indicates that the subprogram has appeared in
				an INTERFACE or IMPORT pragma.  For now we
				assume that the external language is C.
		Is_Exported     Likewise but for an EXPORT pragma.
		Is_Inlined      True if the subprogram is to be inlined.

       Each parameter is first checked by calling must_pass_by_ref on its
       type to determine if it is passed by reference.  For parameters which
       are copied in, if they are Ada In Out or Out parameters, their return
       value becomes part of a record which becomes the return type of the
       function (C function - note that this applies only to Ada procedures
       so there is no Ada return type).  Additional code to store back the
       parameters will be generated on the caller side.  This transformation
       is done here, not in the front-end.

       The intended result of the transformation can be seen from the
       equivalent source rewritings that follow:

						struct temp {int a,b};
       procedure P (A,B: In Out ...) is		temp P (int A,B)
       begin					{
	 ..					  ..
       end P;					  return {A,B};
						}

						temp t;
       P(X,Y);					t = P(X,Y);
						X = t.a , Y = t.b;

       For subprogram types we need to perform mainly the same conversions to
       GCC form that are needed for procedures and function declarations.  The
       only difference is that at the end, we make a type declaration instead
       of a function declaration.  */

    case E_Subprogram_Type:
    case E_Function:
    case E_Procedure:
      {
	/* The type returned by a function or else Standard_Void_Type for a
	   procedure.  */
	Entity_Id gnat_return_type = Etype (gnat_entity);
	tree gnu_return_type;
	/* The first GCC parameter declaration (a PARM_DECL node).  The
	   PARM_DECL nodes are chained through the DECL_CHAIN field, so this
	   actually is the head of this parameter list.  */
	tree gnu_param_list = NULL_TREE;
	/* Likewise for the stub associated with an exported procedure.  */
	tree gnu_stub_param_list = NULL_TREE;
	/* Non-null for subprograms containing parameters passed by copy-in
	   copy-out (Ada In Out or Out parameters not passed by reference),
	   in which case it is the list of nodes used to specify the values
	   of the In Out/Out parameters that are returned as a record upon
	   procedure return.  The TREE_PURPOSE of an element of this list is
	   a field of the record and the TREE_VALUE is the PARM_DECL
	   corresponding to that field.  This list will be saved in the
	   TYPE_CI_CO_LIST field of the FUNCTION_TYPE node we create.  */
	tree gnu_cico_list = NULL_TREE;
	/* List of fields in return type of procedure with copy-in copy-out
	   parameters.  */
	tree gnu_field_list = NULL_TREE;
	/* If an import pragma asks to map this subprogram to a GCC builtin,
	   this is the builtin DECL node.  */
	tree gnu_builtin_decl = NULL_TREE;
	/* For the stub associated with an exported procedure.  */
	tree gnu_stub_type = NULL_TREE, gnu_stub_name = NULL_TREE;
	tree gnu_ext_name = create_concat_name (gnat_entity, NULL);
	Entity_Id gnat_param;
	bool inline_flag = Is_Inlined (gnat_entity);
	bool public_flag = Is_Public (gnat_entity) || imported_p;
	bool extern_flag
	  = (Is_Public (gnat_entity) && !definition) || imported_p;
	bool artificial_flag = !Comes_From_Source (gnat_entity);
       /* The semantics of "pure" in Ada essentially matches that of "const"
          in the back-end.  In particular, both properties are orthogonal to
          the "nothrow" property if the EH circuitry is explicit in the
          internal representation of the back-end.  If we are to completely
          hide the EH circuitry from it, we need to declare that calls to pure
          Ada subprograms that can throw have side effects since they can
          trigger an "abnormal" transfer of control flow; thus they can be
          neither "const" nor "pure" in the back-end sense.  */
	bool const_flag
	  = (Exception_Mechanism == Back_End_Exceptions
	     && Is_Pure (gnat_entity));
	bool volatile_flag = No_Return (gnat_entity);
	bool return_by_direct_ref_p = false;
	bool return_by_invisi_ref_p = false;
	bool return_unconstrained_p = false;
	bool has_stub = false;
	int parmnum;

	/* A parameter may refer to this type, so defer completion of any
	   incomplete types.  */
	if (kind == E_Subprogram_Type && !definition)
	  {
	    defer_incomplete_level++;
	    this_deferred = true;
	  }

	/* If the subprogram has an alias, it is probably inherited, so
	   we can use the original one.  If the original "subprogram"
	   is actually an enumeration literal, it may be the first use
	   of its type, so we must elaborate that type now.  */
	if (Present (Alias (gnat_entity)))
	  {
	    if (Ekind (Alias (gnat_entity)) == E_Enumeration_Literal)
	      gnat_to_gnu_entity (Etype (Alias (gnat_entity)), NULL_TREE, 0);

	    gnu_decl = gnat_to_gnu_entity (Alias (gnat_entity), gnu_expr, 0);

	    /* Elaborate any Itypes in the parameters of this entity.  */
	    for (gnat_temp = First_Formal_With_Extras (gnat_entity);
		 Present (gnat_temp);
		 gnat_temp = Next_Formal_With_Extras (gnat_temp))
	      if (Is_Itype (Etype (gnat_temp)))
		gnat_to_gnu_entity (Etype (gnat_temp), NULL_TREE, 0);

	    break;
	  }

	/* If this subprogram is expectedly bound to a GCC builtin, fetch the
	   corresponding DECL node.  Proper generation of calls later on need
	   proper parameter associations so we don't "break;" here.  */
	if (Convention (gnat_entity) == Convention_Intrinsic
	    && Present (Interface_Name (gnat_entity)))
	  {
	    gnu_builtin_decl = builtin_decl_for (gnu_ext_name);

	    /* Inability to find the builtin decl most often indicates a
	       genuine mistake, but imports of unregistered intrinsics are
	       sometimes issued on purpose to allow hooking in alternate
	       bodies.  We post a warning conditioned on Wshadow in this case,
	       to let developers be notified on demand without risking false
	       positives with common default sets of options.  */

	    if (gnu_builtin_decl == NULL_TREE && warn_shadow)
	      post_error ("?gcc intrinsic not found for&!", gnat_entity);
	  }

	/* ??? What if we don't find the builtin node above ? warn ? err ?
	   In the current state we neither warn nor err, and calls will just
	   be handled as for regular subprograms.  */

	/* Look into the return type and get its associated GCC tree.  If it
	   is not void, compute various flags for the subprogram type.  */
	if (Ekind (gnat_return_type) == E_Void)
	  gnu_return_type = void_type_node;
	else
	  {
	    gnu_return_type = gnat_to_gnu_type (gnat_return_type);

	    /* If this function returns by reference, make the actual return
	       type the pointer type and make a note of that.  */
	    if (Returns_By_Ref (gnat_entity))
	      {
		gnu_return_type = build_pointer_type (gnu_return_type);
		return_by_direct_ref_p = true;
	      }

	    /* If we are supposed to return an unconstrained array type, make
	       the actual return type the fat pointer type.  */
	    else if (TREE_CODE (gnu_return_type) == UNCONSTRAINED_ARRAY_TYPE)
	      {
		gnu_return_type = TREE_TYPE (gnu_return_type);
		return_unconstrained_p = true;
	      }

	    /* Likewise, if the return type requires a transient scope, the
	       return value will be allocated on the secondary stack so the
	       actual return type is the pointer type.  */
	    else if (Requires_Transient_Scope (gnat_return_type))
	      {
		gnu_return_type = build_pointer_type (gnu_return_type);
		return_unconstrained_p = true;
	      }

	    /* If the Mechanism is By_Reference, ensure this function uses the
	       target's by-invisible-reference mechanism, which may not be the
	       same as above (e.g. it might be passing an extra parameter).  */
	    else if (kind == E_Function
		     && Mechanism (gnat_entity) == By_Reference)
	      return_by_invisi_ref_p = true;

	    /* Likewise, if the return type is itself By_Reference.  */
	    else if (TREE_ADDRESSABLE (gnu_return_type))
	      return_by_invisi_ref_p = true;

	    /* If the type is a padded type and the underlying type would not
	       be passed by reference or the function has a foreign convention,
	       return the underlying type.  */
	    else if (TYPE_IS_PADDING_P (gnu_return_type)
		     && (!default_pass_by_ref
			  (TREE_TYPE (TYPE_FIELDS (gnu_return_type)))
			 || Has_Foreign_Convention (gnat_entity)))
	      gnu_return_type = TREE_TYPE (TYPE_FIELDS (gnu_return_type));

	    /* If the return type is unconstrained, that means it must have a
	       maximum size.  Use the padded type as the effective return type.
	       And ensure the function uses the target's by-invisible-reference
	       mechanism to avoid copying too much data when it returns.  */
	    if (CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_return_type)))
	      {
		gnu_return_type
		  = maybe_pad_type (gnu_return_type,
				    max_size (TYPE_SIZE (gnu_return_type),
					      true),
				    0, gnat_entity, false, false, false, true);

		/* Declare it now since it will never be declared otherwise.
		   This is necessary to ensure that its subtrees are properly
		   marked.  */
		create_type_decl (TYPE_NAME (gnu_return_type), gnu_return_type,
				  NULL, true, debug_info_p, gnat_entity);

		return_by_invisi_ref_p = true;
	      }

	    /* If the return type has a size that overflows, we cannot have
	       a function that returns that type.  This usage doesn't make
	       sense anyway, so give an error here.  */
	    if (TYPE_SIZE_UNIT (gnu_return_type)
		&& TREE_CONSTANT (TYPE_SIZE_UNIT (gnu_return_type))
		&& TREE_OVERFLOW (TYPE_SIZE_UNIT (gnu_return_type)))
	      {
		post_error ("cannot return type whose size overflows",
			    gnat_entity);
		gnu_return_type = copy_node (gnu_return_type);
		TYPE_SIZE (gnu_return_type) = bitsize_zero_node;
		TYPE_SIZE_UNIT (gnu_return_type) = size_zero_node;
		TYPE_MAIN_VARIANT (gnu_return_type) = gnu_return_type;
		TYPE_NEXT_VARIANT (gnu_return_type) = NULL_TREE;
	      }
	  }

	/* Loop over the parameters and get their associated GCC tree.  While
	   doing this, build a copy-in copy-out structure if we need one.  */
	for (gnat_param = First_Formal_With_Extras (gnat_entity), parmnum = 0;
	     Present (gnat_param);
	     gnat_param = Next_Formal_With_Extras (gnat_param), parmnum++)
	  {
	    tree gnu_param_name = get_entity_name (gnat_param);
	    tree gnu_param_type = gnat_to_gnu_type (Etype (gnat_param));
	    tree gnu_param, gnu_field;
	    bool copy_in_copy_out = false;
	    Mechanism_Type mech = Mechanism (gnat_param);

	    /* Builtins are expanded inline and there is no real call sequence
	       involved.  So the type expected by the underlying expander is
	       always the type of each argument "as is".  */
	    if (gnu_builtin_decl)
	      mech = By_Copy;
	    /* Handle the first parameter of a valued procedure specially.  */
	    else if (Is_Valued_Procedure (gnat_entity) && parmnum == 0)
	      mech = By_Copy_Return;
	    /* Otherwise, see if a Mechanism was supplied that forced this
	       parameter to be passed one way or another.  */
	    else if (mech == Default
		     || mech == By_Copy || mech == By_Reference)
	      ;
	    else if (By_Descriptor_Last <= mech && mech <= By_Descriptor)
	      mech = By_Descriptor;

	    else if (By_Short_Descriptor_Last <= mech &&
                     mech <= By_Short_Descriptor)
	      mech = By_Short_Descriptor;

	    else if (mech > 0)
	      {
		if (TREE_CODE (gnu_param_type) == UNCONSTRAINED_ARRAY_TYPE
		    || TREE_CODE (TYPE_SIZE (gnu_param_type)) != INTEGER_CST
		    || 0 < compare_tree_int (TYPE_SIZE (gnu_param_type),
					     mech))
		  mech = By_Reference;
		else
		  mech = By_Copy;
	      }
	    else
	      {
		post_error ("unsupported mechanism for&", gnat_param);
		mech = Default;
	      }

	    gnu_param
	      = gnat_to_gnu_param (gnat_param, mech, gnat_entity,
				   Has_Foreign_Convention (gnat_entity),
				   &copy_in_copy_out);

	    /* We are returned either a PARM_DECL or a type if no parameter
	       needs to be passed; in either case, adjust the type.  */
	    if (DECL_P (gnu_param))
	      gnu_param_type = TREE_TYPE (gnu_param);
	    else
	      {
		gnu_param_type = gnu_param;
		gnu_param = NULL_TREE;
	      }

	    /* The failure of this assertion will very likely come from an
	       order of elaboration issue for the type of the parameter.  */
	    gcc_assert (kind == E_Subprogram_Type
			|| !TYPE_IS_DUMMY_P (gnu_param_type)
			|| type_annotate_only);

	    if (gnu_param)
	      {
		/* If it's an exported subprogram, we build a parameter list
		   in parallel, in case we need to emit a stub for it.  */
		if (Is_Exported (gnat_entity))
		  {
		    gnu_stub_param_list
		      = chainon (gnu_param, gnu_stub_param_list);
		    /* Change By_Descriptor parameter to By_Reference for
		       the internal version of an exported subprogram.  */
		    if (mech == By_Descriptor || mech == By_Short_Descriptor)
		      {
			gnu_param
			  = gnat_to_gnu_param (gnat_param, By_Reference,
					       gnat_entity, false,
					       &copy_in_copy_out);
			has_stub = true;
		      }
		    else
		      gnu_param = copy_node (gnu_param);
		  }

		gnu_param_list = chainon (gnu_param, gnu_param_list);
		Sloc_to_locus (Sloc (gnat_param),
			       &DECL_SOURCE_LOCATION (gnu_param));
		save_gnu_tree (gnat_param, gnu_param, false);

		/* If a parameter is a pointer, this function may modify
		   memory through it and thus shouldn't be considered
		   a const function.  Also, the memory may be modified
		   between two calls, so they can't be CSE'ed.  The latter
		   case also handles by-ref parameters.  */
		if (POINTER_TYPE_P (gnu_param_type)
		    || TYPE_IS_FAT_POINTER_P (gnu_param_type))
		  const_flag = false;
	      }

	    if (copy_in_copy_out)
	      {
		if (!gnu_cico_list)
		  {
		    tree gnu_new_ret_type = make_node (RECORD_TYPE);

		    /* If this is a function, we also need a field for the
		       return value to be placed.  */
		    if (TREE_CODE (gnu_return_type) != VOID_TYPE)
		      {
			gnu_field
			  = create_field_decl (get_identifier ("RETVAL"),
					       gnu_return_type,
					       gnu_new_ret_type, NULL_TREE,
					       NULL_TREE, 0, 0);
			Sloc_to_locus (Sloc (gnat_entity),
				       &DECL_SOURCE_LOCATION (gnu_field));
			gnu_field_list = gnu_field;
			gnu_cico_list
			  = tree_cons (gnu_field, void_type_node, NULL_TREE);
		      }

		    gnu_return_type = gnu_new_ret_type;
		    TYPE_NAME (gnu_return_type) = get_identifier ("RETURN");
		    /* Set a default alignment to speed up accesses.  */
		    TYPE_ALIGN (gnu_return_type)
		      = get_mode_alignment (ptr_mode);
		  }

		gnu_field
		  = create_field_decl (gnu_param_name, gnu_param_type,
				       gnu_return_type, NULL_TREE, NULL_TREE,
				       0, 0);
		/* Set a minimum alignment to speed up accesses.  */
		if (DECL_ALIGN (gnu_field) < TYPE_ALIGN (gnu_return_type))
		  DECL_ALIGN (gnu_field) = TYPE_ALIGN (gnu_return_type);
		Sloc_to_locus (Sloc (gnat_param),
			       &DECL_SOURCE_LOCATION (gnu_field));
		DECL_CHAIN (gnu_field) = gnu_field_list;
		gnu_field_list = gnu_field;
		gnu_cico_list
		  = tree_cons (gnu_field, gnu_param, gnu_cico_list);
	      }
	  }

	if (gnu_cico_list)
	  {
	    /* If we have a CICO list but it has only one entry, we convert
	       this function into a function that returns this object.  */
	    if (list_length (gnu_cico_list) == 1)
	      gnu_return_type = TREE_TYPE (TREE_PURPOSE (gnu_cico_list));

	    /* Do not finalize the return type if the subprogram is stubbed
	       since structures are incomplete for the back-end.  */
	    else if (Convention (gnat_entity) != Convention_Stubbed)
	      {
		finish_record_type (gnu_return_type, nreverse (gnu_field_list),
				    0, false);

	        /* Try to promote the mode of the return type if it is passed
		   in registers, again to speed up accesses.  */
		if (TYPE_MODE (gnu_return_type) == BLKmode
		    && !targetm.calls.return_in_memory (gnu_return_type,
							NULL_TREE))
		  {
		    unsigned int size
		      = TREE_INT_CST_LOW (TYPE_SIZE (gnu_return_type));
		    unsigned int i = BITS_PER_UNIT;
		    enum machine_mode mode;

		    while (i < size)
		      i <<= 1;
		    mode = mode_for_size (i, MODE_INT, 0);
		    if (mode != BLKmode)
		      {
			SET_TYPE_MODE (gnu_return_type, mode);
			TYPE_ALIGN (gnu_return_type)
			  = GET_MODE_ALIGNMENT (mode);
			TYPE_SIZE (gnu_return_type)
			  = bitsize_int (GET_MODE_BITSIZE (mode));
			TYPE_SIZE_UNIT (gnu_return_type)
			  = size_int (GET_MODE_SIZE (mode));
		      }
		  }

	        if (debug_info_p)
		  rest_of_record_type_compilation (gnu_return_type);
	      }
	  }

	if (Has_Stdcall_Convention (gnat_entity))
	  prepend_one_attribute_to
	    (&attr_list, ATTR_MACHINE_ATTRIBUTE,
	     get_identifier ("stdcall"), NULL_TREE,
	     gnat_entity);

	/* If we should request stack realignment for a foreign convention
	   subprogram, do so.  Note that this applies to task entry points in
	   particular.  */
	if (FOREIGN_FORCE_REALIGN_STACK
	    && Has_Foreign_Convention (gnat_entity))
	  prepend_one_attribute_to
	    (&attr_list, ATTR_MACHINE_ATTRIBUTE,
	     get_identifier ("force_align_arg_pointer"), NULL_TREE,
	     gnat_entity);

	/* The lists have been built in reverse.  */
	gnu_param_list = nreverse (gnu_param_list);
	if (has_stub)
	  gnu_stub_param_list = nreverse (gnu_stub_param_list);
	gnu_cico_list = nreverse (gnu_cico_list);

	if (kind == E_Function)
	  Set_Mechanism (gnat_entity, return_unconstrained_p
				      || return_by_direct_ref_p
				      || return_by_invisi_ref_p
				      ? By_Reference : By_Copy);
	gnu_type
	  = create_subprog_type (gnu_return_type, gnu_param_list,
				 gnu_cico_list, return_unconstrained_p,
				 return_by_direct_ref_p,
				 return_by_invisi_ref_p);

	if (has_stub)
	  gnu_stub_type
	    = create_subprog_type (gnu_return_type, gnu_stub_param_list,
				   gnu_cico_list, return_unconstrained_p,
				   return_by_direct_ref_p,
				   return_by_invisi_ref_p);

	/* A subprogram (something that doesn't return anything) shouldn't
	   be considered const since there would be no reason for such a
	   subprogram.  Note that procedures with Out (or In Out) parameters
	   have already been converted into a function with a return type.  */
	if (TREE_CODE (gnu_return_type) == VOID_TYPE)
	  const_flag = false;

	gnu_type
	  = build_qualified_type (gnu_type,
				  TYPE_QUALS (gnu_type)
				  | (TYPE_QUAL_CONST * const_flag)
				  | (TYPE_QUAL_VOLATILE * volatile_flag));

	if (has_stub)
	  gnu_stub_type
	    = build_qualified_type (gnu_stub_type,
				    TYPE_QUALS (gnu_stub_type)
				    | (TYPE_QUAL_CONST * const_flag)
				    | (TYPE_QUAL_VOLATILE * volatile_flag));

	/* If we have a builtin decl for that function, use it.  Check if the
	   profiles are compatible and warn if they are not.  The checker is
	   expected to post extra diagnostics in this case.  */
	if (gnu_builtin_decl)
	  {
	    intrin_binding_t inb;

	    inb.gnat_entity = gnat_entity;
	    inb.ada_fntype = gnu_type;
	    inb.btin_fntype = TREE_TYPE (gnu_builtin_decl);

	    if (!intrin_profiles_compatible_p (&inb))
	      post_error
		("?profile of& doesn''t match the builtin it binds!",
		 gnat_entity);

	    gnu_decl = gnu_builtin_decl;
	    gnu_type = TREE_TYPE (gnu_builtin_decl);
	    break;
	  }

	/* If there was no specified Interface_Name and the external and
	   internal names of the subprogram are the same, only use the
	   internal name to allow disambiguation of nested subprograms.  */
	if (No (Interface_Name (gnat_entity))
	    && gnu_ext_name == gnu_entity_name)
	  gnu_ext_name = NULL_TREE;

	/* If we are defining the subprogram and it has an Address clause
	   we must get the address expression from the saved GCC tree for the
	   subprogram if it has a Freeze_Node.  Otherwise, we elaborate
	   the address expression here since the front-end has guaranteed
	   in that case that the elaboration has no effects.  If there is
	   an Address clause and we are not defining the object, just
	   make it a constant.  */
	if (Present (Address_Clause (gnat_entity)))
	  {
	    tree gnu_address = NULL_TREE;

	    if (definition)
	      gnu_address
		= (present_gnu_tree (gnat_entity)
		   ? get_gnu_tree (gnat_entity)
		   : gnat_to_gnu (Expression (Address_Clause (gnat_entity))));

	    save_gnu_tree (gnat_entity, NULL_TREE, false);

	    /* Convert the type of the object to a reference type that can
	       alias everything as per 13.3(19).  */
	    gnu_type
	      = build_reference_type_for_mode (gnu_type, ptr_mode, true);
	    if (gnu_address)
	      gnu_address = convert (gnu_type, gnu_address);

	    gnu_decl
	      = create_var_decl (gnu_entity_name, gnu_ext_name, gnu_type,
				 gnu_address, false, Is_Public (gnat_entity),
				 extern_flag, false, NULL, gnat_entity);
	    DECL_BY_REF_P (gnu_decl) = 1;
	  }

	else if (kind == E_Subprogram_Type)
	  gnu_decl
	    = create_type_decl (gnu_entity_name, gnu_type, attr_list,
				artificial_flag, debug_info_p, gnat_entity);
	else
	  {
	    if (has_stub)
	      {
		gnu_stub_name = gnu_ext_name;
		gnu_ext_name = create_concat_name (gnat_entity, "internal");
		public_flag = false;
 		artificial_flag = true;
	      }

	    gnu_decl
	      = create_subprog_decl (gnu_entity_name, gnu_ext_name, gnu_type,
				     gnu_param_list, inline_flag, public_flag,
				     extern_flag, artificial_flag, attr_list,
				     gnat_entity);
	    if (has_stub)
	      {
		tree gnu_stub_decl
		  = create_subprog_decl (gnu_entity_name, gnu_stub_name,
					 gnu_stub_type, gnu_stub_param_list,
					 inline_flag, true, extern_flag,
					 false, attr_list, gnat_entity);
		SET_DECL_FUNCTION_STUB (gnu_decl, gnu_stub_decl);
	      }

	    /* This is unrelated to the stub built right above.  */
	    DECL_STUBBED_P (gnu_decl)
	      = Convention (gnat_entity) == Convention_Stubbed;
	  }
      }
      break;

    case E_Incomplete_Type:
    case E_Incomplete_Subtype:
    case E_Private_Type:
    case E_Private_Subtype:
    case E_Limited_Private_Type:
    case E_Limited_Private_Subtype:
    case E_Record_Type_With_Private:
    case E_Record_Subtype_With_Private:
      {
	/* Get the "full view" of this entity.  If this is an incomplete
	   entity from a limited with, treat its non-limited view as the
	   full view.  Otherwise, use either the full view or the underlying
	   full view, whichever is present.  This is used in all the tests
	   below.  */
	Entity_Id full_view
	  = (IN (kind, Incomplete_Kind) && From_With_Type (gnat_entity))
	    ? Non_Limited_View (gnat_entity)
	    : Present (Full_View (gnat_entity))
	      ? Full_View (gnat_entity)
	      : Underlying_Full_View (gnat_entity);

	/* If this is an incomplete type with no full view, it must be a Taft
	   Amendment type, in which case we return a dummy type.  Otherwise,
	   just get the type from its Etype.  */
	if (No (full_view))
	  {
	    if (kind == E_Incomplete_Type)
	      {
		gnu_type = make_dummy_type (gnat_entity);
		gnu_decl = TYPE_STUB_DECL (gnu_type);
	      }
	    else
	      {
		gnu_decl = gnat_to_gnu_entity (Etype (gnat_entity),
					       NULL_TREE, 0);
		maybe_present = true;
	      }
	    break;
	  }

	/* If we already made a type for the full view, reuse it.  */
	else if (present_gnu_tree (full_view))
	  {
	    gnu_decl = get_gnu_tree (full_view);
	    break;
	  }

	/* Otherwise, if we are not defining the type now, get the type
	   from the full view.  But always get the type from the full view
	   for define on use types, since otherwise we won't see them!  */
	else if (!definition
		 || (Is_Itype (full_view)
		   && No (Freeze_Node (gnat_entity)))
		 || (Is_Itype (gnat_entity)
		   && No (Freeze_Node (full_view))))
	  {
	    gnu_decl = gnat_to_gnu_entity (full_view, NULL_TREE, 0);
	    maybe_present = true;
	    break;
	  }

	/* For incomplete types, make a dummy type entry which will be
	   replaced later.  Save it as the full declaration's type so
	   we can do any needed updates when we see it.  */
	gnu_type = make_dummy_type (gnat_entity);
	gnu_decl = TYPE_STUB_DECL (gnu_type);
	if (Has_Completion_In_Body (gnat_entity))
	  DECL_TAFT_TYPE_P (gnu_decl) = 1;
	save_gnu_tree (full_view, gnu_decl, 0);
	break;
      }

    case E_Class_Wide_Type:
      /* Class-wide types are always transformed into their root type.  */
      gnu_decl = gnat_to_gnu_entity (gnat_equiv_type, NULL_TREE, 0);
      maybe_present = true;
      break;

    case E_Task_Type:
    case E_Task_Subtype:
    case E_Protected_Type:
    case E_Protected_Subtype:
      /* Concurrent types are always transformed into their record type.  */
      if (type_annotate_only && No (gnat_equiv_type))
	gnu_type = void_type_node;
      else
	gnu_decl = gnat_to_gnu_entity (gnat_equiv_type, NULL_TREE, 0);
      maybe_present = true;
      break;

    case E_Label:
      gnu_decl = create_label_decl (gnu_entity_name);
      break;

    case E_Block:
    case E_Loop:
      /* Nothing at all to do here, so just return an ERROR_MARK and claim
	 we've already saved it, so we don't try to.  */
      gnu_decl = error_mark_node;
      saved = true;
      break;

    default:
      gcc_unreachable ();
    }

  /* If we had a case where we evaluated another type and it might have
     defined this one, handle it here.  */
  if (maybe_present && present_gnu_tree (gnat_entity))
    {
      gnu_decl = get_gnu_tree (gnat_entity);
      saved = true;
    }

  /* If we are processing a type and there is either no decl for it or
     we just made one, do some common processing for the type, such as
     handling alignment and possible padding.  */
  if (is_type && (!gnu_decl || this_made_decl))
    {
      /* Tell the middle-end that objects of tagged types are guaranteed to
	 be properly aligned.  This is necessary because conversions to the
	 class-wide type are translated into conversions to the root type,
	 which can be less aligned than some of its derived types.  */
      if (Is_Tagged_Type (gnat_entity)
	  || Is_Class_Wide_Equivalent_Type (gnat_entity))
	TYPE_ALIGN_OK (gnu_type) = 1;

      /* If the type is passed by reference, objects of this type must be
	 fully addressable and cannot be copied.  */
      if (Is_By_Reference_Type (gnat_entity))
	TREE_ADDRESSABLE (gnu_type) = 1;

      /* ??? Don't set the size for a String_Literal since it is either
	 confirming or we don't handle it properly (if the low bound is
	 non-constant).  */
      if (!gnu_size && kind != E_String_Literal_Subtype)
	{
	  Uint gnat_size = Known_Esize (gnat_entity)
			   ? Esize (gnat_entity) : RM_Size (gnat_entity);
	  gnu_size
	    = validate_size (gnat_size, gnu_type, gnat_entity, TYPE_DECL,
			     false, Has_Size_Clause (gnat_entity));
	}

      /* If a size was specified, see if we can make a new type of that size
	 by rearranging the type, for example from a fat to a thin pointer.  */
      if (gnu_size)
	{
	  gnu_type
	    = make_type_from_size (gnu_type, gnu_size,
				   Has_Biased_Representation (gnat_entity));

	  if (operand_equal_p (TYPE_SIZE (gnu_type), gnu_size, 0)
	      && operand_equal_p (rm_size (gnu_type), gnu_size, 0))
	    gnu_size = 0;
	}

      /* If the alignment hasn't already been processed and this is
	 not an unconstrained array, see if an alignment is specified.
	 If not, we pick a default alignment for atomic objects.  */
      if (align != 0 || TREE_CODE (gnu_type) == UNCONSTRAINED_ARRAY_TYPE)
	;
      else if (Known_Alignment (gnat_entity))
	{
	  align = validate_alignment (Alignment (gnat_entity), gnat_entity,
				      TYPE_ALIGN (gnu_type));

	  /* Warn on suspiciously large alignments.  This should catch
	     errors about the (alignment,byte)/(size,bit) discrepancy.  */
	  if (align > BIGGEST_ALIGNMENT && Has_Alignment_Clause (gnat_entity))
	    {
	      tree size;

	      /* If a size was specified, take it into account.  Otherwise
		 use the RM size for records or unions as the type size has
		 already been adjusted to the alignment.  */
	      if (gnu_size)
		size = gnu_size;
	      else if (RECORD_OR_UNION_TYPE_P (gnu_type)
		       && !TYPE_FAT_POINTER_P (gnu_type))
		size = rm_size (gnu_type);
	      else
	        size = TYPE_SIZE (gnu_type);

	      /* Consider an alignment as suspicious if the alignment/size
		 ratio is greater or equal to the byte/bit ratio.  */
	      if (host_integerp (size, 1)
		  && align >= TREE_INT_CST_LOW (size) * BITS_PER_UNIT)
		post_error_ne ("?suspiciously large alignment specified for&",
			       Expression (Alignment_Clause (gnat_entity)),
			       gnat_entity);
	    }
	}
      else if (Is_Atomic (gnat_entity) && !gnu_size
	       && host_integerp (TYPE_SIZE (gnu_type), 1)
	       && integer_pow2p (TYPE_SIZE (gnu_type)))
	align = MIN (BIGGEST_ALIGNMENT,
		     tree_low_cst (TYPE_SIZE (gnu_type), 1));
      else if (Is_Atomic (gnat_entity) && gnu_size
	       && host_integerp (gnu_size, 1)
	       && integer_pow2p (gnu_size))
	align = MIN (BIGGEST_ALIGNMENT, tree_low_cst (gnu_size, 1));

      /* See if we need to pad the type.  If we did, and made a record,
	 the name of the new type may be changed.  So get it back for
	 us when we make the new TYPE_DECL below.  */
      if (gnu_size || align > 0)
	gnu_type = maybe_pad_type (gnu_type, gnu_size, align, gnat_entity,
				   false, !gnu_decl, definition, false);

      if (TYPE_IS_PADDING_P (gnu_type))
	{
	  gnu_entity_name = TYPE_NAME (gnu_type);
	  if (TREE_CODE (gnu_entity_name) == TYPE_DECL)
	    gnu_entity_name = DECL_NAME (gnu_entity_name);
	}

      set_rm_size (RM_Size (gnat_entity), gnu_type, gnat_entity);

      /* If we are at global level, GCC will have applied variable_size to
	 the type, but that won't have done anything.  So, if it's not
	 a constant or self-referential, call elaborate_expression_1 to
	 make a variable for the size rather than calculating it each time.
	 Handle both the RM size and the actual size.  */
      if (global_bindings_p ()
	  && TYPE_SIZE (gnu_type)
	  && !TREE_CONSTANT (TYPE_SIZE (gnu_type))
	  && !CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_type)))
	{
	  tree size = TYPE_SIZE (gnu_type);

	  TYPE_SIZE (gnu_type)
	    = elaborate_expression_1 (size, gnat_entity,
				      get_identifier ("SIZE"),
				      definition, false);

	  /* ??? For now, store the size as a multiple of the alignment in
	     bytes so that we can see the alignment from the tree.  */
	  TYPE_SIZE_UNIT (gnu_type)
	    = elaborate_expression_2 (TYPE_SIZE_UNIT (gnu_type), gnat_entity,
				      get_identifier ("SIZE_A_UNIT"),
				      definition, false,
				      TYPE_ALIGN (gnu_type));

	  /* ??? gnu_type may come from an existing type so the MULT_EXPR node
	     may not be marked by the call to create_type_decl below.  */
	  MARK_VISITED (TYPE_SIZE_UNIT (gnu_type));

	  if (TREE_CODE (gnu_type) == RECORD_TYPE)
	    {
	      tree variant_part = get_variant_part (gnu_type);
	      tree ada_size = TYPE_ADA_SIZE (gnu_type);

	      if (variant_part)
		{
		  tree union_type = TREE_TYPE (variant_part);
		  tree offset = DECL_FIELD_OFFSET (variant_part);

		  /* If the position of the variant part is constant, subtract
		     it from the size of the type of the parent to get the new
		     size.  This manual CSE reduces the data size.  */
		  if (TREE_CODE (offset) == INTEGER_CST)
		    {
		      tree bitpos = DECL_FIELD_BIT_OFFSET (variant_part);
		      TYPE_SIZE (union_type)
			= size_binop (MINUS_EXPR, TYPE_SIZE (gnu_type),
				      bit_from_pos (offset, bitpos));
		      TYPE_SIZE_UNIT (union_type)
			= size_binop (MINUS_EXPR, TYPE_SIZE_UNIT (gnu_type),
				      byte_from_pos (offset, bitpos));
		    }
		  else
		    {
		      TYPE_SIZE (union_type)
			= elaborate_expression_1 (TYPE_SIZE (union_type),
						  gnat_entity,
						  get_identifier ("VSIZE"),
						  definition, false);

		      /* ??? For now, store the size as a multiple of the
			 alignment in bytes so that we can see the alignment
			 from the tree.  */
		      TYPE_SIZE_UNIT (union_type)
			= elaborate_expression_2 (TYPE_SIZE_UNIT (union_type),
						  gnat_entity,
						  get_identifier
						  ("VSIZE_A_UNIT"),
						  definition, false,
						  TYPE_ALIGN (union_type));

		      /* ??? For now, store the offset as a multiple of the
			 alignment in bytes so that we can see the alignment
			 from the tree.  */
		      DECL_FIELD_OFFSET (variant_part)
			= elaborate_expression_2 (offset,
						  gnat_entity,
						  get_identifier ("VOFFSET"),
						  definition, false,
						  DECL_OFFSET_ALIGN
						  (variant_part));
		    }

		  DECL_SIZE (variant_part) = TYPE_SIZE (union_type);
		  DECL_SIZE_UNIT (variant_part) = TYPE_SIZE_UNIT (union_type);
		}

	      if (operand_equal_p (ada_size, size, 0))
		ada_size = TYPE_SIZE (gnu_type);
	      else
		ada_size
		  = elaborate_expression_1 (ada_size, gnat_entity,
					    get_identifier ("RM_SIZE"),
					    definition, false);
	      SET_TYPE_ADA_SIZE (gnu_type, ada_size);
	    }
	}

      /* If this is a record type or subtype, call elaborate_expression_1 on
	 any field position.  Do this for both global and local types.
	 Skip any fields that we haven't made trees for to avoid problems with
	 class wide types.  */
      if (IN (kind, Record_Kind))
	for (gnat_temp = First_Entity (gnat_entity); Present (gnat_temp);
	     gnat_temp = Next_Entity (gnat_temp))
	  if (Ekind (gnat_temp) == E_Component && present_gnu_tree (gnat_temp))
	    {
	      tree gnu_field = get_gnu_tree (gnat_temp);

	      /* ??? For now, store the offset as a multiple of the alignment
		 in bytes so that we can see the alignment from the tree.  */
	      if (!CONTAINS_PLACEHOLDER_P (DECL_FIELD_OFFSET (gnu_field)))
		{
		  DECL_FIELD_OFFSET (gnu_field)
		    = elaborate_expression_2 (DECL_FIELD_OFFSET (gnu_field),
					      gnat_temp,
					      get_identifier ("OFFSET"),
					      definition, false,
					      DECL_OFFSET_ALIGN (gnu_field));

		  /* ??? The context of gnu_field is not necessarily gnu_type
		     so the MULT_EXPR node built above may not be marked by
		     the call to create_type_decl below.  */
		  if (global_bindings_p ())
		    MARK_VISITED (DECL_FIELD_OFFSET (gnu_field));
		}
	    }

      if (Treat_As_Volatile (gnat_entity))
	gnu_type
	  = build_qualified_type (gnu_type,
				  TYPE_QUALS (gnu_type) | TYPE_QUAL_VOLATILE);

      if (Is_Atomic (gnat_entity))
	check_ok_for_atomic (gnu_type, gnat_entity, false);

      if (Present (Alignment_Clause (gnat_entity)))
	TYPE_USER_ALIGN (gnu_type) = 1;

      if (Universal_Aliasing (gnat_entity))
	TYPE_UNIVERSAL_ALIASING_P (TYPE_MAIN_VARIANT (gnu_type)) = 1;

      if (!gnu_decl)
	gnu_decl = create_type_decl (gnu_entity_name, gnu_type, attr_list,
				     !Comes_From_Source (gnat_entity),
				     debug_info_p, gnat_entity);
      else
	{
	  TREE_TYPE (gnu_decl) = gnu_type;
	  TYPE_STUB_DECL (gnu_type) = gnu_decl;
	}
    }

  if (is_type && !TYPE_IS_DUMMY_P (TREE_TYPE (gnu_decl)))
    {
      gnu_type = TREE_TYPE (gnu_decl);

      /* If this is a derived type, relate its alias set to that of its parent
	 to avoid troubles when a call to an inherited primitive is inlined in
	 a context where a derived object is accessed.  The inlined code works
	 on the parent view so the resulting code may access the same object
	 using both the parent and the derived alias sets, which thus have to
	 conflict.  As the same issue arises with component references, the
	 parent alias set also has to conflict with composite types enclosing
	 derived components.  For instance, if we have:

	    type D is new T;
	    type R is record
	       Component : D;
	    end record;

	 we want T to conflict with both D and R, in addition to R being a
	 superset of D by record/component construction.

	 One way to achieve this is to perform an alias set copy from the
	 parent to the derived type.  This is not quite appropriate, though,
	 as we don't want separate derived types to conflict with each other:

	    type I1 is new Integer;
	    type I2 is new Integer;

	 We want I1 and I2 to both conflict with Integer but we do not want
	 I1 to conflict with I2, and an alias set copy on derivation would
	 have that effect.

	 The option chosen is to make the alias set of the derived type a
	 superset of that of its parent type.  It trivially fulfills the
	 simple requirement for the Integer derivation example above, and
	 the component case as well by superset transitivity:

		   superset      superset
		R ----------> D ----------> T

	 However, for composite types, conversions between derived types are
	 translated into VIEW_CONVERT_EXPRs so a sequence like:

	    type Comp1 is new Comp;
	    type Comp2 is new Comp;
	    procedure Proc (C : Comp1);

	    C : Comp2;
	    Proc (Comp1 (C));

	 is translated into:

	    C : Comp2;
	    Proc ((Comp1 &) &VIEW_CONVERT_EXPR <Comp1> (C));

	 and gimplified into:

	    C : Comp2;
	    Comp1 *C.0;
	    C.0 = (Comp1 *) &C;
	    Proc (C.0);

	 i.e. generates code involving type punning.  Therefore, Comp1 needs
	 to conflict with Comp2 and an alias set copy is required.

	 The language rules ensure the parent type is already frozen here.  */
      if (Is_Derived_Type (gnat_entity))
	{
	  tree gnu_parent_type = gnat_to_gnu_type (Etype (gnat_entity));
	  relate_alias_sets (gnu_type, gnu_parent_type,
			     Is_Composite_Type (gnat_entity)
			     ? ALIAS_SET_COPY : ALIAS_SET_SUPERSET);
	}

      /* Back-annotate the Alignment of the type if not already in the
	 tree.  Likewise for sizes.  */
      if (Unknown_Alignment (gnat_entity))
	{
	  unsigned int double_align, align;
	  bool is_capped_double, align_clause;

	  /* If the default alignment of "double" or larger scalar types is
	     specifically capped and this is not an array with an alignment
	     clause on the component type, return the cap.  */
	  if ((double_align = double_float_alignment) > 0)
	    is_capped_double
	      = is_double_float_or_array (gnat_entity, &align_clause);
	  else if ((double_align = double_scalar_alignment) > 0)
	    is_capped_double
	      = is_double_scalar_or_array (gnat_entity, &align_clause);
	  else
	    is_capped_double = align_clause = false;

	  if (is_capped_double && !align_clause)
	    align = double_align;
	  else
	    align = TYPE_ALIGN (gnu_type) / BITS_PER_UNIT;

	  Set_Alignment (gnat_entity, UI_From_Int (align));
	}

      if (Unknown_Esize (gnat_entity) && TYPE_SIZE (gnu_type))
	{
	  tree gnu_size = TYPE_SIZE (gnu_type);

	  /* If the size is self-referential, annotate the maximum value.  */
	  if (CONTAINS_PLACEHOLDER_P (gnu_size))
	    gnu_size = max_size (gnu_size, true);

	  if (type_annotate_only && Is_Tagged_Type (gnat_entity))
	    {
	      /* In this mode, the tag and the parent components are not
		 generated by the front-end so the sizes must be adjusted.  */
	      tree pointer_size = bitsize_int (POINTER_SIZE), offset;
	      Uint uint_size;

	      if (Is_Derived_Type (gnat_entity))
		{
		  offset = UI_To_gnu (Esize (Etype (Base_Type (gnat_entity))),
				      bitsizetype);
		  Set_Alignment (gnat_entity,
				 Alignment (Etype (Base_Type (gnat_entity))));
		}
	      else
		offset = pointer_size;

	      gnu_size = size_binop (PLUS_EXPR, gnu_size, offset);
	      gnu_size = size_binop (MULT_EXPR, pointer_size,
						size_binop (CEIL_DIV_EXPR,
							    gnu_size,
							    pointer_size));
	      uint_size = annotate_value (gnu_size);
	      Set_Esize (gnat_entity, uint_size);
	      Set_RM_Size (gnat_entity, uint_size);
	    }
	  else
	    Set_Esize (gnat_entity, annotate_value (gnu_size));
	}

      if (Unknown_RM_Size (gnat_entity) && rm_size (gnu_type))
	Set_RM_Size (gnat_entity, annotate_value (rm_size (gnu_type)));
    }

  /* If we really have a ..._DECL node, set a couple of flags on it.  But we
     cannot do so if we are reusing the ..._DECL node made for an alias or a
     renamed object as the predicates don't apply to it but to GNAT_ENTITY.  */
  if (DECL_P (gnu_decl)
      && !Present (Alias (gnat_entity))
      && !(Present (Renamed_Object (gnat_entity)) && saved))
    {
      if (!Comes_From_Source (gnat_entity))
	DECL_ARTIFICIAL (gnu_decl) = 1;

      if (!debug_info_p)
	DECL_IGNORED_P (gnu_decl) = 1;
    }

  /* If we haven't already, associate the ..._DECL node that we just made with
     the input GNAT entity node.  */
  if (!saved)
    save_gnu_tree (gnat_entity, gnu_decl, false);

  /* If this is an enumeration or floating-point type, we were not able to set
     the bounds since they refer to the type.  These are always static.  */
  if ((kind == E_Enumeration_Type && Present (First_Literal (gnat_entity)))
      || (kind == E_Floating_Point_Type && !Vax_Float (gnat_entity)))
    {
      tree gnu_scalar_type = gnu_type;
      tree gnu_low_bound, gnu_high_bound;

      /* If this is a padded type, we need to use the underlying type.  */
      if (TYPE_IS_PADDING_P (gnu_scalar_type))
	gnu_scalar_type = TREE_TYPE (TYPE_FIELDS (gnu_scalar_type));

      /* If this is a floating point type and we haven't set a floating
	 point type yet, use this in the evaluation of the bounds.  */
      if (!longest_float_type_node && kind == E_Floating_Point_Type)
	longest_float_type_node = gnu_scalar_type;

      gnu_low_bound = gnat_to_gnu (Type_Low_Bound (gnat_entity));
      gnu_high_bound = gnat_to_gnu (Type_High_Bound (gnat_entity));

      if (kind == E_Enumeration_Type)
	{
	  /* Enumeration types have specific RM bounds.  */
	  SET_TYPE_RM_MIN_VALUE (gnu_scalar_type, gnu_low_bound);
	  SET_TYPE_RM_MAX_VALUE (gnu_scalar_type, gnu_high_bound);

	  /* Write full debugging information.  */
	  rest_of_type_decl_compilation (gnu_decl);
	}

      else
	{
	  /* Floating-point types don't have specific RM bounds.  */
	  TYPE_GCC_MIN_VALUE (gnu_scalar_type) = gnu_low_bound;
	  TYPE_GCC_MAX_VALUE (gnu_scalar_type) = gnu_high_bound;
	}
    }

  /* If we deferred processing of incomplete types, re-enable it.  If there
     were no other disables and we have deferred types to process, do so.  */
  if (this_deferred
      && --defer_incomplete_level == 0
      && defer_incomplete_list)
    {
      struct incomplete *p, *next;

      /* We are back to level 0 for the deferring of incomplete types.
	 But processing these incomplete types below may itself require
	 deferring, so preserve what we have and restart from scratch.  */
      p = defer_incomplete_list;
      defer_incomplete_list = NULL;

      /* For finalization, however, all types must be complete so we
	 cannot do the same because deferred incomplete types may end up
	 referencing each other.  Process them all recursively first.  */
      defer_finalize_level++;

      for (; p; p = next)
	{
	  next = p->next;

	  if (p->old_type)
	    update_pointer_to (TYPE_MAIN_VARIANT (p->old_type),
			       gnat_to_gnu_type (p->full_type));
	  free (p);
	}

      defer_finalize_level--;
    }

  /* If all the deferred incomplete types have been processed, we can proceed
     with the finalization of the deferred types.  */
  if (defer_incomplete_level == 0
      && defer_finalize_level == 0
      && defer_finalize_list)
    {
      unsigned int i;
      tree t;

      FOR_EACH_VEC_ELT (tree, defer_finalize_list, i, t)
	rest_of_type_decl_compilation_no_defer (t);

      VEC_free (tree, heap, defer_finalize_list);
    }

  /* If we are not defining this type, see if it's on one of the lists of
     incomplete types.  If so, handle the list entry now.  */
  if (is_type && !definition)
    {
      struct incomplete *p;

      for (p = defer_incomplete_list; p; p = p->next)
	if (p->old_type && p->full_type == gnat_entity)
	  {
	    update_pointer_to (TYPE_MAIN_VARIANT (p->old_type),
			       TREE_TYPE (gnu_decl));
	    p->old_type = NULL_TREE;
	  }

      for (p = defer_limited_with; p; p = p->next)
	if (p->old_type && Non_Limited_View (p->full_type) == gnat_entity)
	  {
	    update_pointer_to (TYPE_MAIN_VARIANT (p->old_type),
			       TREE_TYPE (gnu_decl));
	    p->old_type = NULL_TREE;
	  }
    }

  if (this_global)
    force_global--;

  /* If this is a packed array type whose original array type is itself
     an Itype without freeze node, make sure the latter is processed.  */
  if (Is_Packed_Array_Type (gnat_entity)
      && Is_Itype (Original_Array_Type (gnat_entity))
      && No (Freeze_Node (Original_Array_Type (gnat_entity)))
      && !present_gnu_tree (Original_Array_Type (gnat_entity)))
    gnat_to_gnu_entity (Original_Array_Type (gnat_entity), NULL_TREE, 0);

  return gnu_decl;
}

/* Similar, but if the returned value is a COMPONENT_REF, return the
   FIELD_DECL.  */

tree
gnat_to_gnu_field_decl (Entity_Id gnat_entity)
{
  tree gnu_field = gnat_to_gnu_entity (gnat_entity, NULL_TREE, 0);

  if (TREE_CODE (gnu_field) == COMPONENT_REF)
    gnu_field = TREE_OPERAND (gnu_field, 1);

  return gnu_field;
}

/* Similar, but GNAT_ENTITY is assumed to refer to a GNAT type.  Return
   the GCC type corresponding to that entity.  */

tree
gnat_to_gnu_type (Entity_Id gnat_entity)
{
  tree gnu_decl;

  /* The back end never attempts to annotate generic types.  */
  if (Is_Generic_Type (gnat_entity) && type_annotate_only)
     return void_type_node;

  gnu_decl = gnat_to_gnu_entity (gnat_entity, NULL_TREE, 0);
  gcc_assert (TREE_CODE (gnu_decl) == TYPE_DECL);

  return TREE_TYPE (gnu_decl);
}

/* Similar, but GNAT_ENTITY is assumed to refer to a GNAT type.  Return
   the unpadded version of the GCC type corresponding to that entity.  */

tree
get_unpadded_type (Entity_Id gnat_entity)
{
  tree type = gnat_to_gnu_type (gnat_entity);

  if (TYPE_IS_PADDING_P (type))
    type = TREE_TYPE (TYPE_FIELDS (type));

  return type;
}

/* Wrap up compilation of DECL, a TYPE_DECL, possibly deferring it.
   Every TYPE_DECL generated for a type definition must be passed
   to this function once everything else has been done for it.  */

void
rest_of_type_decl_compilation (tree decl)
{
  /* We need to defer finalizing the type if incomplete types
     are being deferred or if they are being processed.  */
  if (defer_incomplete_level != 0 || defer_finalize_level != 0)
    VEC_safe_push (tree, heap, defer_finalize_list, decl);
  else
    rest_of_type_decl_compilation_no_defer (decl);
}

/* Same as above but without deferring the compilation.  This
   function should not be invoked directly on a TYPE_DECL.  */

static void
rest_of_type_decl_compilation_no_defer (tree decl)
{
  const int toplev = global_bindings_p ();
  tree t = TREE_TYPE (decl);

  rest_of_decl_compilation (decl, toplev, 0);

  /* Now process all the variants.  This is needed for STABS.  */
  for (t = TYPE_MAIN_VARIANT (t); t; t = TYPE_NEXT_VARIANT (t))
    {
      if (t == TREE_TYPE (decl))
	continue;

      if (!TYPE_STUB_DECL (t))
	TYPE_STUB_DECL (t) = create_type_stub_decl (DECL_NAME (decl), t);

      rest_of_type_compilation (t, toplev);
    }
}

/* Finalize the processing of From_With_Type incomplete types.  */

void
finalize_from_with_types (void)
{
  struct incomplete *p, *next;

  p = defer_limited_with;
  defer_limited_with = NULL;

  for (; p; p = next)
    {
      next = p->next;

      if (p->old_type)
	update_pointer_to (TYPE_MAIN_VARIANT (p->old_type),
			   gnat_to_gnu_type (p->full_type));
      free (p);
    }
}

/* Return the equivalent type to be used for GNAT_ENTITY, if it's a
   kind of type (such E_Task_Type) that has a different type which Gigi
   uses for its representation.  If the type does not have a special type
   for its representation, return GNAT_ENTITY.  If a type is supposed to
   exist, but does not, abort unless annotating types, in which case
   return Empty.  If GNAT_ENTITY is Empty, return Empty.  */

Entity_Id
Gigi_Equivalent_Type (Entity_Id gnat_entity)
{
  Entity_Id gnat_equiv = gnat_entity;

  if (No (gnat_entity))
    return gnat_entity;

  switch (Ekind (gnat_entity))
    {
    case E_Class_Wide_Subtype:
      if (Present (Equivalent_Type (gnat_entity)))
	gnat_equiv = Equivalent_Type (gnat_entity);
      break;

    case E_Access_Protected_Subprogram_Type:
    case E_Anonymous_Access_Protected_Subprogram_Type:
      gnat_equiv = Equivalent_Type (gnat_entity);
      break;

    case E_Class_Wide_Type:
      gnat_equiv = Root_Type (gnat_entity);
      break;

    case E_Task_Type:
    case E_Task_Subtype:
    case E_Protected_Type:
    case E_Protected_Subtype:
      gnat_equiv = Corresponding_Record_Type (gnat_entity);
      break;

    default:
      break;
    }

  gcc_assert (Present (gnat_equiv) || type_annotate_only);
  return gnat_equiv;
}

/* Return a GCC tree for a type corresponding to the component type of the
   array type or subtype GNAT_ARRAY.  DEFINITION is true if this component
   is for an array being defined.  DEBUG_INFO_P is true if we need to write
   debug information for other types that we may create in the process.  */

static tree
gnat_to_gnu_component_type (Entity_Id gnat_array, bool definition,
			    bool debug_info_p)
{
  const Entity_Id gnat_type = Component_Type (gnat_array);
  tree gnu_type = gnat_to_gnu_type (gnat_type);
  tree gnu_comp_size;

  /* Try to get a smaller form of the component if needed.  */
  if ((Is_Packed (gnat_array)
       || Has_Component_Size_Clause (gnat_array))
      && !Is_Bit_Packed_Array (gnat_array)
      && !Has_Aliased_Components (gnat_array)
      && !Strict_Alignment (gnat_type)
      && RECORD_OR_UNION_TYPE_P (gnu_type)
      && !TYPE_FAT_POINTER_P (gnu_type)
      && host_integerp (TYPE_SIZE (gnu_type), 1))
    gnu_type = make_packable_type (gnu_type, false);

  if (Has_Atomic_Components (gnat_array))
    check_ok_for_atomic (gnu_type, gnat_array, true);

  /* Get and validate any specified Component_Size.  */
  gnu_comp_size
    = validate_size (Component_Size (gnat_array), gnu_type, gnat_array,
		     Is_Bit_Packed_Array (gnat_array) ? TYPE_DECL : VAR_DECL,
		     true, Has_Component_Size_Clause (gnat_array));

  /* If the array has aliased components and the component size can be zero,
     force at least unit size to ensure that the components have distinct
     addresses.  */
  if (!gnu_comp_size
      && Has_Aliased_Components (gnat_array)
      && (integer_zerop (TYPE_SIZE (gnu_type))
	  || (TREE_CODE (gnu_type) == ARRAY_TYPE
	      && !TREE_CONSTANT (TYPE_SIZE (gnu_type)))))
    gnu_comp_size
      = size_binop (MAX_EXPR, TYPE_SIZE (gnu_type), bitsize_unit_node);

  /* If the component type is a RECORD_TYPE that has a self-referential size,
     then use the maximum size for the component size.  */
  if (!gnu_comp_size
      && TREE_CODE (gnu_type) == RECORD_TYPE
      && CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_type)))
    gnu_comp_size = max_size (TYPE_SIZE (gnu_type), true);

  /* Honor the component size.  This is not needed for bit-packed arrays.  */
  if (gnu_comp_size && !Is_Bit_Packed_Array (gnat_array))
    {
      tree orig_type = gnu_type;
      unsigned int max_align;

      /* If an alignment is specified, use it as a cap on the component type
	 so that it can be honored for the whole type.  But ignore it for the
	 original type of packed array types.  */
      if (No (Packed_Array_Type (gnat_array)) && Known_Alignment (gnat_array))
	max_align = validate_alignment (Alignment (gnat_array), gnat_array, 0);
      else
	max_align = 0;

      gnu_type = make_type_from_size (gnu_type, gnu_comp_size, false);
      if (max_align > 0 && TYPE_ALIGN (gnu_type) > max_align)
	gnu_type = orig_type;
      else
	orig_type = gnu_type;

      gnu_type = maybe_pad_type (gnu_type, gnu_comp_size, 0, gnat_array,
				 true, false, definition, true);

      /* If a padding record was made, declare it now since it will never be
	 declared otherwise.  This is necessary to ensure that its subtrees
	 are properly marked.  */
      if (gnu_type != orig_type && !DECL_P (TYPE_NAME (gnu_type)))
	create_type_decl (TYPE_NAME (gnu_type), gnu_type, NULL, true,
			  debug_info_p, gnat_array);
    }

  if (Has_Volatile_Components (gnat_array))
    gnu_type
      = build_qualified_type (gnu_type,
			      TYPE_QUALS (gnu_type) | TYPE_QUAL_VOLATILE);

  return gnu_type;
}

/* Return a GCC tree for a parameter corresponding to GNAT_PARAM and
   using MECH as its passing mechanism, to be placed in the parameter
   list built for GNAT_SUBPROG.  Assume a foreign convention for the
   latter if FOREIGN is true.  Also set CICO to true if the parameter
   must use the copy-in copy-out implementation mechanism.

   The returned tree is a PARM_DECL, except for those cases where no
   parameter needs to be actually passed to the subprogram; the type
   of this "shadow" parameter is then returned instead.  */

static tree
gnat_to_gnu_param (Entity_Id gnat_param, Mechanism_Type mech,
		   Entity_Id gnat_subprog, bool foreign, bool *cico)
{
  tree gnu_param_name = get_entity_name (gnat_param);
  tree gnu_param_type = gnat_to_gnu_type (Etype (gnat_param));
  tree gnu_param_type_alt = NULL_TREE;
  bool in_param = (Ekind (gnat_param) == E_In_Parameter);
  /* The parameter can be indirectly modified if its address is taken.  */
  bool ro_param = in_param && !Address_Taken (gnat_param);
  bool by_return = false, by_component_ptr = false;
  bool by_ref = false, by_double_ref = false;
  tree gnu_param;

  /* Copy-return is used only for the first parameter of a valued procedure.
     It's a copy mechanism for which a parameter is never allocated.  */
  if (mech == By_Copy_Return)
    {
      gcc_assert (Ekind (gnat_param) == E_Out_Parameter);
      mech = By_Copy;
      by_return = true;
    }

  /* If this is either a foreign function or if the underlying type won't
     be passed by reference, strip off possible padding type.  */
  if (TYPE_IS_PADDING_P (gnu_param_type))
    {
      tree unpadded_type = TREE_TYPE (TYPE_FIELDS (gnu_param_type));

      if (mech == By_Reference
	  || foreign
	  || (!must_pass_by_ref (unpadded_type)
	      && (mech == By_Copy || !default_pass_by_ref (unpadded_type))))
	gnu_param_type = unpadded_type;
    }

  /* If this is a read-only parameter, make a variant of the type that is
     read-only.  ??? However, if this is an unconstrained array, that type
     can be very complex, so skip it for now.  Likewise for any other
     self-referential type.  */
  if (ro_param
      && TREE_CODE (gnu_param_type) != UNCONSTRAINED_ARRAY_TYPE
      && !CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_param_type)))
    gnu_param_type = build_qualified_type (gnu_param_type,
					   (TYPE_QUALS (gnu_param_type)
					    | TYPE_QUAL_CONST));

  /* For foreign conventions, pass arrays as pointers to the element type.
     First check for unconstrained array and get the underlying array.  */
  if (foreign && TREE_CODE (gnu_param_type) == UNCONSTRAINED_ARRAY_TYPE)
    gnu_param_type
      = TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_param_type))));

  /* For GCC builtins, pass Address integer types as (void *)  */
  if (Convention (gnat_subprog) == Convention_Intrinsic
      && Present (Interface_Name (gnat_subprog))
      && Is_Descendent_Of_Address (Etype (gnat_param)))
    gnu_param_type = ptr_void_type_node;

  /* VMS descriptors are themselves passed by reference.  */
  if (mech == By_Short_Descriptor ||
      (mech == By_Descriptor && TARGET_ABI_OPEN_VMS && !TARGET_MALLOC64))
    gnu_param_type
      = build_pointer_type (build_vms_descriptor32 (gnu_param_type,
						    Mechanism (gnat_param),
						    gnat_subprog));
  else if (mech == By_Descriptor)
    {
      /* Build both a 32-bit and 64-bit descriptor, one of which will be
	 chosen in fill_vms_descriptor.  */
      gnu_param_type_alt
        = build_pointer_type (build_vms_descriptor32 (gnu_param_type,
						      Mechanism (gnat_param),
						      gnat_subprog));
      gnu_param_type
        = build_pointer_type (build_vms_descriptor (gnu_param_type,
						    Mechanism (gnat_param),
						    gnat_subprog));
    }

  /* Arrays are passed as pointers to element type for foreign conventions.  */
  else if (foreign
	   && mech != By_Copy
	   && TREE_CODE (gnu_param_type) == ARRAY_TYPE)
    {
      /* Strip off any multi-dimensional entries, then strip
	 off the last array to get the component type.  */
      while (TREE_CODE (TREE_TYPE (gnu_param_type)) == ARRAY_TYPE
	     && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_param_type)))
	gnu_param_type = TREE_TYPE (gnu_param_type);

      by_component_ptr = true;
      gnu_param_type = TREE_TYPE (gnu_param_type);

      if (ro_param)
	gnu_param_type = build_qualified_type (gnu_param_type,
					       (TYPE_QUALS (gnu_param_type)
						| TYPE_QUAL_CONST));

      gnu_param_type = build_pointer_type (gnu_param_type);
    }

  /* Fat pointers are passed as thin pointers for foreign conventions.  */
  else if (foreign && TYPE_IS_FAT_POINTER_P (gnu_param_type))
    gnu_param_type
      = make_type_from_size (gnu_param_type, size_int (POINTER_SIZE), 0);

  /* If we must pass or were requested to pass by reference, do so.
     If we were requested to pass by copy, do so.
     Otherwise, for foreign conventions, pass In Out or Out parameters
     or aggregates by reference.  For COBOL and Fortran, pass all
     integer and FP types that way too.  For Convention Ada, use
     the standard Ada default.  */
  else if (must_pass_by_ref (gnu_param_type)
	   || mech == By_Reference
	   || (mech != By_Copy
	       && ((foreign
		    && (!in_param || AGGREGATE_TYPE_P (gnu_param_type)))
		   || (foreign
		       && (Convention (gnat_subprog) == Convention_Fortran
			   || Convention (gnat_subprog) == Convention_COBOL)
		       && (INTEGRAL_TYPE_P (gnu_param_type)
			   || FLOAT_TYPE_P (gnu_param_type)))
		   || (!foreign
		       && default_pass_by_ref (gnu_param_type)))))
    {
      /* We take advantage of 6.2(12) by considering that references built for
	 parameters whose type isn't by-ref and for which the mechanism hasn't
	 been forced to by-ref are restrict-qualified in the C sense.  */
      bool restrict_p
	= !TREE_ADDRESSABLE (gnu_param_type) && mech != By_Reference;
      gnu_param_type = build_reference_type (gnu_param_type);
      if (restrict_p)
	gnu_param_type
	  = build_qualified_type (gnu_param_type, TYPE_QUAL_RESTRICT);
      by_ref = true;

      /* In some ABIs, e.g. SPARC 32-bit, fat pointer types are themselves
	 passed by reference.  Pass them by explicit reference, this will
	 generate more debuggable code at -O0.  */
      if (TYPE_IS_FAT_POINTER_P (gnu_param_type)
	  && targetm.calls.pass_by_reference (pack_cumulative_args (NULL),
					      TYPE_MODE (gnu_param_type),
					      gnu_param_type,
					      true))
	{
	   gnu_param_type = build_reference_type (gnu_param_type);
	   by_double_ref = true;
	}
    }

  /* Pass In Out or Out parameters using copy-in copy-out mechanism.  */
  else if (!in_param)
    *cico = true;

  if (mech == By_Copy && (by_ref || by_component_ptr))
    post_error ("?cannot pass & by copy", gnat_param);

  /* If this is an Out parameter that isn't passed by reference and isn't
     a pointer or aggregate, we don't make a PARM_DECL for it.  Instead,
     it will be a VAR_DECL created when we process the procedure, so just
     return its type.  For the special parameter of a valued procedure,
     never pass it in.

     An exception is made to cover the RM-6.4.1 rule requiring "by copy"
     Out parameters with discriminants or implicit initial values to be
     handled like In Out parameters.  These type are normally built as
     aggregates, hence passed by reference, except for some packed arrays
     which end up encoded in special integer types.

     The exception we need to make is then for packed arrays of records
     with discriminants or implicit initial values.  We have no light/easy
     way to check for the latter case, so we merely check for packed arrays
     of records.  This may lead to useless copy-in operations, but in very
     rare cases only, as these would be exceptions in a set of already
     exceptional situations.  */
  if (Ekind (gnat_param) == E_Out_Parameter
      && !by_ref
      && (by_return
	  || (mech != By_Descriptor
              && mech != By_Short_Descriptor
	      && !POINTER_TYPE_P (gnu_param_type)
	      && !AGGREGATE_TYPE_P (gnu_param_type)))
      && !(Is_Array_Type (Etype (gnat_param))
	   && Is_Packed (Etype (gnat_param))
	   && Is_Composite_Type (Component_Type (Etype (gnat_param)))))
    return gnu_param_type;

  gnu_param = create_param_decl (gnu_param_name, gnu_param_type,
				 ro_param || by_ref || by_component_ptr);
  DECL_BY_REF_P (gnu_param) = by_ref;
  DECL_BY_DOUBLE_REF_P (gnu_param) = by_double_ref;
  DECL_BY_COMPONENT_PTR_P (gnu_param) = by_component_ptr;
  DECL_BY_DESCRIPTOR_P (gnu_param) = (mech == By_Descriptor ||
                                      mech == By_Short_Descriptor);
  DECL_POINTS_TO_READONLY_P (gnu_param)
    = (ro_param && (by_ref || by_component_ptr));
  DECL_CAN_NEVER_BE_NULL_P (gnu_param) = Can_Never_Be_Null (gnat_param);

  /* Save the alternate descriptor type, if any.  */
  if (gnu_param_type_alt)
    SET_DECL_PARM_ALT_TYPE (gnu_param, gnu_param_type_alt);

  /* If no Mechanism was specified, indicate what we're using, then
     back-annotate it.  */
  if (mech == Default)
    mech = (by_ref || by_component_ptr) ? By_Reference : By_Copy;

  Set_Mechanism (gnat_param, mech);
  return gnu_param;
}

/* Return true if DISCR1 and DISCR2 represent the same discriminant.  */

static bool
same_discriminant_p (Entity_Id discr1, Entity_Id discr2)
{
  while (Present (Corresponding_Discriminant (discr1)))
    discr1 = Corresponding_Discriminant (discr1);

  while (Present (Corresponding_Discriminant (discr2)))
    discr2 = Corresponding_Discriminant (discr2);

  return
    Original_Record_Component (discr1) == Original_Record_Component (discr2);
}

/* Return true if the array type GNU_TYPE, which represents a dimension of
   GNAT_TYPE, has a non-aliased component in the back-end sense.  */

static bool
array_type_has_nonaliased_component (tree gnu_type, Entity_Id gnat_type)
{
  /* If the array type is not the innermost dimension of the GNAT type,
     then it has a non-aliased component.  */
  if (TREE_CODE (TREE_TYPE (gnu_type)) == ARRAY_TYPE
      && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_type)))
    return true;

  /* If the array type has an aliased component in the front-end sense,
     then it also has an aliased component in the back-end sense.  */
  if (Has_Aliased_Components (gnat_type))
    return false;

  /* If this is a derived type, then it has a non-aliased component if
     and only if its parent type also has one.  */
  if (Is_Derived_Type (gnat_type))
    {
      tree gnu_parent_type = gnat_to_gnu_type (Etype (gnat_type));
      int index;
      if (TREE_CODE (gnu_parent_type) == UNCONSTRAINED_ARRAY_TYPE)
	gnu_parent_type
	  = TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_parent_type))));
      for (index = Number_Dimensions (gnat_type) - 1; index > 0; index--)
	gnu_parent_type = TREE_TYPE (gnu_parent_type);
      return TYPE_NONALIASED_COMPONENT (gnu_parent_type);
    }

  /* Otherwise, rely exclusively on properties of the element type.  */
  return type_for_nonaliased_component_p (TREE_TYPE (gnu_type));
}

/* Return true if GNAT_ADDRESS is a value known at compile-time.  */

static bool
compile_time_known_address_p (Node_Id gnat_address)
{
  /* Catch System'To_Address.  */
  if (Nkind (gnat_address) == N_Unchecked_Type_Conversion)
    gnat_address = Expression (gnat_address);

  return Compile_Time_Known_Value (gnat_address);
}

/* Return true if GNAT_RANGE, a N_Range node, cannot be superflat, i.e. if the
   inequality HB >= LB-1 is true.  LB and HB are the low and high bounds.  */

static bool
cannot_be_superflat_p (Node_Id gnat_range)
{
  Node_Id gnat_lb = Low_Bound (gnat_range), gnat_hb = High_Bound (gnat_range);
  Node_Id scalar_range;
  tree gnu_lb, gnu_hb, gnu_lb_minus_one;

  /* If the low bound is not constant, try to find an upper bound.  */
  while (Nkind (gnat_lb) != N_Integer_Literal
	 && (Ekind (Etype (gnat_lb)) == E_Signed_Integer_Subtype
	     || Ekind (Etype (gnat_lb)) == E_Modular_Integer_Subtype)
	 && (scalar_range = Scalar_Range (Etype (gnat_lb)))
	 && (Nkind (scalar_range) == N_Signed_Integer_Type_Definition
	     || Nkind (scalar_range) == N_Range))
    gnat_lb = High_Bound (scalar_range);

  /* If the high bound is not constant, try to find a lower bound.  */
  while (Nkind (gnat_hb) != N_Integer_Literal
	 && (Ekind (Etype (gnat_hb)) == E_Signed_Integer_Subtype
	     || Ekind (Etype (gnat_hb)) == E_Modular_Integer_Subtype)
	 && (scalar_range = Scalar_Range (Etype (gnat_hb)))
	 && (Nkind (scalar_range) == N_Signed_Integer_Type_Definition
	     || Nkind (scalar_range) == N_Range))
    gnat_hb = Low_Bound (scalar_range);

  /* If we have failed to find constant bounds, punt.  */
  if (Nkind (gnat_lb) != N_Integer_Literal
      || Nkind (gnat_hb) != N_Integer_Literal)
    return false;

  /* We need at least a signed 64-bit type to catch most cases.  */
  gnu_lb = UI_To_gnu (Intval (gnat_lb), sbitsizetype);
  gnu_hb = UI_To_gnu (Intval (gnat_hb), sbitsizetype);
  if (TREE_OVERFLOW (gnu_lb) || TREE_OVERFLOW (gnu_hb))
    return false;

  /* If the low bound is the smallest integer, nothing can be smaller.  */
  gnu_lb_minus_one = size_binop (MINUS_EXPR, gnu_lb, sbitsize_one_node);
  if (TREE_OVERFLOW (gnu_lb_minus_one))
    return true;

  return !tree_int_cst_lt (gnu_hb, gnu_lb_minus_one);
}

/* Return true if GNU_EXPR is (essentially) the address of a CONSTRUCTOR.  */

static bool
constructor_address_p (tree gnu_expr)
{
  while (TREE_CODE (gnu_expr) == NOP_EXPR
	 || TREE_CODE (gnu_expr) == CONVERT_EXPR
	 || TREE_CODE (gnu_expr) == NON_LVALUE_EXPR)
    gnu_expr = TREE_OPERAND (gnu_expr, 0);

  return (TREE_CODE (gnu_expr) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (gnu_expr, 0)) == CONSTRUCTOR);
}

/* Given GNAT_ENTITY, elaborate all expressions that are required to
   be elaborated at the point of its definition, but do nothing else.  */

void
elaborate_entity (Entity_Id gnat_entity)
{
  switch (Ekind (gnat_entity))
    {
    case E_Signed_Integer_Subtype:
    case E_Modular_Integer_Subtype:
    case E_Enumeration_Subtype:
    case E_Ordinary_Fixed_Point_Subtype:
    case E_Decimal_Fixed_Point_Subtype:
    case E_Floating_Point_Subtype:
      {
	Node_Id gnat_lb = Type_Low_Bound (gnat_entity);
	Node_Id gnat_hb = Type_High_Bound (gnat_entity);

	/* ??? Tests to avoid Constraint_Error in static expressions
	   are needed until after the front stops generating bogus
	   conversions on bounds of real types.  */
	if (!Raises_Constraint_Error (gnat_lb))
	  elaborate_expression (gnat_lb, gnat_entity, get_identifier ("L"),
				true, false, Needs_Debug_Info (gnat_entity));
	if (!Raises_Constraint_Error (gnat_hb))
	  elaborate_expression (gnat_hb, gnat_entity, get_identifier ("U"),
				true, false, Needs_Debug_Info (gnat_entity));
      break;
      }

    case E_Record_Type:
      {
	Node_Id full_definition = Declaration_Node (gnat_entity);
	Node_Id record_definition = Type_Definition (full_definition);

	/* If this is a record extension, go a level further to find the
	   record definition.  */
	if (Nkind (record_definition) == N_Derived_Type_Definition)
	  record_definition = Record_Extension_Part (record_definition);
      }
      break;

    case E_Record_Subtype:
    case E_Private_Subtype:
    case E_Limited_Private_Subtype:
    case E_Record_Subtype_With_Private:
      if (Is_Constrained (gnat_entity)
	  && Has_Discriminants (gnat_entity)
	  && Present (Discriminant_Constraint (gnat_entity)))
	{
	  Node_Id gnat_discriminant_expr;
	  Entity_Id gnat_field;

	  for (gnat_field
	       = First_Discriminant (Implementation_Base_Type (gnat_entity)),
	       gnat_discriminant_expr
	       = First_Elmt (Discriminant_Constraint (gnat_entity));
	       Present (gnat_field);
	       gnat_field = Next_Discriminant (gnat_field),
	       gnat_discriminant_expr = Next_Elmt (gnat_discriminant_expr))
	    /* ??? For now, ignore access discriminants.  */
	    if (!Is_Access_Type (Etype (Node (gnat_discriminant_expr))))
	      elaborate_expression (Node (gnat_discriminant_expr),
				    gnat_entity, get_entity_name (gnat_field),
				    true, false, false);
	}
      break;

    }
}

/* Mark GNAT_ENTITY as going out of scope at this point.  Recursively mark
   any entities on its entity chain similarly.  */

void
mark_out_of_scope (Entity_Id gnat_entity)
{
  Entity_Id gnat_sub_entity;
  unsigned int kind = Ekind (gnat_entity);

  /* If this has an entity list, process all in the list.  */
  if (IN (kind, Class_Wide_Kind) || IN (kind, Concurrent_Kind)
      || IN (kind, Private_Kind)
      || kind == E_Block || kind == E_Entry || kind == E_Entry_Family
      || kind == E_Function || kind == E_Generic_Function
      || kind == E_Generic_Package || kind == E_Generic_Procedure
      || kind == E_Loop || kind == E_Operator || kind == E_Package
      || kind == E_Package_Body || kind == E_Procedure
      || kind == E_Record_Type || kind == E_Record_Subtype
      || kind == E_Subprogram_Body || kind == E_Subprogram_Type)
    for (gnat_sub_entity = First_Entity (gnat_entity);
	 Present (gnat_sub_entity);
	 gnat_sub_entity = Next_Entity (gnat_sub_entity))
      if (Scope (gnat_sub_entity) == gnat_entity
	  && gnat_sub_entity != gnat_entity)
	mark_out_of_scope (gnat_sub_entity);

  /* Now clear this if it has been defined, but only do so if it isn't
     a subprogram or parameter.  We could refine this, but it isn't
     worth it.  If this is statically allocated, it is supposed to
     hang around out of cope.  */
  if (present_gnu_tree (gnat_entity) && !Is_Statically_Allocated (gnat_entity)
      && kind != E_Procedure && kind != E_Function && !IN (kind, Formal_Kind))
    {
      save_gnu_tree (gnat_entity, NULL_TREE, true);
      save_gnu_tree (gnat_entity, error_mark_node, true);
    }
}

/* Relate the alias sets of GNU_NEW_TYPE and GNU_OLD_TYPE according to OP.
   If this is a multi-dimensional array type, do this recursively.

   OP may be
   - ALIAS_SET_COPY:     the new set is made a copy of the old one.
   - ALIAS_SET_SUPERSET: the new set is made a superset of the old one.
   - ALIAS_SET_SUBSET:   the new set is made a subset of the old one.  */

static void
relate_alias_sets (tree gnu_new_type, tree gnu_old_type, enum alias_set_op op)
{
  /* Remove any padding from GNU_OLD_TYPE.  It doesn't matter in the case
     of a one-dimensional array, since the padding has the same alias set
     as the field type, but if it's a multi-dimensional array, we need to
     see the inner types.  */
  while (TREE_CODE (gnu_old_type) == RECORD_TYPE
	 && (TYPE_JUSTIFIED_MODULAR_P (gnu_old_type)
	     || TYPE_PADDING_P (gnu_old_type)))
    gnu_old_type = TREE_TYPE (TYPE_FIELDS (gnu_old_type));

  /* Unconstrained array types are deemed incomplete and would thus be given
     alias set 0.  Retrieve the underlying array type.  */
  if (TREE_CODE (gnu_old_type) == UNCONSTRAINED_ARRAY_TYPE)
    gnu_old_type
      = TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_old_type))));
  if (TREE_CODE (gnu_new_type) == UNCONSTRAINED_ARRAY_TYPE)
    gnu_new_type
      = TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_new_type))));

  if (TREE_CODE (gnu_new_type) == ARRAY_TYPE
      && TREE_CODE (TREE_TYPE (gnu_new_type)) == ARRAY_TYPE
      && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_new_type)))
    relate_alias_sets (TREE_TYPE (gnu_new_type), TREE_TYPE (gnu_old_type), op);

  switch (op)
    {
    case ALIAS_SET_COPY:
      /* The alias set shouldn't be copied between array types with different
	 aliasing settings because this can break the aliasing relationship
	 between the array type and its element type.  */
#ifndef ENABLE_CHECKING
      if (flag_strict_aliasing)
#endif
	gcc_assert (!(TREE_CODE (gnu_new_type) == ARRAY_TYPE
		      && TREE_CODE (gnu_old_type) == ARRAY_TYPE
		      && TYPE_NONALIASED_COMPONENT (gnu_new_type)
			 != TYPE_NONALIASED_COMPONENT (gnu_old_type)));

      TYPE_ALIAS_SET (gnu_new_type) = get_alias_set (gnu_old_type);
      break;

    case ALIAS_SET_SUBSET:
    case ALIAS_SET_SUPERSET:
      {
	alias_set_type old_set = get_alias_set (gnu_old_type);
	alias_set_type new_set = get_alias_set (gnu_new_type);

	/* Do nothing if the alias sets conflict.  This ensures that we
	   never call record_alias_subset several times for the same pair
	   or at all for alias set 0.  */
	if (!alias_sets_conflict_p (old_set, new_set))
	  {
	    if (op == ALIAS_SET_SUBSET)
	      record_alias_subset (old_set, new_set);
	    else
	      record_alias_subset (new_set, old_set);
	  }
      }
      break;

    default:
      gcc_unreachable ();
    }

  record_component_aliases (gnu_new_type);
}

/* Return true if the size represented by GNU_SIZE can be handled by an
   allocation.  If STATIC_P is true, consider only what can be done with a
   static allocation.  */

static bool
allocatable_size_p (tree gnu_size, bool static_p)
{
  HOST_WIDE_INT our_size;

  /* If this is not a static allocation, the only case we want to forbid
     is an overflowing size.  That will be converted into a raise a
     Storage_Error.  */
  if (!static_p)
    return !(TREE_CODE (gnu_size) == INTEGER_CST
	     && TREE_OVERFLOW (gnu_size));

  /* Otherwise, we need to deal with both variable sizes and constant
     sizes that won't fit in a host int.  We use int instead of HOST_WIDE_INT
     since assemblers may not like very large sizes.  */
  if (!host_integerp (gnu_size, 1))
    return false;

  our_size = tree_low_cst (gnu_size, 1);
  return (int) our_size == our_size;
}

/* Prepend to ATTR_LIST an entry for an attribute with provided TYPE,
   NAME, ARGS and ERROR_POINT.  */

static void
prepend_one_attribute_to (struct attrib ** attr_list,
			  enum attr_type attr_type,
			  tree attr_name,
			  tree attr_args,
			  Node_Id attr_error_point)
{
  struct attrib * attr = (struct attrib *) xmalloc (sizeof (struct attrib));

  attr->type = attr_type;
  attr->name = attr_name;
  attr->args = attr_args;
  attr->error_point = attr_error_point;

  attr->next = *attr_list;
  *attr_list = attr;
}

/* Prepend to ATTR_LIST the list of attributes for GNAT_ENTITY, if any.  */

static void
prepend_attributes (Entity_Id gnat_entity, struct attrib ** attr_list)
{
  Node_Id gnat_temp;

  /* Attributes are stored as Representation Item pragmas.  */

  for (gnat_temp = First_Rep_Item (gnat_entity); Present (gnat_temp);
       gnat_temp = Next_Rep_Item (gnat_temp))
    if (Nkind (gnat_temp) == N_Pragma)
      {
	tree gnu_arg0 = NULL_TREE, gnu_arg1 = NULL_TREE;
	Node_Id gnat_assoc = Pragma_Argument_Associations (gnat_temp);
	enum attr_type etype;

	/* Map the kind of pragma at hand.  Skip if this is not one
	   we know how to handle.  */

	switch (Get_Pragma_Id (Chars (Pragma_Identifier (gnat_temp))))
	  {
	  case Pragma_Machine_Attribute:
	    etype = ATTR_MACHINE_ATTRIBUTE;
	    break;

	  case Pragma_Linker_Alias:
	    etype = ATTR_LINK_ALIAS;
	    break;

	  case Pragma_Linker_Section:
	    etype = ATTR_LINK_SECTION;
	    break;

	  case Pragma_Linker_Constructor:
	    etype = ATTR_LINK_CONSTRUCTOR;
	    break;

	  case Pragma_Linker_Destructor:
	    etype = ATTR_LINK_DESTRUCTOR;
	    break;

	  case Pragma_Weak_External:
	    etype = ATTR_WEAK_EXTERNAL;
	    break;

	  case Pragma_Thread_Local_Storage:
	    etype = ATTR_THREAD_LOCAL_STORAGE;
	    break;

	  default:
	    continue;
	  }

	/* See what arguments we have and turn them into GCC trees for
	   attribute handlers.  These expect identifier for strings.  We
	   handle at most two arguments, static expressions only.  */

	if (Present (gnat_assoc) && Present (First (gnat_assoc)))
	  {
	    Node_Id gnat_arg0 = Next (First (gnat_assoc));
	    Node_Id gnat_arg1 = Empty;

	    if (Present (gnat_arg0)
		&& Is_Static_Expression (Expression (gnat_arg0)))
	      {
		gnu_arg0 = gnat_to_gnu (Expression (gnat_arg0));

		if (TREE_CODE (gnu_arg0) == STRING_CST)
		  gnu_arg0 = get_identifier (TREE_STRING_POINTER (gnu_arg0));

		gnat_arg1 = Next (gnat_arg0);
	      }

	    if (Present (gnat_arg1)
		&& Is_Static_Expression (Expression (gnat_arg1)))
	      {
		gnu_arg1 = gnat_to_gnu (Expression (gnat_arg1));

		if (TREE_CODE (gnu_arg1) == STRING_CST)
		  gnu_arg1 = get_identifier (TREE_STRING_POINTER (gnu_arg1));
	      }
	  }

	/* Prepend to the list now.  Make a list of the argument we might
	   have, as GCC expects it.  */
	prepend_one_attribute_to
	  (attr_list,
	   etype, gnu_arg0,
	   (gnu_arg1 != NULL_TREE)
	   ? build_tree_list (NULL_TREE, gnu_arg1) : NULL_TREE,
	   Present (Next (First (gnat_assoc)))
	   ? Expression (Next (First (gnat_assoc))) : gnat_temp);
      }
}

/* Given a GNAT tree GNAT_EXPR, for an expression which is a value within a
   type definition (either a bound or a discriminant value) for GNAT_ENTITY,
   return the GCC tree to use for that expression.  GNU_NAME is the suffix
   to use if a variable needs to be created and DEFINITION is true if this
   is a definition of GNAT_ENTITY.  If NEED_VALUE is true, we need a result;
   otherwise, we are just elaborating the expression for side-effects.  If
   NEED_DEBUG is true, we need a variable for debugging purposes even if it
   isn't needed for code generation.  */

static tree
elaborate_expression (Node_Id gnat_expr, Entity_Id gnat_entity, tree gnu_name,
		      bool definition, bool need_value, bool need_debug)
{
  tree gnu_expr;

  /* If we already elaborated this expression (e.g. it was involved
     in the definition of a private type), use the old value.  */
  if (present_gnu_tree (gnat_expr))
    return get_gnu_tree (gnat_expr);

  /* If we don't need a value and this is static or a discriminant,
     we don't need to do anything.  */
  if (!need_value
      && (Is_OK_Static_Expression (gnat_expr)
	  || (Nkind (gnat_expr) == N_Identifier
	      && Ekind (Entity (gnat_expr)) == E_Discriminant)))
    return NULL_TREE;

  /* If it's a static expression, we don't need a variable for debugging.  */
  if (need_debug && Is_OK_Static_Expression (gnat_expr))
    need_debug = false;

  /* Otherwise, convert this tree to its GCC equivalent and elaborate it.  */
  gnu_expr = elaborate_expression_1 (gnat_to_gnu (gnat_expr), gnat_entity,
				     gnu_name, definition, need_debug);

  /* Save the expression in case we try to elaborate this entity again.  Since
     it's not a DECL, don't check it.  Don't save if it's a discriminant.  */
  if (!CONTAINS_PLACEHOLDER_P (gnu_expr))
    save_gnu_tree (gnat_expr, gnu_expr, true);

  return need_value ? gnu_expr : error_mark_node;
}

/* Similar, but take a GNU expression and always return a result.  */

static tree
elaborate_expression_1 (tree gnu_expr, Entity_Id gnat_entity, tree gnu_name,
			bool definition, bool need_debug)
{
  const bool expr_public_p = Is_Public (gnat_entity);
  const bool expr_global_p = expr_public_p || global_bindings_p ();
  bool expr_variable_p, use_variable;

  /* In most cases, we won't see a naked FIELD_DECL because a discriminant
     reference will have been replaced with a COMPONENT_REF when the type
     is being elaborated.  However, there are some cases involving child
     types where we will.  So convert it to a COMPONENT_REF.  We hope it
     will be at the highest level of the expression in these cases.  */
  if (TREE_CODE (gnu_expr) == FIELD_DECL)
    gnu_expr = build3 (COMPONENT_REF, TREE_TYPE (gnu_expr),
		       build0 (PLACEHOLDER_EXPR, DECL_CONTEXT (gnu_expr)),
		       gnu_expr, NULL_TREE);

  /* If GNU_EXPR contains a placeholder, just return it.  We rely on the fact
     that an expression cannot contain both a discriminant and a variable.  */
  if (CONTAINS_PLACEHOLDER_P (gnu_expr))
    return gnu_expr;

  /* If GNU_EXPR is neither a constant nor based on a read-only variable, make
     a variable that is initialized to contain the expression when the package
     containing the definition is elaborated.  If this entity is defined at top
     level, replace the expression by the variable; otherwise use a SAVE_EXPR
     if this is necessary.  */
  if (CONSTANT_CLASS_P (gnu_expr))
    expr_variable_p = false;
  else
    {
      /* Skip any conversions and simple arithmetics to see if the expression
	 is based on a read-only variable.
	 ??? This really should remain read-only, but we have to think about
	 the typing of the tree here.  */
      tree inner
	= skip_simple_arithmetic (remove_conversions (gnu_expr, true));

      if (handled_component_p (inner))
	{
	  HOST_WIDE_INT bitsize, bitpos;
	  tree offset;
	  enum machine_mode mode;
	  int unsignedp, volatilep;

	  inner = get_inner_reference (inner, &bitsize, &bitpos, &offset,
				       &mode, &unsignedp, &volatilep, false);
	  /* If the offset is variable, err on the side of caution.  */
	  if (offset)
	    inner = NULL_TREE;
	}

      expr_variable_p
	= !(inner
	    && TREE_CODE (inner) == VAR_DECL
	    && (TREE_READONLY (inner) || DECL_READONLY_ONCE_ELAB (inner)));
    }

  /* We only need to use the variable if we are in a global context since GCC
     can do the right thing in the local case.  However, when not optimizing,
     use it for bounds of loop iteration scheme to avoid code duplication.  */
  use_variable = expr_variable_p
		 && (expr_global_p
		     || (!optimize
			 && Is_Itype (gnat_entity)
			 && Nkind (Associated_Node_For_Itype (gnat_entity))
			    == N_Loop_Parameter_Specification));

  /* Now create it, possibly only for debugging purposes.  */
  if (use_variable || need_debug)
    {
      tree gnu_decl
	= create_var_decl_1
	  (create_concat_name (gnat_entity, IDENTIFIER_POINTER (gnu_name)),
	   NULL_TREE, TREE_TYPE (gnu_expr), gnu_expr, true, expr_public_p,
	   !definition, expr_global_p, !need_debug, NULL, gnat_entity);

      if (use_variable)
	return gnu_decl;
    }

  return expr_variable_p ? gnat_save_expr (gnu_expr) : gnu_expr;
}

/* Similar, but take an alignment factor and make it explicit in the tree.  */

static tree
elaborate_expression_2 (tree gnu_expr, Entity_Id gnat_entity, tree gnu_name,
			bool definition, bool need_debug, unsigned int align)
{
  tree unit_align = size_int (align / BITS_PER_UNIT);
  return
    size_binop (MULT_EXPR,
		elaborate_expression_1 (size_binop (EXACT_DIV_EXPR,
						    gnu_expr,
						    unit_align),
					gnat_entity, gnu_name, definition,
					need_debug),
		unit_align);
}

/* Create a record type that contains a SIZE bytes long field of TYPE with a
   starting bit position so that it is aligned to ALIGN bits, and leaving at
   least ROOM bytes free before the field.  BASE_ALIGN is the alignment the
   record is guaranteed to get.  */

tree
make_aligning_type (tree type, unsigned int align, tree size,
		    unsigned int base_align, int room)
{
  /* We will be crafting a record type with one field at a position set to be
     the next multiple of ALIGN past record'address + room bytes.  We use a
     record placeholder to express record'address.  */
  tree record_type = make_node (RECORD_TYPE);
  tree record = build0 (PLACEHOLDER_EXPR, record_type);

  tree record_addr_st
    = convert (sizetype, build_unary_op (ADDR_EXPR, NULL_TREE, record));

  /* The diagram below summarizes the shape of what we manipulate:

                    <--------- pos ---------->
                {  +------------+-------------+-----------------+
      record  =>{  |############|     ...     | field (type)    |
                {  +------------+-------------+-----------------+
		   |<-- room -->|<- voffset ->|<---- size ----->|
		   o            o
		   |            |
		   record_addr  vblock_addr

     Every length is in sizetype bytes there, except "pos" which has to be
     set as a bit position in the GCC tree for the record.  */
  tree room_st = size_int (room);
  tree vblock_addr_st = size_binop (PLUS_EXPR, record_addr_st, room_st);
  tree voffset_st, pos, field;

  tree name = TYPE_NAME (type);

  if (TREE_CODE (name) == TYPE_DECL)
    name = DECL_NAME (name);
  name = concat_name (name, "ALIGN");
  TYPE_NAME (record_type) = name;

  /* Compute VOFFSET and then POS.  The next byte position multiple of some
     alignment after some address is obtained by "and"ing the alignment minus
     1 with the two's complement of the address.   */
  voffset_st = size_binop (BIT_AND_EXPR,
			   fold_build1 (NEGATE_EXPR, sizetype, vblock_addr_st),
			   size_int ((align / BITS_PER_UNIT) - 1));

  /* POS = (ROOM + VOFFSET) * BIT_PER_UNIT, in bitsizetype.  */
  pos = size_binop (MULT_EXPR,
		    convert (bitsizetype,
			     size_binop (PLUS_EXPR, room_st, voffset_st)),
                    bitsize_unit_node);

  /* Craft the GCC record representation.  We exceptionally do everything
     manually here because 1) our generic circuitry is not quite ready to
     handle the complex position/size expressions we are setting up, 2) we
     have a strong simplifying factor at hand: we know the maximum possible
     value of voffset, and 3) we have to set/reset at least the sizes in
     accordance with this maximum value anyway, as we need them to convey
     what should be "alloc"ated for this type.

     Use -1 as the 'addressable' indication for the field to prevent the
     creation of a bitfield.  We don't need one, it would have damaging
     consequences on the alignment computation, and create_field_decl would
     make one without this special argument, for instance because of the
     complex position expression.  */
  field = create_field_decl (get_identifier ("F"), type, record_type, size,
			     pos, 1, -1);
  TYPE_FIELDS (record_type) = field;

  TYPE_ALIGN (record_type) = base_align;
  TYPE_USER_ALIGN (record_type) = 1;

  TYPE_SIZE (record_type)
    = size_binop (PLUS_EXPR,
                  size_binop (MULT_EXPR, convert (bitsizetype, size),
                              bitsize_unit_node),
		  bitsize_int (align + room * BITS_PER_UNIT));
  TYPE_SIZE_UNIT (record_type)
    = size_binop (PLUS_EXPR, size,
		  size_int (room + align / BITS_PER_UNIT));

  SET_TYPE_MODE (record_type, BLKmode);
  relate_alias_sets (record_type, type, ALIAS_SET_COPY);

  /* Declare it now since it will never be declared otherwise.  This is
     necessary to ensure that its subtrees are properly marked.  */
  create_type_decl (name, record_type, NULL, true, false, Empty);

  return record_type;
}

/* Return the result of rounding T up to ALIGN.  */

static inline unsigned HOST_WIDE_INT
round_up_to_align (unsigned HOST_WIDE_INT t, unsigned int align)
{
  t += align - 1;
  t /= align;
  t *= align;
  return t;
}

/* TYPE is a RECORD_TYPE, UNION_TYPE or QUAL_UNION_TYPE that is being used
   as the field type of a packed record if IN_RECORD is true, or as the
   component type of a packed array if IN_RECORD is false.  See if we can
   rewrite it either as a type that has a non-BLKmode, which we can pack
   tighter in the packed record case, or as a smaller type.  If so, return
   the new type.  If not, return the original type.  */

static tree
make_packable_type (tree type, bool in_record)
{
  unsigned HOST_WIDE_INT size = tree_low_cst (TYPE_SIZE (type), 1);
  unsigned HOST_WIDE_INT new_size;
  tree new_type, old_field, field_list = NULL_TREE;

  /* No point in doing anything if the size is zero.  */
  if (size == 0)
    return type;

  new_type = make_node (TREE_CODE (type));

  /* Copy the name and flags from the old type to that of the new.
     Note that we rely on the pointer equality created here for
     TYPE_NAME to look through conversions in various places.  */
  TYPE_NAME (new_type) = TYPE_NAME (type);
  TYPE_JUSTIFIED_MODULAR_P (new_type) = TYPE_JUSTIFIED_MODULAR_P (type);
  TYPE_CONTAINS_TEMPLATE_P (new_type) = TYPE_CONTAINS_TEMPLATE_P (type);
  if (TREE_CODE (type) == RECORD_TYPE)
    TYPE_PADDING_P (new_type) = TYPE_PADDING_P (type);

  /* If we are in a record and have a small size, set the alignment to
     try for an integral mode.  Otherwise set it to try for a smaller
     type with BLKmode.  */
  if (in_record && size <= MAX_FIXED_MODE_SIZE)
    {
      TYPE_ALIGN (new_type) = ceil_alignment (size);
      new_size = round_up_to_align (size, TYPE_ALIGN (new_type));
    }
  else
    {
      unsigned HOST_WIDE_INT align;

      /* Do not try to shrink the size if the RM size is not constant.  */
      if (TYPE_CONTAINS_TEMPLATE_P (type)
	  || !host_integerp (TYPE_ADA_SIZE (type), 1))
	return type;

      /* Round the RM size up to a unit boundary to get the minimal size
	 for a BLKmode record.  Give up if it's already the size.  */
      new_size = TREE_INT_CST_LOW (TYPE_ADA_SIZE (type));
      new_size = round_up_to_align (new_size, BITS_PER_UNIT);
      if (new_size == size)
	return type;

      align = new_size & -new_size;
      TYPE_ALIGN (new_type) = MIN (TYPE_ALIGN (type), align);
    }

  TYPE_USER_ALIGN (new_type) = 1;

  /* Now copy the fields, keeping the position and size as we don't want
     to change the layout by propagating the packedness downwards.  */
  for (old_field = TYPE_FIELDS (type); old_field;
       old_field = DECL_CHAIN (old_field))
    {
      tree new_field_type = TREE_TYPE (old_field);
      tree new_field, new_size;

      if (RECORD_OR_UNION_TYPE_P (new_field_type)
	  && !TYPE_FAT_POINTER_P (new_field_type)
	  && host_integerp (TYPE_SIZE (new_field_type), 1))
	new_field_type = make_packable_type (new_field_type, true);

      /* However, for the last field in a not already packed record type
	 that is of an aggregate type, we need to use the RM size in the
	 packable version of the record type, see finish_record_type.  */
      if (!DECL_CHAIN (old_field)
	  && !TYPE_PACKED (type)
	  && RECORD_OR_UNION_TYPE_P (new_field_type)
	  && !TYPE_FAT_POINTER_P (new_field_type)
	  && !TYPE_CONTAINS_TEMPLATE_P (new_field_type)
	  && TYPE_ADA_SIZE (new_field_type))
	new_size = TYPE_ADA_SIZE (new_field_type);
      else
	new_size = DECL_SIZE (old_field);

      new_field
	= create_field_decl (DECL_NAME (old_field), new_field_type, new_type,
			     new_size, bit_position (old_field),
			     TYPE_PACKED (type),
			     !DECL_NONADDRESSABLE_P (old_field));

      DECL_INTERNAL_P (new_field) = DECL_INTERNAL_P (old_field);
      SET_DECL_ORIGINAL_FIELD_TO_FIELD (new_field, old_field);
      if (TREE_CODE (new_type) == QUAL_UNION_TYPE)
	DECL_QUALIFIER (new_field) = DECL_QUALIFIER (old_field);

      DECL_CHAIN (new_field) = field_list;
      field_list = new_field;
    }

  finish_record_type (new_type, nreverse (field_list), 2, false);
  relate_alias_sets (new_type, type, ALIAS_SET_COPY);
  SET_DECL_PARALLEL_TYPE (TYPE_STUB_DECL (new_type),
			  DECL_PARALLEL_TYPE (TYPE_STUB_DECL (type)));

  /* If this is a padding record, we never want to make the size smaller
     than what was specified.  For QUAL_UNION_TYPE, also copy the size.  */
  if (TYPE_IS_PADDING_P (type) || TREE_CODE (type) == QUAL_UNION_TYPE)
    {
      TYPE_SIZE (new_type) = TYPE_SIZE (type);
      TYPE_SIZE_UNIT (new_type) = TYPE_SIZE_UNIT (type);
      new_size = size;
    }
  else
    {
      TYPE_SIZE (new_type) = bitsize_int (new_size);
      TYPE_SIZE_UNIT (new_type)
	= size_int ((new_size + BITS_PER_UNIT - 1) / BITS_PER_UNIT);
    }

  if (!TYPE_CONTAINS_TEMPLATE_P (type))
    SET_TYPE_ADA_SIZE (new_type, TYPE_ADA_SIZE (type));

  compute_record_mode (new_type);

  /* Try harder to get a packable type if necessary, for example
     in case the record itself contains a BLKmode field.  */
  if (in_record && TYPE_MODE (new_type) == BLKmode)
    SET_TYPE_MODE (new_type,
		   mode_for_size_tree (TYPE_SIZE (new_type), MODE_INT, 1));

  /* If neither the mode nor the size has shrunk, return the old type.  */
  if (TYPE_MODE (new_type) == BLKmode && new_size >= size)
    return type;

  return new_type;
}

/* Ensure that TYPE has SIZE and ALIGN.  Make and return a new padded type
   if needed.  We have already verified that SIZE and TYPE are large enough.
   GNAT_ENTITY is used to name the resulting record and to issue a warning.
   IS_COMPONENT_TYPE is true if this is being done for the component type
   of an array.  IS_USER_TYPE is true if we must complete the original type.
   DEFINITION is true if this type is being defined.  SAME_RM_SIZE is true
   if the RM size of the resulting type is to be set to SIZE too; otherwise,
   it's set to the RM size of the original type.  */

tree
maybe_pad_type (tree type, tree size, unsigned int align,
		Entity_Id gnat_entity, bool is_component_type,
		bool is_user_type, bool definition, bool same_rm_size)
{
  tree orig_rm_size = same_rm_size ? NULL_TREE : rm_size (type);
  tree orig_size = TYPE_SIZE (type);
  tree record, field;

  /* If TYPE is a padded type, see if it agrees with any size and alignment
     we were given.  If so, return the original type.  Otherwise, strip
     off the padding, since we will either be returning the inner type
     or repadding it.  If no size or alignment is specified, use that of
     the original padded type.  */
  if (TYPE_IS_PADDING_P (type))
    {
      if ((!size
	   || operand_equal_p (round_up (size,
					 MAX (align, TYPE_ALIGN (type))),
			       round_up (TYPE_SIZE (type),
					 MAX (align, TYPE_ALIGN (type))),
			       0))
	  && (align == 0 || align == TYPE_ALIGN (type)))
	return type;

      if (!size)
	size = TYPE_SIZE (type);
      if (align == 0)
	align = TYPE_ALIGN (type);

      type = TREE_TYPE (TYPE_FIELDS (type));
      orig_size = TYPE_SIZE (type);
    }

  /* If the size is either not being changed or is being made smaller (which
     is not done here and is only valid for bitfields anyway), show the size
     isn't changing.  Likewise, clear the alignment if it isn't being
     changed.  Then return if we aren't doing anything.  */
  if (size
      && (operand_equal_p (size, orig_size, 0)
	  || (TREE_CODE (orig_size) == INTEGER_CST
	      && tree_int_cst_lt (size, orig_size))))
    size = NULL_TREE;

  if (align == TYPE_ALIGN (type))
    align = 0;

  if (align == 0 && !size)
    return type;

  /* If requested, complete the original type and give it a name.  */
  if (is_user_type)
    create_type_decl (get_entity_name (gnat_entity), type,
		      NULL, !Comes_From_Source (gnat_entity),
		      !(TYPE_NAME (type)
			&& TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
			&& DECL_IGNORED_P (TYPE_NAME (type))),
		      gnat_entity);

  /* We used to modify the record in place in some cases, but that could
     generate incorrect debugging information.  So make a new record
     type and name.  */
  record = make_node (RECORD_TYPE);
  TYPE_PADDING_P (record) = 1;

  if (Present (gnat_entity))
    TYPE_NAME (record) = create_concat_name (gnat_entity, "PAD");

  TYPE_VOLATILE (record)
    = Present (gnat_entity) && Treat_As_Volatile (gnat_entity);

  TYPE_ALIGN (record) = align;
  TYPE_SIZE (record) = size ? size : orig_size;
  TYPE_SIZE_UNIT (record)
    = convert (sizetype,
	       size_binop (CEIL_DIV_EXPR, TYPE_SIZE (record),
			   bitsize_unit_node));

  /* If we are changing the alignment and the input type is a record with
     BLKmode and a small constant size, try to make a form that has an
     integral mode.  This might allow the padding record to also have an
     integral mode, which will be much more efficient.  There is no point
     in doing so if a size is specified unless it is also a small constant
     size and it is incorrect to do so if we cannot guarantee that the mode
     will be naturally aligned since the field must always be addressable.

     ??? This might not always be a win when done for a stand-alone object:
     since the nominal and the effective type of the object will now have
     different modes, a VIEW_CONVERT_EXPR will be required for converting
     between them and it might be hard to overcome afterwards, including
     at the RTL level when the stand-alone object is accessed as a whole.  */
  if (align != 0
      && RECORD_OR_UNION_TYPE_P (type)
      && TYPE_MODE (type) == BLKmode
      && !TREE_ADDRESSABLE (type)
      && TREE_CODE (orig_size) == INTEGER_CST
      && !TREE_OVERFLOW (orig_size)
      && compare_tree_int (orig_size, MAX_FIXED_MODE_SIZE) <= 0
      && (!size
	  || (TREE_CODE (size) == INTEGER_CST
	      && compare_tree_int (size, MAX_FIXED_MODE_SIZE) <= 0)))
    {
      tree packable_type = make_packable_type (type, true);
      if (TYPE_MODE (packable_type) != BLKmode
	  && align >= TYPE_ALIGN (packable_type))
        type = packable_type;
    }

  /* Now create the field with the original size.  */
  field  = create_field_decl (get_identifier ("F"), type, record, orig_size,
			      bitsize_zero_node, 0, 1);
  DECL_INTERNAL_P (field) = 1;

  /* Do not emit debug info until after the auxiliary record is built.  */
  finish_record_type (record, field, 1, false);

  /* Set the same size for its RM size if requested; otherwise reuse
     the RM size of the original type.  */
  SET_TYPE_ADA_SIZE (record, same_rm_size ? size : orig_rm_size);

  /* Unless debugging information isn't being written for the input type,
     write a record that shows what we are a subtype of and also make a
     variable that indicates our size, if still variable.  */
  if (TREE_CODE (orig_size) != INTEGER_CST
      && TYPE_NAME (record)
      && TYPE_NAME (type)
      && !(TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	   && DECL_IGNORED_P (TYPE_NAME (type))))
    {
      tree marker = make_node (RECORD_TYPE);
      tree name = TYPE_NAME (record);
      tree orig_name = TYPE_NAME (type);

      if (TREE_CODE (name) == TYPE_DECL)
	name = DECL_NAME (name);

      if (TREE_CODE (orig_name) == TYPE_DECL)
	orig_name = DECL_NAME (orig_name);

      TYPE_NAME (marker) = concat_name (name, "XVS");
      finish_record_type (marker,
			  create_field_decl (orig_name,
					     build_reference_type (type),
					     marker, NULL_TREE, NULL_TREE,
					     0, 0),
			  0, true);

      add_parallel_type (TYPE_STUB_DECL (record), marker);

      if (definition && size && TREE_CODE (size) != INTEGER_CST)
	TYPE_SIZE_UNIT (marker)
	  = create_var_decl (concat_name (name, "XVZ"), NULL_TREE, sizetype,
			     TYPE_SIZE_UNIT (record), false, false, false,
			     false, NULL, gnat_entity);
    }

  rest_of_record_type_compilation (record);

  /* If the size was widened explicitly, maybe give a warning.  Take the
     original size as the maximum size of the input if there was an
     unconstrained record involved and round it up to the specified alignment,
     if one was specified.  */
  if (CONTAINS_PLACEHOLDER_P (orig_size))
    orig_size = max_size (orig_size, true);

  if (align)
    orig_size = round_up (orig_size, align);

  if (Present (gnat_entity)
      && size
      && TREE_CODE (size) != MAX_EXPR
      && TREE_CODE (size) != COND_EXPR
      && !operand_equal_p (size, orig_size, 0)
      && !(TREE_CODE (size) == INTEGER_CST
	   && TREE_CODE (orig_size) == INTEGER_CST
	   && (TREE_OVERFLOW (size)
	       || TREE_OVERFLOW (orig_size)
	       || tree_int_cst_lt (size, orig_size))))
    {
      Node_Id gnat_error_node = Empty;

      if (Is_Packed_Array_Type (gnat_entity))
	gnat_entity = Original_Array_Type (gnat_entity);

      if ((Ekind (gnat_entity) == E_Component
	   || Ekind (gnat_entity) == E_Discriminant)
	  && Present (Component_Clause (gnat_entity)))
	gnat_error_node = Last_Bit (Component_Clause (gnat_entity));
      else if (Present (Size_Clause (gnat_entity)))
	gnat_error_node = Expression (Size_Clause (gnat_entity));

      /* Generate message only for entities that come from source, since
	 if we have an entity created by expansion, the message will be
	 generated for some other corresponding source entity.  */
      if (Comes_From_Source (gnat_entity))
	{
	  if (Present (gnat_error_node))
	    post_error_ne_tree ("{^ }bits of & unused?",
				gnat_error_node, gnat_entity,
				size_diffop (size, orig_size));
	  else if (is_component_type)
	    post_error_ne_tree ("component of& padded{ by ^ bits}?",
				gnat_entity, gnat_entity,
				size_diffop (size, orig_size));
	}
    }

  return record;
}

/* Given a GNU tree and a GNAT list of choices, generate an expression to test
   the value passed against the list of choices.  */

tree
choices_to_gnu (tree operand, Node_Id choices)
{
  Node_Id choice;
  Node_Id gnat_temp;
  tree result = boolean_false_node;
  tree this_test, low = 0, high = 0, single = 0;

  for (choice = First (choices); Present (choice); choice = Next (choice))
    {
      switch (Nkind (choice))
	{
	case N_Range:
	  low = gnat_to_gnu (Low_Bound (choice));
	  high = gnat_to_gnu (High_Bound (choice));

	  this_test
	    = build_binary_op (TRUTH_ANDIF_EXPR, boolean_type_node,
			       build_binary_op (GE_EXPR, boolean_type_node,
						operand, low),
			       build_binary_op (LE_EXPR, boolean_type_node,
						operand, high));

	  break;

	case N_Subtype_Indication:
	  gnat_temp = Range_Expression (Constraint (choice));
	  low = gnat_to_gnu (Low_Bound (gnat_temp));
	  high = gnat_to_gnu (High_Bound (gnat_temp));

	  this_test
	    = build_binary_op (TRUTH_ANDIF_EXPR, boolean_type_node,
			       build_binary_op (GE_EXPR, boolean_type_node,
						operand, low),
			       build_binary_op (LE_EXPR, boolean_type_node,
						operand, high));
	  break;

	case N_Identifier:
	case N_Expanded_Name:
	  /* This represents either a subtype range, an enumeration
	     literal, or a constant  Ekind says which.  If an enumeration
	     literal or constant, fall through to the next case.  */
	  if (Ekind (Entity (choice)) != E_Enumeration_Literal
	      && Ekind (Entity (choice)) != E_Constant)
	    {
	      tree type = gnat_to_gnu_type (Entity (choice));

	      low = TYPE_MIN_VALUE (type);
	      high = TYPE_MAX_VALUE (type);

	      this_test
		= build_binary_op (TRUTH_ANDIF_EXPR, boolean_type_node,
				   build_binary_op (GE_EXPR, boolean_type_node,
						    operand, low),
				   build_binary_op (LE_EXPR, boolean_type_node,
						    operand, high));
	      break;
	    }

	  /* ... fall through ... */

	case N_Character_Literal:
	case N_Integer_Literal:
	  single = gnat_to_gnu (choice);
	  this_test = build_binary_op (EQ_EXPR, boolean_type_node, operand,
				       single);
	  break;

	case N_Others_Choice:
	  this_test = boolean_true_node;
	  break;

	default:
	  gcc_unreachable ();
	}

      result = build_binary_op (TRUTH_ORIF_EXPR, boolean_type_node, result,
				this_test);
    }

  return result;
}

/* Adjust PACKED setting as passed to gnat_to_gnu_field for a field of
   type FIELD_TYPE to be placed in RECORD_TYPE.  Return the result.  */

static int
adjust_packed (tree field_type, tree record_type, int packed)
{
  /* If the field contains an item of variable size, we cannot pack it
     because we cannot create temporaries of non-fixed size in case
     we need to take the address of the field.  See addressable_p and
     the notes on the addressability issues for further details.  */
  if (is_variable_size (field_type))
    return 0;

  /* If the alignment of the record is specified and the field type
     is over-aligned, request Storage_Unit alignment for the field.  */
  if (packed == -2)
    {
      if (TYPE_ALIGN (field_type) > TYPE_ALIGN (record_type))
	return -1;
      else
	return 0;
    }

  return packed;
}

/* Return a GCC tree for a field corresponding to GNAT_FIELD to be
   placed in GNU_RECORD_TYPE.

   PACKED is 1 if the enclosing record is packed, -1 if the enclosing
   record has Component_Alignment of Storage_Unit, -2 if the enclosing
   record has a specified alignment.

   DEFINITION is true if this field is for a record being defined.

   DEBUG_INFO_P is true if we need to write debug information for types
   that we may create in the process.  */

static tree
gnat_to_gnu_field (Entity_Id gnat_field, tree gnu_record_type, int packed,
		   bool definition, bool debug_info_p)
{
  const Entity_Id gnat_field_type = Etype (gnat_field);
  tree gnu_field_type = gnat_to_gnu_type (gnat_field_type);
  tree gnu_field_id = get_entity_name (gnat_field);
  tree gnu_field, gnu_size, gnu_pos;
  bool is_volatile
    = (Treat_As_Volatile (gnat_field) || Treat_As_Volatile (gnat_field_type));
  bool needs_strict_alignment
    = (is_volatile
       || Is_Aliased (gnat_field)
       || Strict_Alignment (gnat_field_type));

  /* If this field requires strict alignment, we cannot pack it because
     it would very likely be under-aligned in the record.  */
  if (needs_strict_alignment)
    packed = 0;
  else
    packed = adjust_packed (gnu_field_type, gnu_record_type, packed);

  /* If a size is specified, use it.  Otherwise, if the record type is packed,
     use the official RM size.  See "Handling of Type'Size Values" in Einfo
     for further details.  */
  if (Known_Esize (gnat_field))
    gnu_size = validate_size (Esize (gnat_field), gnu_field_type,
			      gnat_field, FIELD_DECL, false, true);
  else if (packed == 1)
    gnu_size = validate_size (RM_Size (gnat_field_type), gnu_field_type,
			      gnat_field, FIELD_DECL, false, true);
  else
    gnu_size = NULL_TREE;

  /* If we have a specified size that is smaller than that of the field's type,
     or a position is specified, and the field's type is a record that doesn't
     require strict alignment, see if we can get either an integral mode form
     of the type or a smaller form.  If we can, show a size was specified for
     the field if there wasn't one already, so we know to make this a bitfield
     and avoid making things wider.

     Changing to an integral mode form is useful when the record is packed as
     we can then place the field at a non-byte-aligned position and so achieve
     tighter packing.  This is in addition required if the field shares a byte
     with another field and the front-end lets the back-end handle the access
     to the field, because GCC cannot handle non-byte-aligned BLKmode fields.

     Changing to a smaller form is required if the specified size is smaller
     than that of the field's type and the type contains sub-fields that are
     padded, in order to avoid generating accesses to these sub-fields that
     are wider than the field.

     We avoid the transformation if it is not required or potentially useful,
     as it might entail an increase of the field's alignment and have ripple
     effects on the outer record type.  A typical case is a field known to be
     byte-aligned and not to share a byte with another field.  */
  if (!needs_strict_alignment
      && RECORD_OR_UNION_TYPE_P (gnu_field_type)
      && !TYPE_FAT_POINTER_P (gnu_field_type)
      && host_integerp (TYPE_SIZE (gnu_field_type), 1)
      && (packed == 1
	  || (gnu_size
	      && (tree_int_cst_lt (gnu_size, TYPE_SIZE (gnu_field_type))
		  || (Present (Component_Clause (gnat_field))
		      && !(UI_To_Int (Component_Bit_Offset (gnat_field))
			   % BITS_PER_UNIT == 0
			   && value_factor_p (gnu_size, BITS_PER_UNIT)))))))
    {
      tree gnu_packable_type = make_packable_type (gnu_field_type, true);
      if (gnu_packable_type != gnu_field_type)
	{
	  gnu_field_type = gnu_packable_type;
	  if (!gnu_size)
	    gnu_size = rm_size (gnu_field_type);
	}
    }

  if (Is_Atomic (gnat_field))
    check_ok_for_atomic (gnu_field_type, gnat_field, false);

  if (Present (Component_Clause (gnat_field)))
    {
      Entity_Id gnat_parent
	= Parent_Subtype (Underlying_Type (Scope (gnat_field)));

      gnu_pos = UI_To_gnu (Component_Bit_Offset (gnat_field), bitsizetype);
      gnu_size = validate_size (Esize (gnat_field), gnu_field_type,
				gnat_field, FIELD_DECL, false, true);

      /* Ensure the position does not overlap with the parent subtype, if there
	 is one.  This test is omitted if the parent of the tagged type has a
	 full rep clause since, in this case, component clauses are allowed to
	 overlay the space allocated for the parent type and the front-end has
	 checked that there are no overlapping components.  */
      if (Present (gnat_parent) && !Is_Fully_Repped_Tagged_Type (gnat_parent))
	{
	  tree gnu_parent = gnat_to_gnu_type (gnat_parent);

	  if (TREE_CODE (TYPE_SIZE (gnu_parent)) == INTEGER_CST
	      && tree_int_cst_lt (gnu_pos, TYPE_SIZE (gnu_parent)))
	    {
	      post_error_ne_tree
		("offset of& must be beyond parent{, minimum allowed is ^}",
		 First_Bit (Component_Clause (gnat_field)), gnat_field,
		 TYPE_SIZE_UNIT (gnu_parent));
	    }
	}

      /* If this field needs strict alignment, ensure the record is
	 sufficiently aligned and that that position and size are
	 consistent with the alignment.  */
      if (needs_strict_alignment)
	{
	  TYPE_ALIGN (gnu_record_type)
	    = MAX (TYPE_ALIGN (gnu_record_type), TYPE_ALIGN (gnu_field_type));

	  if (gnu_size
	      && !operand_equal_p (gnu_size, TYPE_SIZE (gnu_field_type), 0))
	    {
	      if (Is_Atomic (gnat_field) || Is_Atomic (gnat_field_type))
		post_error_ne_tree
		  ("atomic field& must be natural size of type{ (^)}",
		   Last_Bit (Component_Clause (gnat_field)), gnat_field,
		   TYPE_SIZE (gnu_field_type));

	      else if (Is_Aliased (gnat_field))
		post_error_ne_tree
		  ("size of aliased field& must be ^ bits",
		   Last_Bit (Component_Clause (gnat_field)), gnat_field,
		   TYPE_SIZE (gnu_field_type));

	      else if (Strict_Alignment (gnat_field_type))
		post_error_ne_tree
		  ("size of & with aliased or tagged components not ^ bits",
		   Last_Bit (Component_Clause (gnat_field)), gnat_field,
		   TYPE_SIZE (gnu_field_type));

	      gnu_size = NULL_TREE;
	    }

	  if (!integer_zerop (size_binop
			      (TRUNC_MOD_EXPR, gnu_pos,
			       bitsize_int (TYPE_ALIGN (gnu_field_type)))))
	    {
	      if (is_volatile)
		post_error_ne_num
		  ("position of volatile field& must be multiple of ^ bits",
		   First_Bit (Component_Clause (gnat_field)), gnat_field,
		   TYPE_ALIGN (gnu_field_type));

	      else if (Is_Aliased (gnat_field))
		post_error_ne_num
		  ("position of aliased field& must be multiple of ^ bits",
		   First_Bit (Component_Clause (gnat_field)), gnat_field,
		   TYPE_ALIGN (gnu_field_type));

	      else if (Strict_Alignment (gnat_field_type))
		post_error_ne_num
  ("position of & with aliased or tagged components not multiple of ^ bits",
		   First_Bit (Component_Clause (gnat_field)), gnat_field,
		   TYPE_ALIGN (gnu_field_type));

	      else
		gcc_unreachable ();

	      gnu_pos = NULL_TREE;
	    }
	}
    }

  /* If the record has rep clauses and this is the tag field, make a rep
     clause for it as well.  */
  else if (Has_Specified_Layout (Scope (gnat_field))
	   && Chars (gnat_field) == Name_uTag)
    {
      gnu_pos = bitsize_zero_node;
      gnu_size = TYPE_SIZE (gnu_field_type);
    }

  else
    {
      gnu_pos = NULL_TREE;

      /* If we are packing the record and the field is BLKmode, round the
	 size up to a byte boundary.  */
      if (packed && TYPE_MODE (gnu_field_type) == BLKmode && gnu_size)
	gnu_size = round_up (gnu_size, BITS_PER_UNIT);
    }

  /* We need to make the size the maximum for the type if it is
     self-referential and an unconstrained type.  In that case, we can't
     pack the field since we can't make a copy to align it.  */
  if (TREE_CODE (gnu_field_type) == RECORD_TYPE
      && !gnu_size
      && CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_field_type))
      && !Is_Constrained (Underlying_Type (gnat_field_type)))
    {
      gnu_size = max_size (TYPE_SIZE (gnu_field_type), true);
      packed = 0;
    }

  /* If a size is specified, adjust the field's type to it.  */
  if (gnu_size)
    {
      tree orig_field_type;

      /* If the field's type is justified modular, we would need to remove
	 the wrapper to (better) meet the layout requirements.  However we
	 can do so only if the field is not aliased to preserve the unique
	 layout and if the prescribed size is not greater than that of the
	 packed array to preserve the justification.  */
      if (!needs_strict_alignment
	  && TREE_CODE (gnu_field_type) == RECORD_TYPE
	  && TYPE_JUSTIFIED_MODULAR_P (gnu_field_type)
	  && tree_int_cst_compare (gnu_size, TYPE_ADA_SIZE (gnu_field_type))
	       <= 0)
	gnu_field_type = TREE_TYPE (TYPE_FIELDS (gnu_field_type));

      gnu_field_type
	= make_type_from_size (gnu_field_type, gnu_size,
			       Has_Biased_Representation (gnat_field));

      orig_field_type = gnu_field_type;
      gnu_field_type = maybe_pad_type (gnu_field_type, gnu_size, 0, gnat_field,
				       false, false, definition, true);

      /* If a padding record was made, declare it now since it will never be
	 declared otherwise.  This is necessary to ensure that its subtrees
	 are properly marked.  */
      if (gnu_field_type != orig_field_type
	  && !DECL_P (TYPE_NAME (gnu_field_type)))
	create_type_decl (TYPE_NAME (gnu_field_type), gnu_field_type, NULL,
			  true, debug_info_p, gnat_field);
    }

  /* Otherwise (or if there was an error), don't specify a position.  */
  else
    gnu_pos = NULL_TREE;

  gcc_assert (TREE_CODE (gnu_field_type) != RECORD_TYPE
	      || !TYPE_CONTAINS_TEMPLATE_P (gnu_field_type));

  /* Now create the decl for the field.  */
  gnu_field
    = create_field_decl (gnu_field_id, gnu_field_type, gnu_record_type,
			 gnu_size, gnu_pos, packed, Is_Aliased (gnat_field));
  Sloc_to_locus (Sloc (gnat_field), &DECL_SOURCE_LOCATION (gnu_field));
  TREE_THIS_VOLATILE (gnu_field) = TREE_SIDE_EFFECTS (gnu_field) = is_volatile;

  if (Ekind (gnat_field) == E_Discriminant)
    DECL_DISCRIMINANT_NUMBER (gnu_field)
      = UI_To_gnu (Discriminant_Number (gnat_field), sizetype);

  return gnu_field;
}

/* Return true if TYPE is a type with variable size, a padding type with a
   field of variable size or is a record that has a field such a field.  */

static bool
is_variable_size (tree type)
{
  tree field;

  if (!TREE_CONSTANT (TYPE_SIZE (type)))
    return true;

  if (TYPE_IS_PADDING_P (type)
      && !TREE_CONSTANT (DECL_SIZE (TYPE_FIELDS (type))))
    return true;

  if (!RECORD_OR_UNION_TYPE_P (type))
    return false;

  for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    if (is_variable_size (TREE_TYPE (field)))
      return true;

  return false;
}

/* qsort comparer for the bit positions of two record components.  */

static int
compare_field_bitpos (const PTR rt1, const PTR rt2)
{
  const_tree const field1 = * (const_tree const *) rt1;
  const_tree const field2 = * (const_tree const *) rt2;
  const int ret
    = tree_int_cst_compare (bit_position (field1), bit_position (field2));

  return ret ? ret : (int) (DECL_UID (field1) - DECL_UID (field2));
}

/* Translate and chain the GNAT_COMPONENT_LIST to the GNU_FIELD_LIST, set
   the result as the field list of GNU_RECORD_TYPE and finish it up.  When
   called from gnat_to_gnu_entity during the processing of a record type
   definition, the GCC node for the parent, if any, will be the single field
   of GNU_RECORD_TYPE and the GCC nodes for the discriminants will be on the
   GNU_FIELD_LIST.  The other calls to this function are recursive calls for
   the component list of a variant and, in this case, GNU_FIELD_LIST is empty.

   PACKED is 1 if this is for a packed record, -1 if this is for a record
   with Component_Alignment of Storage_Unit, -2 if this is for a record
   with a specified alignment.

   DEFINITION is true if we are defining this record type.

   CANCEL_ALIGNMENT is true if the alignment should be zeroed before laying
   out the record.  This means the alignment only serves to force fields to
   be bitfields, but not to require the record to be that aligned.  This is
   used for variants.

   ALL_REP is true if a rep clause is present for all the fields.

   UNCHECKED_UNION is true if we are building this type for a record with a
   Pragma Unchecked_Union.

   DEBUG_INFO is true if we need to write debug information about the type.

   MAYBE_UNUSED is true if this type may be unused in the end; this doesn't
   mean that its contents may be unused as well, only the container itself.

   REORDER is true if we are permitted to reorder components of this type.

   FIRST_FREE_POS, if nonzero, is the first (lowest) free field position in
   the outer record type down to this variant level.  It is nonzero only if
   all the fields down to this level have a rep clause and ALL_REP is false.

   P_GNU_REP_LIST, if nonzero, is a pointer to a list to which each field
   with a rep clause is to be added; in this case, that is all that should
   be done with such fields.  */

static void
components_to_record (tree gnu_record_type, Node_Id gnat_component_list,
		      tree gnu_field_list, int packed, bool definition,
		      bool cancel_alignment, bool all_rep,
		      bool unchecked_union, bool debug_info,
		      bool maybe_unused, bool reorder,
		      tree first_free_pos, tree *p_gnu_rep_list)
{
  bool all_rep_and_size = all_rep && TYPE_SIZE (gnu_record_type);
  bool layout_with_rep = false;
  Node_Id component_decl, variant_part;
  tree gnu_field, gnu_next, gnu_last;
  tree gnu_rep_part = NULL_TREE;
  tree gnu_variant_part = NULL_TREE;
  tree gnu_rep_list = NULL_TREE;
  tree gnu_var_list = NULL_TREE;
  tree gnu_self_list = NULL_TREE;

  /* For each component referenced in a component declaration create a GCC
     field and add it to the list, skipping pragmas in the GNAT list.  */
  gnu_last = tree_last (gnu_field_list);
  if (Present (Component_Items (gnat_component_list)))
    for (component_decl
	   = First_Non_Pragma (Component_Items (gnat_component_list));
	 Present (component_decl);
	 component_decl = Next_Non_Pragma (component_decl))
      {
	Entity_Id gnat_field = Defining_Entity (component_decl);
	Name_Id gnat_name = Chars (gnat_field);

	/* If present, the _Parent field must have been created as the single
	   field of the record type.  Put it before any other fields.  */
	if (gnat_name == Name_uParent)
	  {
	    gnu_field = TYPE_FIELDS (gnu_record_type);
	    gnu_field_list = chainon (gnu_field_list, gnu_field);
	  }
	else
	  {
	    gnu_field = gnat_to_gnu_field (gnat_field, gnu_record_type, packed,
					   definition, debug_info);

	    /* If this is the _Tag field, put it before any other fields.  */
	    if (gnat_name == Name_uTag)
	      gnu_field_list = chainon (gnu_field_list, gnu_field);

	    /* If this is the _Controller field, put it before the other
	       fields except for the _Tag or _Parent field.  */
	    else if (gnat_name == Name_uController && gnu_last)
	      {
		DECL_CHAIN (gnu_field) = DECL_CHAIN (gnu_last);
		DECL_CHAIN (gnu_last) = gnu_field;
	      }

	    /* If this is a regular field, put it after the other fields.  */
	    else
	      {
		DECL_CHAIN (gnu_field) = gnu_field_list;
		gnu_field_list = gnu_field;
		if (!gnu_last)
		  gnu_last = gnu_field;
	      }
	  }

	save_gnu_tree (gnat_field, gnu_field, false);
      }

  /* At the end of the component list there may be a variant part.  */
  variant_part = Variant_Part (gnat_component_list);

  /* We create a QUAL_UNION_TYPE for the variant part since the variants are
     mutually exclusive and should go in the same memory.  To do this we need
     to treat each variant as a record whose elements are created from the
     component list for the variant.  So here we create the records from the
     lists for the variants and put them all into the QUAL_UNION_TYPE.
     If this is an Unchecked_Union, we make a UNION_TYPE instead or
     use GNU_RECORD_TYPE if there are no fields so far.  */
  if (Present (variant_part))
    {
      Node_Id gnat_discr = Name (variant_part), variant;
      tree gnu_discr = gnat_to_gnu (gnat_discr);
      tree gnu_name = TYPE_NAME (gnu_record_type);
      tree gnu_var_name
	= concat_name (get_identifier (Get_Name_String (Chars (gnat_discr))),
		       "XVN");
      tree gnu_union_type, gnu_union_name;
      tree this_first_free_pos, gnu_variant_list = NULL_TREE;

      if (TREE_CODE (gnu_name) == TYPE_DECL)
	gnu_name = DECL_NAME (gnu_name);

      gnu_union_name
	= concat_name (gnu_name, IDENTIFIER_POINTER (gnu_var_name));

      /* Reuse the enclosing union if this is an Unchecked_Union whose fields
	 are all in the variant part, to match the layout of C unions.  There
	 is an associated check below.  */
      if (TREE_CODE (gnu_record_type) == UNION_TYPE)
	gnu_union_type = gnu_record_type;
      else
	{
	  gnu_union_type
	    = make_node (unchecked_union ? UNION_TYPE : QUAL_UNION_TYPE);

	  TYPE_NAME (gnu_union_type) = gnu_union_name;
	  TYPE_ALIGN (gnu_union_type) = 0;
	  TYPE_PACKED (gnu_union_type) = TYPE_PACKED (gnu_record_type);
	}

      /* If all the fields down to this level have a rep clause, find out
	 whether all the fields at this level also have one.  If so, then
	 compute the new first free position to be passed downward.  */
      this_first_free_pos = first_free_pos;
      if (this_first_free_pos)
	{
	  for (gnu_field = gnu_field_list;
	       gnu_field;
	       gnu_field = DECL_CHAIN (gnu_field))
	    if (DECL_FIELD_OFFSET (gnu_field))
	      {
		tree pos = bit_position (gnu_field);
		if (!tree_int_cst_lt (pos, this_first_free_pos))
		  this_first_free_pos
		    = size_binop (PLUS_EXPR, pos, DECL_SIZE (gnu_field));
	      }
	    else
	      {
		this_first_free_pos = NULL_TREE;
		break;
	      }
	}

      for (variant = First_Non_Pragma (Variants (variant_part));
	   Present (variant);
	   variant = Next_Non_Pragma (variant))
	{
	  tree gnu_variant_type = make_node (RECORD_TYPE);
	  tree gnu_inner_name;
	  tree gnu_qual;

	  Get_Variant_Encoding (variant);
	  gnu_inner_name = get_identifier_with_length (Name_Buffer, Name_Len);
	  TYPE_NAME (gnu_variant_type)
	    = concat_name (gnu_union_name,
			   IDENTIFIER_POINTER (gnu_inner_name));

	  /* Set the alignment of the inner type in case we need to make
	     inner objects into bitfields, but then clear it out so the
	     record actually gets only the alignment required.  */
	  TYPE_ALIGN (gnu_variant_type) = TYPE_ALIGN (gnu_record_type);
	  TYPE_PACKED (gnu_variant_type) = TYPE_PACKED (gnu_record_type);

	  /* Similarly, if the outer record has a size specified and all
	     the fields have a rep clause, we can propagate the size.  */
	  if (all_rep_and_size)
	    {
	      TYPE_SIZE (gnu_variant_type) = TYPE_SIZE (gnu_record_type);
	      TYPE_SIZE_UNIT (gnu_variant_type)
		= TYPE_SIZE_UNIT (gnu_record_type);
	    }

	  /* Add the fields into the record type for the variant.  Note that
	     we aren't sure to really use it at this point, see below.  */
	  components_to_record (gnu_variant_type, Component_List (variant),
				NULL_TREE, packed, definition,
				!all_rep_and_size, all_rep, unchecked_union,
				debug_info, true, reorder, this_first_free_pos,
				all_rep || this_first_free_pos
				? NULL : &gnu_rep_list);

	  gnu_qual = choices_to_gnu (gnu_discr, Discrete_Choices (variant));
	  Set_Present_Expr (variant, annotate_value (gnu_qual));

	  /* If this is an Unchecked_Union whose fields are all in the variant
	     part and we have a single field with no representation clause or
	     placed at offset zero, use the field directly to match the layout
	     of C unions.  */
	  if (TREE_CODE (gnu_record_type) == UNION_TYPE
	      && (gnu_field = TYPE_FIELDS (gnu_variant_type)) != NULL_TREE
	      && !DECL_CHAIN (gnu_field)
	      && (!DECL_FIELD_OFFSET (gnu_field)
		  || integer_zerop (bit_position (gnu_field))))
	    DECL_CONTEXT (gnu_field) = gnu_union_type;
	  else
	    {
	      /* Deal with packedness like in gnat_to_gnu_field.  */
	      int field_packed
		= adjust_packed (gnu_variant_type, gnu_record_type, packed);

	      /* Finalize the record type now.  We used to throw away
		 empty records but we no longer do that because we need
		 them to generate complete debug info for the variant;
		 otherwise, the union type definition will be lacking
		 the fields associated with these empty variants.  */
	      rest_of_record_type_compilation (gnu_variant_type);
	      create_type_decl (TYPE_NAME (gnu_variant_type), gnu_variant_type,
				NULL, true, debug_info, gnat_component_list);

	      gnu_field
		= create_field_decl (gnu_inner_name, gnu_variant_type,
				     gnu_union_type,
				     all_rep_and_size
				     ? TYPE_SIZE (gnu_variant_type) : 0,
				     all_rep_and_size
				     ? bitsize_zero_node : 0,
				     field_packed, 0);

	      DECL_INTERNAL_P (gnu_field) = 1;

	      if (!unchecked_union)
		DECL_QUALIFIER (gnu_field) = gnu_qual;
	    }

	  DECL_CHAIN (gnu_field) = gnu_variant_list;
	  gnu_variant_list = gnu_field;
	}

      /* Only make the QUAL_UNION_TYPE if there are non-empty variants.  */
      if (gnu_variant_list)
	{
	  int union_field_packed;

	  if (all_rep_and_size)
	    {
	      TYPE_SIZE (gnu_union_type) = TYPE_SIZE (gnu_record_type);
	      TYPE_SIZE_UNIT (gnu_union_type)
		= TYPE_SIZE_UNIT (gnu_record_type);
	    }

	  finish_record_type (gnu_union_type, nreverse (gnu_variant_list),
			      all_rep_and_size ? 1 : 0, debug_info);

	  /* If GNU_UNION_TYPE is our record type, it means we must have an
	     Unchecked_Union with no fields.  Verify that and, if so, just
	     return.  */
	  if (gnu_union_type == gnu_record_type)
	    {
	      gcc_assert (unchecked_union
			  && !gnu_field_list
			  && !gnu_rep_list);
	      return;
	    }

	  create_type_decl (TYPE_NAME (gnu_union_type), gnu_union_type,
			    NULL, true, debug_info, gnat_component_list);

	  /* Deal with packedness like in gnat_to_gnu_field.  */
	  union_field_packed
	    = adjust_packed (gnu_union_type, gnu_record_type, packed);

	  gnu_variant_part
	    = create_field_decl (gnu_var_name, gnu_union_type, gnu_record_type,
				 all_rep ? TYPE_SIZE (gnu_union_type) : 0,
				 all_rep || this_first_free_pos
				 ? bitsize_zero_node : 0,
				 union_field_packed, 0);

	  DECL_INTERNAL_P (gnu_variant_part) = 1;
	}
    }

  /* From now on, a zero FIRST_FREE_POS is totally useless.  */
  if (first_free_pos && integer_zerop (first_free_pos))
    first_free_pos = NULL_TREE;

  /* Scan GNU_FIELD_LIST and see if any fields have rep clauses and, if we are
     permitted to reorder components, self-referential sizes or variable sizes.
     If they do, pull them out and put them onto the appropriate list.  We have
     to do this in a separate pass since we want to handle the discriminants
     but can't play with them until we've used them in debugging data above.

     ??? If we reorder them, debugging information will be wrong but there is
     nothing that can be done about this at the moment.  */
  gnu_last = NULL_TREE;

#define MOVE_FROM_FIELD_LIST_TO(LIST)	\
  do {					\
    if (gnu_last)			\
      DECL_CHAIN (gnu_last) = gnu_next;	\
    else				\
      gnu_field_list = gnu_next;	\
					\
    DECL_CHAIN (gnu_field) = (LIST);	\
    (LIST) = gnu_field;			\
  } while (0)

  for (gnu_field = gnu_field_list; gnu_field; gnu_field = gnu_next)
    {
      gnu_next = DECL_CHAIN (gnu_field);

      if (DECL_FIELD_OFFSET (gnu_field))
	{
	  MOVE_FROM_FIELD_LIST_TO (gnu_rep_list);
	  continue;
	}

      /* Reorder non-internal fields with non-fixed size.  */
      if (reorder
	  && !DECL_INTERNAL_P (gnu_field)
	  && !(DECL_SIZE (gnu_field)
	       && TREE_CODE (DECL_SIZE (gnu_field)) == INTEGER_CST))
	{
	  tree type_size = TYPE_SIZE (TREE_TYPE (gnu_field));

	  if (CONTAINS_PLACEHOLDER_P (type_size))
	    {
	      MOVE_FROM_FIELD_LIST_TO (gnu_self_list);
	      continue;
	    }

	  if (TREE_CODE (type_size) != INTEGER_CST)
	    {
	      MOVE_FROM_FIELD_LIST_TO (gnu_var_list);
	      continue;
	    }
	}

      gnu_last = gnu_field;
    }

#undef MOVE_FROM_FIELD_LIST_TO

  /* If permitted, we reorder the components as follows:

       1) all fixed length fields,
       2) all fields whose length doesn't depend on discriminants,
       3) all fields whose length depends on discriminants,
       4) the variant part,

     within the record and within each variant recursively.  */
  if (reorder)
    gnu_field_list
      = chainon (nreverse (gnu_self_list),
		 chainon (nreverse (gnu_var_list), gnu_field_list));

  /* If P_REP_LIST is nonzero, this means that we are asked to move the fields
     in our REP list to the previous level because this level needs them in
     order to do a correct layout, i.e. avoid having overlapping fields.  */
  if (p_gnu_rep_list && gnu_rep_list)
    *p_gnu_rep_list = chainon (*p_gnu_rep_list, gnu_rep_list);

  /* Otherwise, sort the fields by bit position and put them into their own
     record, before the others, if we also have fields without rep clause.  */
  else if (gnu_rep_list)
    {
      tree gnu_rep_type
	= (gnu_field_list ? make_node (RECORD_TYPE) : gnu_record_type);
      int i, len = list_length (gnu_rep_list);
      tree *gnu_arr = XALLOCAVEC (tree, len);

      for (gnu_field = gnu_rep_list, i = 0;
	   gnu_field;
	   gnu_field = DECL_CHAIN (gnu_field), i++)
	gnu_arr[i] = gnu_field;

      qsort (gnu_arr, len, sizeof (tree), compare_field_bitpos);

      /* Put the fields in the list in order of increasing position, which
	 means we start from the end.  */
      gnu_rep_list = NULL_TREE;
      for (i = len - 1; i >= 0; i--)
	{
	  DECL_CHAIN (gnu_arr[i]) = gnu_rep_list;
	  gnu_rep_list = gnu_arr[i];
	  DECL_CONTEXT (gnu_arr[i]) = gnu_rep_type;
	}

      if (gnu_field_list)
	{
	  finish_record_type (gnu_rep_type, gnu_rep_list, 1, debug_info);

	  /* If FIRST_FREE_POS is nonzero, we need to ensure that the fields
	     without rep clause are laid out starting from this position.
	     Therefore, we force it as a minimal size on the REP part.  */
	  gnu_rep_part
	    = create_rep_part (gnu_rep_type, gnu_record_type, first_free_pos);
	}
      else
	{
	  layout_with_rep = true;
	  gnu_field_list = nreverse (gnu_rep_list);
	}
    }

  /* If FIRST_FREE_POS is nonzero, we need to ensure that the fields without
     rep clause are laid out starting from this position.  Therefore, if we
     have not already done so, we create a fake REP part with this size.  */
  if (first_free_pos && !layout_with_rep && !gnu_rep_part)
    {
      tree gnu_rep_type = make_node (RECORD_TYPE);
      finish_record_type (gnu_rep_type, NULL_TREE, 0, debug_info);
      gnu_rep_part
	= create_rep_part (gnu_rep_type, gnu_record_type, first_free_pos);
    }

  /* Now chain the REP part at the end of the reversed field list.  */
  if (gnu_rep_part)
    gnu_field_list = chainon (gnu_field_list, gnu_rep_part);

  /* And the variant part at the beginning.  */
  if (gnu_variant_part)
    {
      DECL_CHAIN (gnu_variant_part) = gnu_field_list;
      gnu_field_list = gnu_variant_part;
    }

  if (cancel_alignment)
    TYPE_ALIGN (gnu_record_type) = 0;

  finish_record_type (gnu_record_type, nreverse (gnu_field_list),
		      layout_with_rep ? 1 : 0, debug_info && !maybe_unused);
}

/* Given GNU_SIZE, a GCC tree representing a size, return a Uint to be
   placed into an Esize, Component_Bit_Offset, or Component_Size value
   in the GNAT tree.  */

static Uint
annotate_value (tree gnu_size)
{
  TCode tcode;
  Node_Ref_Or_Val ops[3], ret;
  struct tree_int_map in;
  int i;

  /* See if we've already saved the value for this node.  */
  if (EXPR_P (gnu_size))
    {
      struct tree_int_map *e;

      if (!annotate_value_cache)
        annotate_value_cache = htab_create_ggc (512, tree_int_map_hash,
					        tree_int_map_eq, 0);
      in.base.from = gnu_size;
      e = (struct tree_int_map *)
	    htab_find (annotate_value_cache, &in);

      if (e)
	return (Node_Ref_Or_Val) e->to;
    }
  else
    in.base.from = NULL_TREE;

  /* If we do not return inside this switch, TCODE will be set to the
     code to use for a Create_Node operand and LEN (set above) will be
     the number of recursive calls for us to make.  */

  switch (TREE_CODE (gnu_size))
    {
    case INTEGER_CST:
      if (TREE_OVERFLOW (gnu_size))
	return No_Uint;

      /* This may come from a conversion from some smaller type, so ensure
	 this is in bitsizetype.  */
      gnu_size = convert (bitsizetype, gnu_size);

      /* For a negative value, build NEGATE_EXPR of the opposite.  Such values
	 appear in expressions containing aligning patterns.  Note that, since
	 sizetype is sign-extended but nonetheless unsigned, we don't directly
	 use tree_int_cst_sgn.  */
      if (TREE_INT_CST_HIGH (gnu_size) < 0)
	{
	  tree op_size = fold_build1 (NEGATE_EXPR, bitsizetype, gnu_size);
	  return annotate_value (build1 (NEGATE_EXPR, bitsizetype, op_size));
	}

      return UI_From_gnu (gnu_size);

    case COMPONENT_REF:
      /* The only case we handle here is a simple discriminant reference.  */
      if (TREE_CODE (TREE_OPERAND (gnu_size, 0)) == PLACEHOLDER_EXPR
	  && TREE_CODE (TREE_OPERAND (gnu_size, 1)) == FIELD_DECL
	  && DECL_DISCRIMINANT_NUMBER (TREE_OPERAND (gnu_size, 1)))
	return Create_Node (Discrim_Val,
			    annotate_value (DECL_DISCRIMINANT_NUMBER
					    (TREE_OPERAND (gnu_size, 1))),
			    No_Uint, No_Uint);
      else
	return No_Uint;

    CASE_CONVERT:   case NON_LVALUE_EXPR:
      return annotate_value (TREE_OPERAND (gnu_size, 0));

      /* Now just list the operations we handle.  */
    case COND_EXPR:		tcode = Cond_Expr; break;
    case PLUS_EXPR:		tcode = Plus_Expr; break;
    case MINUS_EXPR:		tcode = Minus_Expr; break;
    case MULT_EXPR:		tcode = Mult_Expr; break;
    case TRUNC_DIV_EXPR:	tcode = Trunc_Div_Expr; break;
    case CEIL_DIV_EXPR:		tcode = Ceil_Div_Expr; break;
    case FLOOR_DIV_EXPR:	tcode = Floor_Div_Expr; break;
    case TRUNC_MOD_EXPR:	tcode = Trunc_Mod_Expr; break;
    case CEIL_MOD_EXPR:		tcode = Ceil_Mod_Expr; break;
    case FLOOR_MOD_EXPR:	tcode = Floor_Mod_Expr; break;
    case EXACT_DIV_EXPR:	tcode = Exact_Div_Expr; break;
    case NEGATE_EXPR:		tcode = Negate_Expr; break;
    case MIN_EXPR:		tcode = Min_Expr; break;
    case MAX_EXPR:		tcode = Max_Expr; break;
    case ABS_EXPR:		tcode = Abs_Expr; break;
    case TRUTH_ANDIF_EXPR:	tcode = Truth_Andif_Expr; break;
    case TRUTH_ORIF_EXPR:	tcode = Truth_Orif_Expr; break;
    case TRUTH_AND_EXPR:	tcode = Truth_And_Expr; break;
    case TRUTH_OR_EXPR:		tcode = Truth_Or_Expr; break;
    case TRUTH_XOR_EXPR:	tcode = Truth_Xor_Expr; break;
    case TRUTH_NOT_EXPR:	tcode = Truth_Not_Expr; break;
    case BIT_AND_EXPR:		tcode = Bit_And_Expr; break;
    case LT_EXPR:		tcode = Lt_Expr; break;
    case LE_EXPR:		tcode = Le_Expr; break;
    case GT_EXPR:		tcode = Gt_Expr; break;
    case GE_EXPR:		tcode = Ge_Expr; break;
    case EQ_EXPR:		tcode = Eq_Expr; break;
    case NE_EXPR:		tcode = Ne_Expr; break;

    case CALL_EXPR:
      {
	tree t = maybe_inline_call_in_expr (gnu_size);
	if (t)
	  return annotate_value (t);
      }

      /* Fall through... */

    default:
      return No_Uint;
    }

  /* Now get each of the operands that's relevant for this code.  If any
     cannot be expressed as a repinfo node, say we can't.  */
  for (i = 0; i < 3; i++)
    ops[i] = No_Uint;

  for (i = 0; i < TREE_CODE_LENGTH (TREE_CODE (gnu_size)); i++)
    {
      ops[i] = annotate_value (TREE_OPERAND (gnu_size, i));
      if (ops[i] == No_Uint)
	return No_Uint;
    }

  ret = Create_Node (tcode, ops[0], ops[1], ops[2]);

  /* Save the result in the cache.  */
  if (in.base.from)
    {
      struct tree_int_map **h;
      /* We can't assume the hash table data hasn't moved since the
	 initial look up, so we have to search again.  Allocating and
	 inserting an entry at that point would be an alternative, but
	 then we'd better discard the entry if we decided not to cache
	 it.  */
      h = (struct tree_int_map **)
	    htab_find_slot (annotate_value_cache, &in, INSERT);
      gcc_assert (!*h);
      *h = ggc_alloc_tree_int_map ();
      (*h)->base.from = gnu_size;
      (*h)->to = ret;
    }

  return ret;
}

/* Given GNAT_ENTITY, an object (constant, variable, parameter, exception)
   and GNU_TYPE, its corresponding GCC type, set Esize and Alignment to the
   size and alignment used by Gigi.  Prefer SIZE over TYPE_SIZE if non-null.
   BY_REF is true if the object is used by reference and BY_DOUBLE_REF is
   true if the object is used by double reference.  */

void
annotate_object (Entity_Id gnat_entity, tree gnu_type, tree size, bool by_ref,
		 bool by_double_ref)
{
  if (by_ref)
    {
      if (by_double_ref)
	gnu_type = TREE_TYPE (gnu_type);

      if (TYPE_IS_FAT_POINTER_P (gnu_type))
	gnu_type = TYPE_UNCONSTRAINED_ARRAY (gnu_type);
      else
	gnu_type = TREE_TYPE (gnu_type);
    }

  if (Unknown_Esize (gnat_entity))
    {
      if (TREE_CODE (gnu_type) == RECORD_TYPE
	  && TYPE_CONTAINS_TEMPLATE_P (gnu_type))
	size = TYPE_SIZE (TREE_TYPE (DECL_CHAIN (TYPE_FIELDS (gnu_type))));
      else if (!size)
	size = TYPE_SIZE (gnu_type);

      if (size)
	Set_Esize (gnat_entity, annotate_value (size));
    }

  if (Unknown_Alignment (gnat_entity))
    Set_Alignment (gnat_entity,
		   UI_From_Int (TYPE_ALIGN (gnu_type) / BITS_PER_UNIT));
}

/* Return first element of field list whose TREE_PURPOSE is the same as ELEM.
   Return NULL_TREE if there is no such element in the list.  */

static tree
purpose_member_field (const_tree elem, tree list)
{
  while (list)
    {
      tree field = TREE_PURPOSE (list);
      if (SAME_FIELD_P (field, elem))
	return list;
      list = TREE_CHAIN (list);
    }
  return NULL_TREE;
}

/* Given GNAT_ENTITY, a record type, and GNU_TYPE, its corresponding GCC type,
   set Component_Bit_Offset and Esize of the components to the position and
   size used by Gigi.  */

static void
annotate_rep (Entity_Id gnat_entity, tree gnu_type)
{
  Entity_Id gnat_field;
  tree gnu_list;

  /* We operate by first making a list of all fields and their position (we
     can get the size easily) and then update all the sizes in the tree.  */
  gnu_list
    = build_position_list (gnu_type, false, size_zero_node, bitsize_zero_node,
			   BIGGEST_ALIGNMENT, NULL_TREE);

  for (gnat_field = First_Entity (gnat_entity);
       Present (gnat_field);
       gnat_field = Next_Entity (gnat_field))
    if (Ekind (gnat_field) == E_Component
	|| (Ekind (gnat_field) == E_Discriminant
	    && !Is_Unchecked_Union (Scope (gnat_field))))
      {
	tree t = purpose_member_field (gnat_to_gnu_field_decl (gnat_field),
				       gnu_list);
	if (t)
	  {
	    tree parent_offset;

	    if (type_annotate_only && Is_Tagged_Type (gnat_entity))
	      {
		/* In this mode the tag and parent components are not
		   generated, so we add the appropriate offset to each
		   component.  For a component appearing in the current
		   extension, the offset is the size of the parent.  */
		if (Is_Derived_Type (gnat_entity)
		    && Original_Record_Component (gnat_field) == gnat_field)
		  parent_offset
		    = UI_To_gnu (Esize (Etype (Base_Type (gnat_entity))),
				 bitsizetype);
		else
		  parent_offset = bitsize_int (POINTER_SIZE);
	      }
	    else
	      parent_offset = bitsize_zero_node;

	    Set_Component_Bit_Offset
	      (gnat_field,
	       annotate_value
		 (size_binop (PLUS_EXPR,
			      bit_from_pos (TREE_VEC_ELT (TREE_VALUE (t), 0),
					    TREE_VEC_ELT (TREE_VALUE (t), 2)),
			      parent_offset)));

	    Set_Esize (gnat_field,
		       annotate_value (DECL_SIZE (TREE_PURPOSE (t))));
	  }
	else if (Is_Tagged_Type (gnat_entity) && Is_Derived_Type (gnat_entity))
	  {
	    /* If there is no entry, this is an inherited component whose
	       position is the same as in the parent type.  */
	    Set_Component_Bit_Offset
	      (gnat_field,
	       Component_Bit_Offset (Original_Record_Component (gnat_field)));

	    Set_Esize (gnat_field,
		       Esize (Original_Record_Component (gnat_field)));
	  }
      }
}

/* Scan all fields in GNU_TYPE and return a TREE_LIST where TREE_PURPOSE is
   the FIELD_DECL and TREE_VALUE a TREE_VEC containing the byte position, the
   value to be placed into DECL_OFFSET_ALIGN and the bit position.  The list
   of fields is flattened, except for variant parts if DO_NOT_FLATTEN_VARIANT
   is set to true.  GNU_POS is to be added to the position, GNU_BITPOS to the
   bit position, OFFSET_ALIGN is the present offset alignment.  GNU_LIST is a
   pre-existing list to be chained to the newly created entries.  */

static tree
build_position_list (tree gnu_type, bool do_not_flatten_variant, tree gnu_pos,
		     tree gnu_bitpos, unsigned int offset_align, tree gnu_list)
{
  tree gnu_field;

  for (gnu_field = TYPE_FIELDS (gnu_type);
       gnu_field;
       gnu_field = DECL_CHAIN (gnu_field))
    {
      tree gnu_our_bitpos = size_binop (PLUS_EXPR, gnu_bitpos,
					DECL_FIELD_BIT_OFFSET (gnu_field));
      tree gnu_our_offset = size_binop (PLUS_EXPR, gnu_pos,
					DECL_FIELD_OFFSET (gnu_field));
      unsigned int our_offset_align
	= MIN (offset_align, DECL_OFFSET_ALIGN (gnu_field));
      tree v = make_tree_vec (3);

      TREE_VEC_ELT (v, 0) = gnu_our_offset;
      TREE_VEC_ELT (v, 1) = size_int (our_offset_align);
      TREE_VEC_ELT (v, 2) = gnu_our_bitpos;
      gnu_list = tree_cons (gnu_field, v, gnu_list);

      /* Recurse on internal fields, flattening the nested fields except for
	 those in the variant part, if requested.  */
      if (DECL_INTERNAL_P (gnu_field))
	{
	  tree gnu_field_type = TREE_TYPE (gnu_field);
	  if (do_not_flatten_variant
	      && TREE_CODE (gnu_field_type) == QUAL_UNION_TYPE)
	    gnu_list
	      = build_position_list (gnu_field_type, do_not_flatten_variant,
				     size_zero_node, bitsize_zero_node,
				     BIGGEST_ALIGNMENT, gnu_list);
	  else
	    gnu_list
	      = build_position_list (gnu_field_type, do_not_flatten_variant,
				     gnu_our_offset, gnu_our_bitpos,
				     our_offset_align, gnu_list);
	}
    }

  return gnu_list;
}

/* Return a VEC describing the substitutions needed to reflect the
   discriminant substitutions from GNAT_TYPE to GNAT_SUBTYPE.  They can
   be in any order.  The values in an element of the VEC are in the form
   of operands to SUBSTITUTE_IN_EXPR.  DEFINITION is true if this is for
   a definition of GNAT_SUBTYPE.  */

static VEC(subst_pair,heap) *
build_subst_list (Entity_Id gnat_subtype, Entity_Id gnat_type, bool definition)
{
  VEC(subst_pair,heap) *gnu_vec = NULL;
  Entity_Id gnat_discrim;
  Node_Id gnat_value;

  for (gnat_discrim = First_Stored_Discriminant (gnat_type),
       gnat_value = First_Elmt (Stored_Constraint (gnat_subtype));
       Present (gnat_discrim);
       gnat_discrim = Next_Stored_Discriminant (gnat_discrim),
       gnat_value = Next_Elmt (gnat_value))
    /* Ignore access discriminants.  */
    if (!Is_Access_Type (Etype (Node (gnat_value))))
      {
	tree gnu_field = gnat_to_gnu_field_decl (gnat_discrim);
	tree replacement = convert (TREE_TYPE (gnu_field),
				    elaborate_expression
				    (Node (gnat_value), gnat_subtype,
				     get_entity_name (gnat_discrim),
				     definition, true, false));
	subst_pair *s = VEC_safe_push (subst_pair, heap, gnu_vec, NULL);
	s->discriminant = gnu_field;
	s->replacement = replacement;
      }

  return gnu_vec;
}

/* Scan all fields in QUAL_UNION_TYPE and return a VEC describing the
   variants of QUAL_UNION_TYPE that are still relevant after applying
   the substitutions described in SUBST_LIST.  VARIANT_LIST is a
   pre-existing VEC onto which newly created entries should be
   pushed.  */

static VEC(variant_desc,heap) *
build_variant_list (tree qual_union_type, VEC(subst_pair,heap) *subst_list,
		    VEC(variant_desc,heap) *variant_list)
{
  tree gnu_field;

  for (gnu_field = TYPE_FIELDS (qual_union_type);
       gnu_field;
       gnu_field = DECL_CHAIN (gnu_field))
    {
      tree qual = DECL_QUALIFIER (gnu_field);
      unsigned ix;
      subst_pair *s;

      FOR_EACH_VEC_ELT_REVERSE (subst_pair, subst_list, ix, s)
	qual = SUBSTITUTE_IN_EXPR (qual, s->discriminant, s->replacement);

      /* If the new qualifier is not unconditionally false, its variant may
	 still be accessed.  */
      if (!integer_zerop (qual))
	{
	  variant_desc *v;
	  tree variant_type = TREE_TYPE (gnu_field), variant_subpart;

	  v = VEC_safe_push (variant_desc, heap, variant_list, NULL);
	  v->type = variant_type;
	  v->field = gnu_field;
	  v->qual = qual;
	  v->record = NULL_TREE;

	  /* Recurse on the variant subpart of the variant, if any.  */
	  variant_subpart = get_variant_part (variant_type);
	  if (variant_subpart)
	    variant_list = build_variant_list (TREE_TYPE (variant_subpart),
					       subst_list, variant_list);

	  /* If the new qualifier is unconditionally true, the subsequent
	     variants cannot be accessed.  */
	  if (integer_onep (qual))
	    break;
	}
    }

  return variant_list;
}

/* UINT_SIZE is a Uint giving the specified size for an object of GNU_TYPE
   corresponding to GNAT_OBJECT.  If the size is valid, return an INTEGER_CST
   corresponding to its value.  Otherwise, return NULL_TREE.  KIND is set to
   VAR_DECL if we are specifying the size of an object, TYPE_DECL for the
   size of a type, and FIELD_DECL for the size of a field.  COMPONENT_P is
   true if we are being called to process the Component_Size of GNAT_OBJECT;
   this is used only for error messages.  ZERO_OK is true if a size of zero
   is permitted; if ZERO_OK is false, it means that a size of zero should be
   treated as an unspecified size.  */

static tree
validate_size (Uint uint_size, tree gnu_type, Entity_Id gnat_object,
	       enum tree_code kind, bool component_p, bool zero_ok)
{
  Node_Id gnat_error_node;
  tree type_size, size;

  /* Return 0 if no size was specified.  */
  if (uint_size == No_Uint)
    return NULL_TREE;

  /* Ignore a negative size since that corresponds to our back-annotation.  */
  if (UI_Lt (uint_size, Uint_0))
    return NULL_TREE;

  /* Find the node to use for error messages.  */
  if ((Ekind (gnat_object) == E_Component
       || Ekind (gnat_object) == E_Discriminant)
      && Present (Component_Clause (gnat_object)))
    gnat_error_node = Last_Bit (Component_Clause (gnat_object));
  else if (Present (Size_Clause (gnat_object)))
    gnat_error_node = Expression (Size_Clause (gnat_object));
  else
    gnat_error_node = gnat_object;

  /* Get the size as an INTEGER_CST.  Issue an error if a size was specified
     but cannot be represented in bitsizetype.  */
  size = UI_To_gnu (uint_size, bitsizetype);
  if (TREE_OVERFLOW (size))
    {
      if (component_p)
	post_error_ne ("component size for& is too large", gnat_error_node,
		       gnat_object);
      else
	post_error_ne ("size for& is too large", gnat_error_node,
		       gnat_object);
      return NULL_TREE;
    }

  /* Ignore a zero size if it is not permitted.  */
  if (!zero_ok && integer_zerop (size))
    return NULL_TREE;

  /* The size of objects is always a multiple of a byte.  */
  if (kind == VAR_DECL
      && !integer_zerop (size_binop (TRUNC_MOD_EXPR, size, bitsize_unit_node)))
    {
      if (component_p)
	post_error_ne ("component size for& is not a multiple of Storage_Unit",
		       gnat_error_node, gnat_object);
      else
	post_error_ne ("size for& is not a multiple of Storage_Unit",
		       gnat_error_node, gnat_object);
      return NULL_TREE;
    }

  /* If this is an integral type or a packed array type, the front-end has
     already verified the size, so we need not do it here (which would mean
     checking against the bounds).  However, if this is an aliased object,
     it may not be smaller than the type of the object.  */
  if ((INTEGRAL_TYPE_P (gnu_type) || TYPE_IS_PACKED_ARRAY_TYPE_P (gnu_type))
      && !(kind == VAR_DECL && Is_Aliased (gnat_object)))
    return size;

  /* If the object is a record that contains a template, add the size of the
     template to the specified size.  */
  if (TREE_CODE (gnu_type) == RECORD_TYPE
      && TYPE_CONTAINS_TEMPLATE_P (gnu_type))
    size = size_binop (PLUS_EXPR, DECL_SIZE (TYPE_FIELDS (gnu_type)), size);

  if (kind == VAR_DECL
      /* If a type needs strict alignment, a component of this type in
	 a packed record cannot be packed and thus uses the type size.  */
      || (kind == TYPE_DECL && Strict_Alignment (gnat_object)))
    type_size = TYPE_SIZE (gnu_type);
  else
    type_size = rm_size (gnu_type);

  /* Modify the size of a discriminated type to be the maximum size.  */
  if (type_size && CONTAINS_PLACEHOLDER_P (type_size))
    type_size = max_size (type_size, true);

  /* If this is an access type or a fat pointer, the minimum size is that given
     by the smallest integral mode that's valid for pointers.  */
  if (TREE_CODE (gnu_type) == POINTER_TYPE || TYPE_IS_FAT_POINTER_P (gnu_type))
    {
      enum machine_mode p_mode = GET_CLASS_NARROWEST_MODE (MODE_INT);
      while (!targetm.valid_pointer_mode (p_mode))
	p_mode = GET_MODE_WIDER_MODE (p_mode);
      type_size = bitsize_int (GET_MODE_BITSIZE (p_mode));
    }

  /* Issue an error either if the default size of the object isn't a constant
     or if the new size is smaller than it.  */
  if (TREE_CODE (type_size) != INTEGER_CST
      || TREE_OVERFLOW (type_size)
      || tree_int_cst_lt (size, type_size))
    {
      if (component_p)
	post_error_ne_tree
	  ("component size for& too small{, minimum allowed is ^}",
	   gnat_error_node, gnat_object, type_size);
      else
	post_error_ne_tree
	  ("size for& too small{, minimum allowed is ^}",
	   gnat_error_node, gnat_object, type_size);
      return NULL_TREE;
    }

  return size;
}

/* Similarly, but both validate and process a value of RM size.  This routine
   is only called for types.  */

static void
set_rm_size (Uint uint_size, tree gnu_type, Entity_Id gnat_entity)
{
  Node_Id gnat_attr_node;
  tree old_size, size;

  /* Do nothing if no size was specified.  */
  if (uint_size == No_Uint)
    return;

  /* Ignore a negative size since that corresponds to our back-annotation.  */
  if (UI_Lt (uint_size, Uint_0))
    return;

  /* Only issue an error if a Value_Size clause was explicitly given.
     Otherwise, we'd be duplicating an error on the Size clause.  */
  gnat_attr_node
    = Get_Attribute_Definition_Clause (gnat_entity, Attr_Value_Size);

  /* Get the size as an INTEGER_CST.  Issue an error if a size was specified
     but cannot be represented in bitsizetype.  */
  size = UI_To_gnu (uint_size, bitsizetype);
  if (TREE_OVERFLOW (size))
    {
      if (Present (gnat_attr_node))
	post_error_ne ("Value_Size for& is too large", gnat_attr_node,
		       gnat_entity);
      return;
    }

  /* Ignore a zero size unless a Value_Size clause exists, or a size clause
     exists, or this is an integer type, in which case the front-end will
     have always set it.  */
  if (No (gnat_attr_node)
      && integer_zerop (size)
      && !Has_Size_Clause (gnat_entity)
      && !Is_Discrete_Or_Fixed_Point_Type (gnat_entity))
    return;

  old_size = rm_size (gnu_type);

  /* If the old size is self-referential, get the maximum size.  */
  if (CONTAINS_PLACEHOLDER_P (old_size))
    old_size = max_size (old_size, true);

  /* Issue an error either if the old size of the object isn't a constant or
     if the new size is smaller than it.  The front-end has already verified
     this for scalar and packed array types.  */
  if (TREE_CODE (old_size) != INTEGER_CST
      || TREE_OVERFLOW (old_size)
      || (AGGREGATE_TYPE_P (gnu_type)
	  && !(TREE_CODE (gnu_type) == ARRAY_TYPE
	       && TYPE_PACKED_ARRAY_TYPE_P (gnu_type))
	  && !(TYPE_IS_PADDING_P (gnu_type)
	       && TREE_CODE (TREE_TYPE (TYPE_FIELDS (gnu_type))) == ARRAY_TYPE
	       && TYPE_PACKED_ARRAY_TYPE_P
		  (TREE_TYPE (TYPE_FIELDS (gnu_type))))
	  && tree_int_cst_lt (size, old_size)))
    {
      if (Present (gnat_attr_node))
	post_error_ne_tree
	  ("Value_Size for& too small{, minimum allowed is ^}",
	   gnat_attr_node, gnat_entity, old_size);
      return;
    }

  /* Otherwise, set the RM size proper for integral types...  */
  if ((TREE_CODE (gnu_type) == INTEGER_TYPE
       && Is_Discrete_Or_Fixed_Point_Type (gnat_entity))
      || (TREE_CODE (gnu_type) == ENUMERAL_TYPE
	  || TREE_CODE (gnu_type) == BOOLEAN_TYPE))
    SET_TYPE_RM_SIZE (gnu_type, size);

  /* ...or the Ada size for record and union types.  */
  else if (RECORD_OR_UNION_TYPE_P (gnu_type)
	   && !TYPE_FAT_POINTER_P (gnu_type))
    SET_TYPE_ADA_SIZE (gnu_type, size);
}

/* Given a type TYPE, return a new type whose size is appropriate for SIZE.
   If TYPE is the best type, return it.  Otherwise, make a new type.  We
   only support new integral and pointer types.  FOR_BIASED is true if
   we are making a biased type.  */

static tree
make_type_from_size (tree type, tree size_tree, bool for_biased)
{
  unsigned HOST_WIDE_INT size;
  bool biased_p;
  tree new_type;

  /* If size indicates an error, just return TYPE to avoid propagating
     the error.  Likewise if it's too large to represent.  */
  if (!size_tree || !host_integerp (size_tree, 1))
    return type;

  size = tree_low_cst (size_tree, 1);

  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
      biased_p = (TREE_CODE (type) == INTEGER_TYPE
		  && TYPE_BIASED_REPRESENTATION_P (type));

      /* Integer types with precision 0 are forbidden.  */
      if (size == 0)
	size = 1;

      /* Only do something if the type is not a packed array type and
	 doesn't already have the proper size.  */
      if (TYPE_PACKED_ARRAY_TYPE_P (type)
	  || (TYPE_PRECISION (type) == size && biased_p == for_biased))
	break;

      biased_p |= for_biased;
      if (size > LONG_LONG_TYPE_SIZE)
	size = LONG_LONG_TYPE_SIZE;

      if (TYPE_UNSIGNED (type) || biased_p)
	new_type = make_unsigned_type (size);
      else
	new_type = make_signed_type (size);
      TREE_TYPE (new_type) = TREE_TYPE (type) ? TREE_TYPE (type) : type;
      SET_TYPE_RM_MIN_VALUE (new_type,
			     convert (TREE_TYPE (new_type),
				      TYPE_MIN_VALUE (type)));
      SET_TYPE_RM_MAX_VALUE (new_type,
			     convert (TREE_TYPE (new_type),
				      TYPE_MAX_VALUE (type)));
      /* Copy the name to show that it's essentially the same type and
	 not a subrange type.  */
      TYPE_NAME (new_type) = TYPE_NAME (type);
      TYPE_BIASED_REPRESENTATION_P (new_type) = biased_p;
      SET_TYPE_RM_SIZE (new_type, bitsize_int (size));
      return new_type;

    case RECORD_TYPE:
      /* Do something if this is a fat pointer, in which case we
	 may need to return the thin pointer.  */
      if (TYPE_FAT_POINTER_P (type) && size < POINTER_SIZE * 2)
	{
	  enum machine_mode p_mode = mode_for_size (size, MODE_INT, 0);
	  if (!targetm.valid_pointer_mode (p_mode))
	    p_mode = ptr_mode;
	  return
	    build_pointer_type_for_mode
	      (TYPE_OBJECT_RECORD_TYPE (TYPE_UNCONSTRAINED_ARRAY (type)),
	       p_mode, 0);
	}
      break;

    case POINTER_TYPE:
      /* Only do something if this is a thin pointer, in which case we
	 may need to return the fat pointer.  */
      if (TYPE_IS_THIN_POINTER_P (type) && size >= POINTER_SIZE * 2)
	return
	  build_pointer_type (TYPE_UNCONSTRAINED_ARRAY (TREE_TYPE (type)));
      break;

    default:
      break;
    }

  return type;
}

/* ALIGNMENT is a Uint giving the alignment specified for GNAT_ENTITY,
   a type or object whose present alignment is ALIGN.  If this alignment is
   valid, return it.  Otherwise, give an error and return ALIGN.  */

static unsigned int
validate_alignment (Uint alignment, Entity_Id gnat_entity, unsigned int align)
{
  unsigned int max_allowed_alignment = get_target_maximum_allowed_alignment ();
  unsigned int new_align;
  Node_Id gnat_error_node;

  /* Don't worry about checking alignment if alignment was not specified
     by the source program and we already posted an error for this entity.  */
  if (Error_Posted (gnat_entity) && !Has_Alignment_Clause (gnat_entity))
    return align;

  /* Post the error on the alignment clause if any.  Note, for the implicit
     base type of an array type, the alignment clause is on the first
     subtype.  */
  if (Present (Alignment_Clause (gnat_entity)))
    gnat_error_node = Expression (Alignment_Clause (gnat_entity));

  else if (Is_Itype (gnat_entity)
           && Is_Array_Type (gnat_entity)
           && Etype (gnat_entity) == gnat_entity
           && Present (Alignment_Clause (First_Subtype (gnat_entity))))
    gnat_error_node =
      Expression (Alignment_Clause (First_Subtype (gnat_entity)));

  else
    gnat_error_node = gnat_entity;

  /* Within GCC, an alignment is an integer, so we must make sure a value is
     specified that fits in that range.  Also, there is an upper bound to
     alignments we can support/allow.  */
  if (!UI_Is_In_Int_Range (alignment)
      || ((new_align = UI_To_Int (alignment)) > max_allowed_alignment))
    post_error_ne_num ("largest supported alignment for& is ^",
		       gnat_error_node, gnat_entity, max_allowed_alignment);
  else if (!(Present (Alignment_Clause (gnat_entity))
	     && From_At_Mod (Alignment_Clause (gnat_entity)))
	   && new_align * BITS_PER_UNIT < align)
    {
      unsigned int double_align;
      bool is_capped_double, align_clause;

      /* If the default alignment of "double" or larger scalar types is
	 specifically capped and the new alignment is above the cap, do
	 not post an error and change the alignment only if there is an
	 alignment clause; this makes it possible to have the associated
	 GCC type overaligned by default for performance reasons.  */
      if ((double_align = double_float_alignment) > 0)
	{
	  Entity_Id gnat_type
	    = Is_Type (gnat_entity) ? gnat_entity : Etype (gnat_entity);
	  is_capped_double
	    = is_double_float_or_array (gnat_type, &align_clause);
	}
      else if ((double_align = double_scalar_alignment) > 0)
	{
	  Entity_Id gnat_type
	    = Is_Type (gnat_entity) ? gnat_entity : Etype (gnat_entity);
	  is_capped_double
	    = is_double_scalar_or_array (gnat_type, &align_clause);
	}
      else
	is_capped_double = align_clause = false;

      if (is_capped_double && new_align >= double_align)
	{
	  if (align_clause)
	    align = new_align * BITS_PER_UNIT;
	}
      else
	{
	  if (is_capped_double)
	    align = double_align * BITS_PER_UNIT;

	  post_error_ne_num ("alignment for& must be at least ^",
			     gnat_error_node, gnat_entity,
			     align / BITS_PER_UNIT);
	}
    }
  else
    {
      new_align = (new_align > 0 ? new_align * BITS_PER_UNIT : 1);
      if (new_align > align)
	align = new_align;
    }

  return align;
}

/* Return the smallest alignment not less than SIZE.  */

static unsigned int
ceil_alignment (unsigned HOST_WIDE_INT size)
{
  return (unsigned int) 1 << (floor_log2 (size - 1) + 1);
}

/* Verify that OBJECT, a type or decl, is something we can implement
   atomically.  If not, give an error for GNAT_ENTITY.  COMP_P is true
   if we require atomic components.  */

static void
check_ok_for_atomic (tree object, Entity_Id gnat_entity, bool comp_p)
{
  Node_Id gnat_error_point = gnat_entity;
  Node_Id gnat_node;
  enum machine_mode mode;
  unsigned int align;
  tree size;

  /* There are three case of what OBJECT can be.  It can be a type, in which
     case we take the size, alignment and mode from the type.  It can be a
     declaration that was indirect, in which case the relevant values are
     that of the type being pointed to, or it can be a normal declaration,
     in which case the values are of the decl.  The code below assumes that
     OBJECT is either a type or a decl.  */
  if (TYPE_P (object))
    {
      /* If this is an anonymous base type, nothing to check.  Error will be
	 reported on the source type.  */
      if (!Comes_From_Source (gnat_entity))
	return;

      mode = TYPE_MODE (object);
      align = TYPE_ALIGN (object);
      size = TYPE_SIZE (object);
    }
  else if (DECL_BY_REF_P (object))
    {
      mode = TYPE_MODE (TREE_TYPE (TREE_TYPE (object)));
      align = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (object)));
      size = TYPE_SIZE (TREE_TYPE (TREE_TYPE (object)));
    }
  else
    {
      mode = DECL_MODE (object);
      align = DECL_ALIGN (object);
      size = DECL_SIZE (object);
    }

  /* Consider all floating-point types atomic and any types that that are
     represented by integers no wider than a machine word.  */
  if (GET_MODE_CLASS (mode) == MODE_FLOAT
      || ((GET_MODE_CLASS (mode) == MODE_INT
	   || GET_MODE_CLASS (mode) == MODE_PARTIAL_INT)
	  && GET_MODE_BITSIZE (mode) <= BITS_PER_WORD))
    return;

  /* For the moment, also allow anything that has an alignment equal
     to its size and which is smaller than a word.  */
  if (size && TREE_CODE (size) == INTEGER_CST
      && compare_tree_int (size, align) == 0
      && align <= BITS_PER_WORD)
    return;

  for (gnat_node = First_Rep_Item (gnat_entity); Present (gnat_node);
       gnat_node = Next_Rep_Item (gnat_node))
    {
      if (!comp_p && Nkind (gnat_node) == N_Pragma
	  && (Get_Pragma_Id (Chars (Pragma_Identifier (gnat_node)))
              == Pragma_Atomic))
	gnat_error_point = First (Pragma_Argument_Associations (gnat_node));
      else if (comp_p && Nkind (gnat_node) == N_Pragma
	       && (Get_Pragma_Id (Chars (Pragma_Identifier (gnat_node)))
		   == Pragma_Atomic_Components))
	gnat_error_point = First (Pragma_Argument_Associations (gnat_node));
    }

  if (comp_p)
    post_error_ne ("atomic access to component of & cannot be guaranteed",
		   gnat_error_point, gnat_entity);
  else
    post_error_ne ("atomic access to & cannot be guaranteed",
		   gnat_error_point, gnat_entity);
}


/* Helper for the intrin compatibility checks family.  Evaluate whether
   two types are definitely incompatible.  */

static bool
intrin_types_incompatible_p (tree t1, tree t2)
{
  enum tree_code code;

  if (TYPE_MAIN_VARIANT (t1) == TYPE_MAIN_VARIANT (t2))
    return false;

  if (TYPE_MODE (t1) != TYPE_MODE (t2))
    return true;

  if (TREE_CODE (t1) != TREE_CODE (t2))
    return true;

  code = TREE_CODE (t1);

  switch (code)
    {
    case INTEGER_TYPE:
    case REAL_TYPE:
      return TYPE_PRECISION (t1) != TYPE_PRECISION (t2);

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      /* Assume designated types are ok.  We'd need to account for char * and
	 void * variants to do better, which could rapidly get messy and isn't
	 clearly worth the effort.  */
      return false;

    default:
      break;
    }

  return false;
}

/* Helper for intrin_profiles_compatible_p, to perform compatibility checks
   on the Ada/builtin argument lists for the INB binding.  */

static bool
intrin_arglists_compatible_p (intrin_binding_t * inb)
{
  function_args_iterator ada_iter, btin_iter;

  function_args_iter_init (&ada_iter, inb->ada_fntype);
  function_args_iter_init (&btin_iter, inb->btin_fntype);

  /* Sequence position of the last argument we checked.  */
  int argpos = 0;

  while (1)
    {
      tree ada_type = function_args_iter_cond (&ada_iter);
      tree btin_type = function_args_iter_cond (&btin_iter);

      /* If we've exhausted both lists simultaneously, we're done.  */
      if (ada_type == NULL_TREE && btin_type == NULL_TREE)
	break;

      /* If one list is shorter than the other, they fail to match.  */
      if (ada_type == NULL_TREE || btin_type == NULL_TREE)
	return false;

      /* If we're done with the Ada args and not with the internal builtin
	 args, or the other way around, complain.  */
      if (ada_type == void_type_node
	  && btin_type != void_type_node)
	{
	  post_error ("?Ada arguments list too short!", inb->gnat_entity);
	  return false;
	}

      if (btin_type == void_type_node
	  && ada_type != void_type_node)
	{
	  post_error_ne_num ("?Ada arguments list too long ('> ^)!",
			     inb->gnat_entity, inb->gnat_entity, argpos);
	  return false;
	}

      /* Otherwise, check that types match for the current argument.  */
      argpos ++;
      if (intrin_types_incompatible_p (ada_type, btin_type))
	{
	  post_error_ne_num ("?intrinsic binding type mismatch on argument ^!",
			     inb->gnat_entity, inb->gnat_entity, argpos);
	  return false;
	}


      function_args_iter_next (&ada_iter);
      function_args_iter_next (&btin_iter);
    }

  return true;
}

/* Helper for intrin_profiles_compatible_p, to perform compatibility checks
   on the Ada/builtin return values for the INB binding.  */

static bool
intrin_return_compatible_p (intrin_binding_t * inb)
{
  tree ada_return_type = TREE_TYPE (inb->ada_fntype);
  tree btin_return_type = TREE_TYPE (inb->btin_fntype);

  /* Accept function imported as procedure, common and convenient.  */
  if (VOID_TYPE_P (ada_return_type)
      && !VOID_TYPE_P (btin_return_type))
    return true;

  /* Check return types compatibility otherwise.  Note that this
     handles void/void as well.  */
  if (intrin_types_incompatible_p (btin_return_type, ada_return_type))
    {
      post_error ("?intrinsic binding type mismatch on return value!",
		  inb->gnat_entity);
      return false;
    }

  return true;
}

/* Check and return whether the Ada and gcc builtin profiles bound by INB are
   compatible.  Issue relevant warnings when they are not.

   This is intended as a light check to diagnose the most obvious cases, not
   as a full fledged type compatibility predicate.  It is the programmer's
   responsibility to ensure correctness of the Ada declarations in Imports,
   especially when binding straight to a compiler internal.  */

static bool
intrin_profiles_compatible_p (intrin_binding_t * inb)
{
  /* Check compatibility on return values and argument lists, each responsible
     for posting warnings as appropriate.  Ensure use of the proper sloc for
     this purpose.  */

  bool arglists_compatible_p, return_compatible_p;
  location_t saved_location = input_location;

  Sloc_to_locus (Sloc (inb->gnat_entity), &input_location);

  return_compatible_p = intrin_return_compatible_p (inb);
  arglists_compatible_p = intrin_arglists_compatible_p (inb);

  input_location = saved_location;

  return return_compatible_p && arglists_compatible_p;
}

/* Return a FIELD_DECL node modeled on OLD_FIELD.  FIELD_TYPE is its type
   and RECORD_TYPE is the type of the parent.  If SIZE is nonzero, it is the
   specified size for this field.  POS_LIST is a position list describing
   the layout of OLD_FIELD and SUBST_LIST a substitution list to be applied
   to this layout.  */

static tree
create_field_decl_from (tree old_field, tree field_type, tree record_type,
			tree size, tree pos_list,
			VEC(subst_pair,heap) *subst_list)
{
  tree t = TREE_VALUE (purpose_member (old_field, pos_list));
  tree pos = TREE_VEC_ELT (t, 0), bitpos = TREE_VEC_ELT (t, 2);
  unsigned int offset_align = tree_low_cst (TREE_VEC_ELT (t, 1), 1);
  tree new_pos, new_field;
  unsigned ix;
  subst_pair *s;

  if (CONTAINS_PLACEHOLDER_P (pos))
    FOR_EACH_VEC_ELT_REVERSE (subst_pair, subst_list, ix, s)
      pos = SUBSTITUTE_IN_EXPR (pos, s->discriminant, s->replacement);

  /* If the position is now a constant, we can set it as the position of the
     field when we make it.  Otherwise, we need to deal with it specially.  */
  if (TREE_CONSTANT (pos))
    new_pos = bit_from_pos (pos, bitpos);
  else
    new_pos = NULL_TREE;

  new_field
    = create_field_decl (DECL_NAME (old_field), field_type, record_type,
			 size, new_pos, DECL_PACKED (old_field),
			 !DECL_NONADDRESSABLE_P (old_field));

  if (!new_pos)
    {
      normalize_offset (&pos, &bitpos, offset_align);
      DECL_FIELD_OFFSET (new_field) = pos;
      DECL_FIELD_BIT_OFFSET (new_field) = bitpos;
      SET_DECL_OFFSET_ALIGN (new_field, offset_align);
      DECL_SIZE (new_field) = size;
      DECL_SIZE_UNIT (new_field)
	= convert (sizetype,
		   size_binop (CEIL_DIV_EXPR, size, bitsize_unit_node));
      layout_decl (new_field, DECL_OFFSET_ALIGN (new_field));
    }

  DECL_INTERNAL_P (new_field) = DECL_INTERNAL_P (old_field);
  SET_DECL_ORIGINAL_FIELD_TO_FIELD (new_field, old_field);
  DECL_DISCRIMINANT_NUMBER (new_field) = DECL_DISCRIMINANT_NUMBER (old_field);
  TREE_THIS_VOLATILE (new_field) = TREE_THIS_VOLATILE (old_field);

  return new_field;
}

/* Create the REP part of RECORD_TYPE with REP_TYPE.  If MIN_SIZE is nonzero,
   it is the minimal size the REP_PART must have.  */

static tree
create_rep_part (tree rep_type, tree record_type, tree min_size)
{
  tree field;

  if (min_size && !tree_int_cst_lt (TYPE_SIZE (rep_type), min_size))
    min_size = NULL_TREE;

  field = create_field_decl (get_identifier ("REP"), rep_type, record_type,
			     min_size, bitsize_zero_node, 0, 1);
  DECL_INTERNAL_P (field) = 1;

  return field;
}

/* Return the REP part of RECORD_TYPE, if any.  Otherwise return NULL.  */

static tree
get_rep_part (tree record_type)
{
  tree field = TYPE_FIELDS (record_type);

  /* The REP part is the first field, internal, another record, and its name
     starts with an 'R'.  */
  if (DECL_INTERNAL_P (field)
      && TREE_CODE (TREE_TYPE (field)) == RECORD_TYPE
      && IDENTIFIER_POINTER (DECL_NAME (field)) [0] == 'R')
    return field;

  return NULL_TREE;
}

/* Return the variant part of RECORD_TYPE, if any.  Otherwise return NULL.  */

tree
get_variant_part (tree record_type)
{
  tree field;

  /* The variant part is the only internal field that is a qualified union.  */
  for (field = TYPE_FIELDS (record_type); field; field = DECL_CHAIN (field))
    if (DECL_INTERNAL_P (field)
	&& TREE_CODE (TREE_TYPE (field)) == QUAL_UNION_TYPE)
      return field;

  return NULL_TREE;
}

/* Return a new variant part modeled on OLD_VARIANT_PART.  VARIANT_LIST is
   the list of variants to be used and RECORD_TYPE is the type of the parent.
   POS_LIST is a position list describing the layout of fields present in
   OLD_VARIANT_PART and SUBST_LIST a substitution list to be applied to this
   layout.  */

static tree
create_variant_part_from (tree old_variant_part,
			  VEC(variant_desc,heap) *variant_list,
			  tree record_type, tree pos_list,
			  VEC(subst_pair,heap) *subst_list)
{
  tree offset = DECL_FIELD_OFFSET (old_variant_part);
  tree old_union_type = TREE_TYPE (old_variant_part);
  tree new_union_type, new_variant_part;
  tree union_field_list = NULL_TREE;
  variant_desc *v;
  unsigned ix;

  /* First create the type of the variant part from that of the old one.  */
  new_union_type = make_node (QUAL_UNION_TYPE);
  TYPE_NAME (new_union_type) = DECL_NAME (TYPE_NAME (old_union_type));

  /* If the position of the variant part is constant, subtract it from the
     size of the type of the parent to get the new size.  This manual CSE
     reduces the code size when not optimizing.  */
  if (TREE_CODE (offset) == INTEGER_CST)
    {
      tree bitpos = DECL_FIELD_BIT_OFFSET (old_variant_part);
      tree first_bit = bit_from_pos (offset, bitpos);
      TYPE_SIZE (new_union_type)
	= size_binop (MINUS_EXPR, TYPE_SIZE (record_type), first_bit);
      TYPE_SIZE_UNIT (new_union_type)
	= size_binop (MINUS_EXPR, TYPE_SIZE_UNIT (record_type),
		      byte_from_pos (offset, bitpos));
      SET_TYPE_ADA_SIZE (new_union_type,
			 size_binop (MINUS_EXPR, TYPE_ADA_SIZE (record_type),
 				     first_bit));
      TYPE_ALIGN (new_union_type) = TYPE_ALIGN (old_union_type);
      relate_alias_sets (new_union_type, old_union_type, ALIAS_SET_COPY);
    }
  else
    copy_and_substitute_in_size (new_union_type, old_union_type, subst_list);

  /* Now finish up the new variants and populate the union type.  */
  FOR_EACH_VEC_ELT_REVERSE (variant_desc, variant_list, ix, v)
    {
      tree old_field = v->field, new_field;
      tree old_variant, old_variant_subpart, new_variant, field_list;

      /* Skip variants that don't belong to this nesting level.  */
      if (DECL_CONTEXT (old_field) != old_union_type)
	continue;

      /* Retrieve the list of fields already added to the new variant.  */
      new_variant = v->record;
      field_list = TYPE_FIELDS (new_variant);

      /* If the old variant had a variant subpart, we need to create a new
	 variant subpart and add it to the field list.  */
      old_variant = v->type;
      old_variant_subpart = get_variant_part (old_variant);
      if (old_variant_subpart)
	{
	  tree new_variant_subpart
	    = create_variant_part_from (old_variant_subpart, variant_list,
					new_variant, pos_list, subst_list);
	  DECL_CHAIN (new_variant_subpart) = field_list;
	  field_list = new_variant_subpart;
	}

      /* Finish up the new variant and create the field.  No need for debug
	 info thanks to the XVS type.  */
      finish_record_type (new_variant, nreverse (field_list), 2, false);
      compute_record_mode (new_variant);
      create_type_decl (TYPE_NAME (new_variant), new_variant, NULL,
			true, false, Empty);

      new_field
	= create_field_decl_from (old_field, new_variant, new_union_type,
				  TYPE_SIZE (new_variant),
				  pos_list, subst_list);
      DECL_QUALIFIER (new_field) = v->qual;
      DECL_INTERNAL_P (new_field) = 1;
      DECL_CHAIN (new_field) = union_field_list;
      union_field_list = new_field;
    }

  /* Finish up the union type and create the variant part.  No need for debug
     info thanks to the XVS type.  */
  finish_record_type (new_union_type, union_field_list, 2, false);
  compute_record_mode (new_union_type);
  create_type_decl (TYPE_NAME (new_union_type), new_union_type, NULL,
		    true, false, Empty);

  new_variant_part
    = create_field_decl_from (old_variant_part, new_union_type, record_type,
			      TYPE_SIZE (new_union_type),
			      pos_list, subst_list);
  DECL_INTERNAL_P (new_variant_part) = 1;

  /* With multiple discriminants it is possible for an inner variant to be
     statically selected while outer ones are not; in this case, the list
     of fields of the inner variant is not flattened and we end up with a
     qualified union with a single member.  Drop the useless container.  */
  if (!DECL_CHAIN (union_field_list))
    {
      DECL_CONTEXT (union_field_list) = record_type;
      DECL_FIELD_OFFSET (union_field_list)
	= DECL_FIELD_OFFSET (new_variant_part);
      DECL_FIELD_BIT_OFFSET (union_field_list)
	= DECL_FIELD_BIT_OFFSET (new_variant_part);
      SET_DECL_OFFSET_ALIGN (union_field_list,
			     DECL_OFFSET_ALIGN (new_variant_part));
      new_variant_part = union_field_list;
    }

  return new_variant_part;
}

/* Copy the size (and alignment and alias set) from OLD_TYPE to NEW_TYPE,
   which are both RECORD_TYPE, after applying the substitutions described
   in SUBST_LIST.  */

static void
copy_and_substitute_in_size (tree new_type, tree old_type,
			     VEC(subst_pair,heap) *subst_list)
{
  unsigned ix;
  subst_pair *s;

  TYPE_SIZE (new_type) = TYPE_SIZE (old_type);
  TYPE_SIZE_UNIT (new_type) = TYPE_SIZE_UNIT (old_type);
  SET_TYPE_ADA_SIZE (new_type, TYPE_ADA_SIZE (old_type));
  TYPE_ALIGN (new_type) = TYPE_ALIGN (old_type);
  relate_alias_sets (new_type, old_type, ALIAS_SET_COPY);

  if (CONTAINS_PLACEHOLDER_P (TYPE_SIZE (new_type)))
    FOR_EACH_VEC_ELT_REVERSE (subst_pair, subst_list, ix, s)
      TYPE_SIZE (new_type)
	= SUBSTITUTE_IN_EXPR (TYPE_SIZE (new_type),
			      s->discriminant, s->replacement);

  if (CONTAINS_PLACEHOLDER_P (TYPE_SIZE_UNIT (new_type)))
    FOR_EACH_VEC_ELT_REVERSE (subst_pair, subst_list, ix, s)
      TYPE_SIZE_UNIT (new_type)
	= SUBSTITUTE_IN_EXPR (TYPE_SIZE_UNIT (new_type),
			      s->discriminant, s->replacement);

  if (CONTAINS_PLACEHOLDER_P (TYPE_ADA_SIZE (new_type)))
    FOR_EACH_VEC_ELT_REVERSE (subst_pair, subst_list, ix, s)
      SET_TYPE_ADA_SIZE
	(new_type, SUBSTITUTE_IN_EXPR (TYPE_ADA_SIZE (new_type),
				       s->discriminant, s->replacement));

  /* Finalize the size.  */
  TYPE_SIZE (new_type) = variable_size (TYPE_SIZE (new_type));
  TYPE_SIZE_UNIT (new_type) = variable_size (TYPE_SIZE_UNIT (new_type));
}

/* Given a type T, a FIELD_DECL F, and a replacement value R, return a
   type with all size expressions that contain F in a PLACEHOLDER_EXPR
   updated by replacing F with R.

   The function doesn't update the layout of the type, i.e. it assumes
   that the substitution is purely formal.  That's why the replacement
   value R must itself contain a PLACEHOLDER_EXPR.  */

tree
substitute_in_type (tree t, tree f, tree r)
{
  tree nt;

  gcc_assert (CONTAINS_PLACEHOLDER_P (r));

  switch (TREE_CODE (t))
    {
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case REAL_TYPE:

      /* First the domain types of arrays.  */
      if (CONTAINS_PLACEHOLDER_P (TYPE_GCC_MIN_VALUE (t))
	  || CONTAINS_PLACEHOLDER_P (TYPE_GCC_MAX_VALUE (t)))
	{
	  tree low = SUBSTITUTE_IN_EXPR (TYPE_GCC_MIN_VALUE (t), f, r);
	  tree high = SUBSTITUTE_IN_EXPR (TYPE_GCC_MAX_VALUE (t), f, r);

	  if (low == TYPE_GCC_MIN_VALUE (t) && high == TYPE_GCC_MAX_VALUE (t))
	    return t;

	  nt = copy_type (t);
	  TYPE_GCC_MIN_VALUE (nt) = low;
	  TYPE_GCC_MAX_VALUE (nt) = high;

	  if (TREE_CODE (t) == INTEGER_TYPE && TYPE_INDEX_TYPE (t))
	    SET_TYPE_INDEX_TYPE
	      (nt, substitute_in_type (TYPE_INDEX_TYPE (t), f, r));

	  return nt;
	}

      /* Then the subtypes.  */
      if (CONTAINS_PLACEHOLDER_P (TYPE_RM_MIN_VALUE (t))
	  || CONTAINS_PLACEHOLDER_P (TYPE_RM_MAX_VALUE (t)))
	{
	  tree low = SUBSTITUTE_IN_EXPR (TYPE_RM_MIN_VALUE (t), f, r);
	  tree high = SUBSTITUTE_IN_EXPR (TYPE_RM_MAX_VALUE (t), f, r);

	  if (low == TYPE_RM_MIN_VALUE (t) && high == TYPE_RM_MAX_VALUE (t))
	    return t;

	  nt = copy_type (t);
	  SET_TYPE_RM_MIN_VALUE (nt, low);
	  SET_TYPE_RM_MAX_VALUE (nt, high);

	  return nt;
	}

      return t;

    case COMPLEX_TYPE:
      nt = substitute_in_type (TREE_TYPE (t), f, r);
      if (nt == TREE_TYPE (t))
	return t;

      return build_complex_type (nt);

    case FUNCTION_TYPE:
      /* These should never show up here.  */
      gcc_unreachable ();

    case ARRAY_TYPE:
      {
	tree component = substitute_in_type (TREE_TYPE (t), f, r);
	tree domain = substitute_in_type (TYPE_DOMAIN (t), f, r);

	if (component == TREE_TYPE (t) && domain == TYPE_DOMAIN (t))
	  return t;

	nt = build_nonshared_array_type (component, domain);
	TYPE_ALIGN (nt) = TYPE_ALIGN (t);
	TYPE_USER_ALIGN (nt) = TYPE_USER_ALIGN (t);
	SET_TYPE_MODE (nt, TYPE_MODE (t));
	TYPE_SIZE (nt) = SUBSTITUTE_IN_EXPR (TYPE_SIZE (t), f, r);
	TYPE_SIZE_UNIT (nt) = SUBSTITUTE_IN_EXPR (TYPE_SIZE_UNIT (t), f, r);
	TYPE_NONALIASED_COMPONENT (nt) = TYPE_NONALIASED_COMPONENT (t);
	TYPE_MULTI_ARRAY_P (nt) = TYPE_MULTI_ARRAY_P (t);
	TYPE_CONVENTION_FORTRAN_P (nt) = TYPE_CONVENTION_FORTRAN_P (t);
	return nt;
      }

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	bool changed_field = false;
	tree field;

	/* Start out with no fields, make new fields, and chain them
	   in.  If we haven't actually changed the type of any field,
	   discard everything we've done and return the old type.  */
	nt = copy_type (t);
	TYPE_FIELDS (nt) = NULL_TREE;

	for (field = TYPE_FIELDS (t); field; field = DECL_CHAIN (field))
	  {
	    tree new_field = copy_node (field), new_n;

	    new_n = substitute_in_type (TREE_TYPE (field), f, r);
	    if (new_n != TREE_TYPE (field))
	      {
		TREE_TYPE (new_field) = new_n;
		changed_field = true;
	      }

	    new_n = SUBSTITUTE_IN_EXPR (DECL_FIELD_OFFSET (field), f, r);
	    if (new_n != DECL_FIELD_OFFSET (field))
	      {
		DECL_FIELD_OFFSET (new_field) = new_n;
		changed_field = true;
	      }

	    /* Do the substitution inside the qualifier, if any.  */
	    if (TREE_CODE (t) == QUAL_UNION_TYPE)
	      {
		new_n = SUBSTITUTE_IN_EXPR (DECL_QUALIFIER (field), f, r);
		if (new_n != DECL_QUALIFIER (field))
		  {
		    DECL_QUALIFIER (new_field) = new_n;
		    changed_field = true;
		  }
	      }

	    DECL_CONTEXT (new_field) = nt;
	    SET_DECL_ORIGINAL_FIELD_TO_FIELD (new_field, field);

	    DECL_CHAIN (new_field) = TYPE_FIELDS (nt);
	    TYPE_FIELDS (nt) = new_field;
	  }

	if (!changed_field)
	  return t;

	TYPE_FIELDS (nt) = nreverse (TYPE_FIELDS (nt));
	TYPE_SIZE (nt) = SUBSTITUTE_IN_EXPR (TYPE_SIZE (t), f, r);
	TYPE_SIZE_UNIT (nt) = SUBSTITUTE_IN_EXPR (TYPE_SIZE_UNIT (t), f, r);
	SET_TYPE_ADA_SIZE (nt, SUBSTITUTE_IN_EXPR (TYPE_ADA_SIZE (t), f, r));
	return nt;
      }

    default:
      return t;
    }
}

/* Return the RM size of GNU_TYPE.  This is the actual number of bits
   needed to represent the object.  */

tree
rm_size (tree gnu_type)
{
  /* For integral types, we store the RM size explicitly.  */
  if (INTEGRAL_TYPE_P (gnu_type) && TYPE_RM_SIZE (gnu_type))
    return TYPE_RM_SIZE (gnu_type);

  /* Return the RM size of the actual data plus the size of the template.  */
  if (TREE_CODE (gnu_type) == RECORD_TYPE
      && TYPE_CONTAINS_TEMPLATE_P (gnu_type))
    return
      size_binop (PLUS_EXPR,
		  rm_size (TREE_TYPE (DECL_CHAIN (TYPE_FIELDS (gnu_type)))),
		  DECL_SIZE (TYPE_FIELDS (gnu_type)));

  /* For record or union types, we store the size explicitly.  */
  if (RECORD_OR_UNION_TYPE_P (gnu_type)
      && !TYPE_FAT_POINTER_P (gnu_type)
      && TYPE_ADA_SIZE (gnu_type))
    return TYPE_ADA_SIZE (gnu_type);

  /* For other types, this is just the size.  */
  return TYPE_SIZE (gnu_type);
}

/* Return the name to be used for GNAT_ENTITY.  If a type, create a
   fully-qualified name, possibly with type information encoding.
   Otherwise, return the name.  */

tree
get_entity_name (Entity_Id gnat_entity)
{
  Get_Encoded_Name (gnat_entity);
  return get_identifier_with_length (Name_Buffer, Name_Len);
}

/* Return an identifier representing the external name to be used for
   GNAT_ENTITY.  If SUFFIX is specified, the name is followed by "___"
   and the specified suffix.  */

tree
create_concat_name (Entity_Id gnat_entity, const char *suffix)
{
  Entity_Kind kind = Ekind (gnat_entity);

  if (suffix)
    {
      String_Template temp = {1, (int) strlen (suffix)};
      Fat_Pointer fp = {suffix, &temp};
      Get_External_Name_With_Suffix (gnat_entity, fp);
    }
  else
    Get_External_Name (gnat_entity, 0);

  /* A variable using the Stdcall convention lives in a DLL.  We adjust
     its name to use the jump table, the _imp__NAME contains the address
     for the NAME variable.  */
  if ((kind == E_Variable || kind == E_Constant)
      && Has_Stdcall_Convention (gnat_entity))
    {
      const int len = 6 + Name_Len;
      char *new_name = (char *) alloca (len + 1);
      strcpy (new_name, "_imp__");
      strcat (new_name, Name_Buffer);
      return get_identifier_with_length (new_name, len);
    }

  return get_identifier_with_length (Name_Buffer, Name_Len);
}

/* Given GNU_NAME, an IDENTIFIER_NODE containing a name and SUFFIX, a
   string, return a new IDENTIFIER_NODE that is the concatenation of
   the name followed by "___" and the specified suffix.  */

tree
concat_name (tree gnu_name, const char *suffix)
{
  const int len = IDENTIFIER_LENGTH (gnu_name) + 3 + strlen (suffix);
  char *new_name = (char *) alloca (len + 1);
  strcpy (new_name, IDENTIFIER_POINTER (gnu_name));
  strcat (new_name, "___");
  strcat (new_name, suffix);
  return get_identifier_with_length (new_name, len);
}

#include "gt-ada-decl.h"
