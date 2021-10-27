/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                 D E C L                                  *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2021, Free Software Foundation, Inc.         *
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
#include "target.h"
#include "tree.h"
#include "gimple-expr.h"
#include "stringpool.h"
#include "diagnostic-core.h"
#include "alias.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "tree-inline.h"
#include "demangle.h"

#include "ada.h"
#include "types.h"
#include "atree.h"
#include "elists.h"
#include "namet.h"
#include "nlists.h"
#include "repinfo.h"
#include "snames.h"
#include "uintp.h"
#include "urealp.h"
#include "fe.h"
#include "sinfo.h"
#include "einfo.h"
#include "ada-tree.h"
#include "gigi.h"

/* The "stdcall" convention is really supported on 32-bit x86/Windows only.
   The following macro is a helper to avoid having to check for a Windows
   specific attribute throughout this unit.  */

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

#define STDCALL_PREFIX "_imp__"

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

/* The largest TYPE_ARRAY_MAX_SIZE value we set on an array type.
   It's an artibrary limit (256 MB) above which we consider that
   the allocation is essentially unbounded.  */

#define TYPE_ARRAY_SIZE_LIMIT (1 << 28)

struct incomplete
{
  struct incomplete *next;
  tree old_type;
  Entity_Id full_type;
};

/* These variables are used to defer recursively expanding incomplete types
   while we are processing a record, an array or a subprogram type.  */
static int defer_incomplete_level = 0;
static struct incomplete *defer_incomplete_list;

/* This variable is used to delay expanding types coming from a limited with
   clause and completed Taft Amendment types until the end of the spec.  */
static struct incomplete *defer_limited_with_list;

typedef struct subst_pair_d {
  tree discriminant;
  tree replacement;
} subst_pair;


typedef struct variant_desc_d {
  /* The type of the variant.  */
  tree type;

  /* The associated field.  */
  tree field;

  /* The value of the qualifier.  */
  tree qual;

  /* The type of the variant after transformation.  */
  tree new_type;

  /* The auxiliary data.  */
  tree aux;
} variant_desc;


/* A map used to cache the result of annotate_value.  */
struct value_annotation_hasher : ggc_cache_ptr_hash<tree_int_map>
{
  static inline hashval_t
  hash (tree_int_map *m)
  {
    return htab_hash_pointer (m->base.from);
  }

  static inline bool
  equal (tree_int_map *a, tree_int_map *b)
  {
    return a->base.from == b->base.from;
  }

  static int
  keep_cache_entry (tree_int_map *&m)
  {
    return ggc_marked_p (m->base.from);
  }
};

static GTY ((cache)) hash_table<value_annotation_hasher> *annotate_value_cache;

/* A map used to associate a dummy type with a list of subprogram entities.  */
struct GTY((for_user)) tree_entity_vec_map
{
  struct tree_map_base base;
  vec<Entity_Id, va_gc_atomic> *to;
};

void
gt_pch_nx (Entity_Id &)
{
}

void
gt_pch_nx (Entity_Id *x, gt_pointer_operator op, void *cookie)
{
  op (x, cookie);
}

struct dummy_type_hasher : ggc_cache_ptr_hash<tree_entity_vec_map>
{
  static inline hashval_t
  hash (tree_entity_vec_map *m)
  {
    return htab_hash_pointer (m->base.from);
  }

  static inline bool
  equal (tree_entity_vec_map *a, tree_entity_vec_map *b)
  {
    return a->base.from == b->base.from;
  }

  static int
  keep_cache_entry (tree_entity_vec_map *&m)
  {
    return ggc_marked_p (m->base.from);
  }
};

static GTY ((cache)) hash_table<dummy_type_hasher> *dummy_to_subprog_map;

static void prepend_one_attribute (struct attrib **,
				   enum attrib_type, tree, tree, Node_Id);
static void prepend_one_attribute_pragma (struct attrib **, Node_Id);
static void prepend_attributes (struct attrib **, Entity_Id);
static tree elaborate_expression (Node_Id, Entity_Id, const char *, bool, bool,
				  bool);
static tree elaborate_expression_1 (tree, Entity_Id, const char *, bool, bool);
static tree elaborate_expression_2 (tree, Entity_Id, const char *, bool, bool,
				    unsigned int);
static tree elaborate_reference (tree, Entity_Id, bool, tree *);
static tree gnat_to_gnu_component_type (Entity_Id, bool, bool);
static tree gnat_to_gnu_subprog_type (Entity_Id, bool, bool, tree *);
static int adjust_packed (tree, tree, int);
static tree gnat_to_gnu_field (Entity_Id, tree, int, bool, bool);
static enum inline_status_t inline_status_for_subprog (Entity_Id);
static tree gnu_ext_name_for_subprog (Entity_Id, tree);
static void set_nonaliased_component_on_array_type (tree);
static void set_reverse_storage_order_on_array_type (tree);
static bool same_discriminant_p (Entity_Id, Entity_Id);
static bool array_type_has_nonaliased_component (tree, Entity_Id);
static bool compile_time_known_address_p (Node_Id);
static bool flb_cannot_be_superflat (Node_Id);
static bool range_cannot_be_superflat (Node_Id);
static bool constructor_address_p (tree);
static bool allocatable_size_p (tree, bool);
static bool initial_value_needs_conversion (tree, tree);
static tree update_n_elem (tree, tree, tree);
static int compare_field_bitpos (const PTR, const PTR);
static bool components_to_record (Node_Id, Entity_Id, tree, tree, int, bool,
				  bool, bool, bool, bool, bool, bool, tree,
				  tree *);
static Uint annotate_value (tree);
static void annotate_rep (Entity_Id, tree);
static tree build_position_list (tree, bool, tree, tree, unsigned int, tree);
static vec<subst_pair> build_subst_list (Entity_Id, Entity_Id, bool);
static vec<variant_desc> build_variant_list (tree, Node_Id, vec<subst_pair>,
					     vec<variant_desc>);
static tree maybe_saturate_size (tree, unsigned int align);
static tree validate_size (Uint, tree, Entity_Id, enum tree_code, bool, bool,
			   const char *, const char *);
static void set_rm_size (Uint, tree, Entity_Id);
static unsigned int validate_alignment (Uint, Entity_Id, unsigned int);
static unsigned int promote_object_alignment (tree, tree, Entity_Id);
static void check_ok_for_atomic_type (tree, Entity_Id, bool);
static bool type_for_atomic_builtin_p (tree);
static tree resolve_atomic_builtin (enum built_in_function, tree);
static tree create_field_decl_from (tree, tree, tree, tree, tree,
				    vec<subst_pair>);
static tree create_rep_part (tree, tree, tree);
static tree get_rep_part (tree);
static tree create_variant_part_from (tree, vec<variant_desc>, tree,
				      tree, vec<subst_pair>, bool);
static void copy_and_substitute_in_size (tree, tree, vec<subst_pair>);
static void copy_and_substitute_in_layout (Entity_Id, Entity_Id, tree, tree,
					   vec<subst_pair>, bool);
static tree associate_original_type_to_packed_array (tree, Entity_Id);
static const char *get_entity_char (Entity_Id);

/* The relevant constituents of a subprogram binding to a GCC builtin.  Used
   to pass around calls performing profile compatibility checks.  */

typedef struct {
  Entity_Id gnat_entity;  /* The Ada subprogram entity.  */
  tree ada_fntype;        /* The corresponding GCC type node.  */
  tree btin_fntype;       /* The GCC builtin function type node.  */
} intrin_binding_t;

static bool intrin_profiles_compatible_p (const intrin_binding_t *);

/* Given GNAT_ENTITY, a GNAT defining identifier node, which denotes some Ada
   entity, return the equivalent GCC tree for that entity (a ..._DECL node)
   and associate the ..._DECL node with the input GNAT defining identifier.

   If GNAT_ENTITY is a variable or a constant declaration, GNU_EXPR gives its
   initial value (in GCC tree form).  This is optional for a variable.  For
   a renamed entity, GNU_EXPR gives the object being renamed.

   DEFINITION is true if this call is intended for a definition.  This is used
   for separate compilation where it is necessary to know whether an external
   declaration or a definition must be created if the GCC equivalent was not
   created previously.  */

tree
gnat_to_gnu_entity (Entity_Id gnat_entity, tree gnu_expr, bool definition)
{
  /* The construct that declared the entity.  */
  const Node_Id gnat_decl = Declaration_Node (gnat_entity);
  /* The object that the entity renames, if any.  */
  const Entity_Id gnat_renamed_obj = Renamed_Object (gnat_entity);
  /* The kind of the entity.  */
  const Entity_Kind kind = Ekind (gnat_entity);
  /* True if this is a type.  */
  const bool is_type = IN (kind, Type_Kind);
  /* True if this is an artificial entity.  */
  const bool artificial_p = !Comes_From_Source (gnat_entity);
  /* True if debug info is requested for this entity.  */
  const bool debug_info_p = Needs_Debug_Info (gnat_entity);
  /* True if this entity is to be considered as imported.  */
  const bool imported_p
    = (Is_Imported (gnat_entity) && No (Address_Clause (gnat_entity)));
  /* True if this entity has a foreign convention.  */
  const bool foreign = Has_Foreign_Convention (gnat_entity);
  /* For a type, contains the equivalent GNAT node to be used in gigi.  */
  Entity_Id gnat_equiv_type = Empty;
  /* For a type, contains the GNAT node to be used for back-annotation.  */
  Entity_Id gnat_annotate_type = Empty;
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
  /* True if we have already saved gnu_decl as a GNAT association.  This can
     also be used to purposely avoid making such an association but this use
     case ought not to be applied to types because it can break the deferral
     mechanism implemented for access types.  */
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

  /* Since a use of an itype is a definition, process it as such if it is in
     the main unit, except for E_Access_Subtype because it's actually a use
     of its base type, see below.  */
  if (!definition
      && is_type
      && Is_Itype (gnat_entity)
      && Ekind (gnat_entity) != E_Access_Subtype
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

	  if (Is_Subprogram (gnat_temp)
	      && Present (Protected_Body_Subprogram (gnat_temp)))
	    gnat_temp = Protected_Body_Subprogram (gnat_temp);

	  if (Ekind (gnat_temp) == E_Entry
	      || Ekind (gnat_temp) == E_Entry_Family
	      || Ekind (gnat_temp) == E_Task_Type
	      || (Is_Subprogram (gnat_temp)
		  && present_gnu_tree (gnat_temp)
		  && (current_function_decl
		      == gnat_to_gnu_entity (gnat_temp, NULL_TREE, false))))
	    {
	      process_type (gnat_entity);
	      return get_gnu_tree (gnat_entity);
	    }
	}

      /* This abort means the itype has an incorrect scope, i.e. that its
	 scope does not correspond to the subprogram it is first used in.  */
      gcc_unreachable ();
    }

  /* If we've already processed this entity, return what we got last time.
     If we are defining the node, we should not have already processed it.
     In that case, we will abort below when we try to save a new GCC tree
     for this object.  We also need to handle the case of getting a dummy
     type when a Full_View exists but be careful so as not to trigger its
     premature elaboration.  Likewise for a cloned subtype without its own
     freeze node, which typically happens when a generic gets instantiated
     on an incomplete or private type.  */
  if ((!definition || (is_type && imported_p))
      && present_gnu_tree (gnat_entity))
    {
      gnu_decl = get_gnu_tree (gnat_entity);

      if (TREE_CODE (gnu_decl) == TYPE_DECL
	  && TYPE_IS_DUMMY_P (TREE_TYPE (gnu_decl))
	  && IN (kind, Incomplete_Or_Private_Kind)
	  && Present (Full_View (gnat_entity))
	  && (present_gnu_tree (Full_View (gnat_entity))
	      || No (Freeze_Node (Full_View (gnat_entity)))))
	{
	  gnu_decl
	    = gnat_to_gnu_entity (Full_View (gnat_entity), NULL_TREE,
				  false);
	  save_gnu_tree (gnat_entity, NULL_TREE, false);
	  save_gnu_tree (gnat_entity, gnu_decl, false);
	}

      if (TREE_CODE (gnu_decl) == TYPE_DECL
	  && TYPE_IS_DUMMY_P (TREE_TYPE (gnu_decl))
	  && Ekind (gnat_entity) == E_Record_Subtype
	  && No (Freeze_Node (gnat_entity))
	  && Present (Cloned_Subtype (gnat_entity))
	  && (present_gnu_tree (Cloned_Subtype (gnat_entity))
	      || No (Freeze_Node (Cloned_Subtype (gnat_entity)))))
	{
	  gnu_decl
	    = gnat_to_gnu_entity (Cloned_Subtype (gnat_entity), NULL_TREE,
				  false);
	  save_gnu_tree (gnat_entity, NULL_TREE, false);
	  save_gnu_tree (gnat_entity, gnu_decl, false);
	}

      return gnu_decl;
    }

  /* If this is a numeric or enumeral type, or an access type, a nonzero Esize
     must be specified unless it was specified by the programmer.  Exceptions
     are for access-to-protected-subprogram types and all access subtypes, as
     another GNAT type is used to lay out the GCC type for them.  */
  gcc_assert (!is_type
	      || Known_Esize (gnat_entity)
	      || Has_Size_Clause (gnat_entity)
	      || (!Is_In_Numeric_Kind (kind)
		  && !IN (kind, Enumeration_Kind)
		  && (!IN (kind, Access_Kind)
		      || kind == E_Access_Protected_Subprogram_Type
		      || kind == E_Anonymous_Access_Protected_Subprogram_Type
		      || kind == E_Access_Subtype
		      || type_annotate_only)));

  /* The RM size must be specified for all discrete and fixed-point types.  */
  gcc_assert (!(Is_In_Discrete_Or_Fixed_Point_Kind (kind)
		&& !Known_RM_Size (gnat_entity)));

  /* If we get here, it means we have not yet done anything with this entity.
     If we are not defining it, it must be a type or an entity that is defined
     elsewhere or externally, otherwise we should have defined it already.

     In other words, the failure of this assertion typically arises when a
     reference to an entity (type or object) is made before its declaration,
     either directly or by means of a freeze node which is incorrectly placed.
     This can also happen for an entity referenced out of context, for example
     a parameter outside of the subprogram where it is declared.  GNAT_ENTITY
     is the N_Defining_Identifier of the entity, the problematic N_Identifier
     being the argument passed to Identifier_to_gnu in the parent frame.

     One exception is for an entity, typically an inherited operation, which is
     a local alias for the parent's operation.  It is neither defined, since it
     is an inherited operation, nor public, since it is declared in the current
     compilation unit, so we test Is_Public on the Alias entity instead.  */
  gcc_assert (definition
	      || is_type
	      || kind == E_Discriminant
	      || kind == E_Component
	      || kind == E_Label
	      || (kind == E_Constant && Present (Full_View (gnat_entity)))
	      || Is_Public (gnat_entity)
	      || (Present (Alias (gnat_entity))
		  && Is_Public (Alias (gnat_entity)))
	      || type_annotate_only);

  /* Get the name of the entity and set up the line number and filename of
     the original definition for use in any decl we make.  Make sure we do
     not inherit another source location.  */
  gnu_entity_name = get_entity_name (gnat_entity);
  if (!renaming_from_instantiation_p (gnat_entity))
    Sloc_to_locus (Sloc (gnat_entity), &input_location);

  /* For cases when we are not defining (i.e., we are referencing from
     another compilation unit) public entities, show we are at global level
     for the purpose of computing scopes.  Don't do this for components or
     discriminants since the relevant test is whether or not the record is
     being defined.  */
  if (!definition
      && kind != E_Component
      && kind != E_Discriminant
      && Is_Public (gnat_entity)
      && !Is_Statically_Allocated (gnat_entity))
    force_global++, this_global = true;

  /* Handle any attributes directly attached to the entity.  */
  if (Has_Gigi_Rep_Item (gnat_entity))
    prepend_attributes (&attr_list, gnat_entity);

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
	prepend_attributes (&attr_list,
			    First_Subtype (Base_Type (gnat_entity)));

      /* Compute a default value for the size of an elementary type.  */
      if (Known_Esize (gnat_entity) && Is_Elementary_Type (gnat_entity))
	{
	  unsigned int max_esize;

	  gcc_assert (UI_Is_In_Int_Range (Esize (gnat_entity)));
	  esize = UI_To_Int (Esize (gnat_entity));

	  if (IN (kind, Float_Kind))
	    max_esize = fp_prec_to_size (LONG_DOUBLE_TYPE_SIZE);
	  else if (IN (kind, Access_Kind))
	    max_esize = POINTER_SIZE * 2;
	  else
	    max_esize = Enable_128bit_Types ? 128 : LONG_LONG_TYPE_SIZE;

	  if (esize > max_esize)
	   esize = max_esize;
	}
    }

  switch (kind)
    {
    case E_Component:
    case E_Discriminant:
      {
	/* The GNAT record where the component was defined.  */
	Entity_Id gnat_record = Underlying_Type (Scope (gnat_entity));

	/* If the entity is a discriminant of an extended tagged type used to
	   rename a discriminant of the parent type, return the latter.  */
	if (kind == E_Discriminant
	    && Present (Corresponding_Discriminant (gnat_entity))
	    && Is_Tagged_Type (gnat_record))
	  {
	    gnu_decl
	      = gnat_to_gnu_entity (Corresponding_Discriminant (gnat_entity),
				    gnu_expr, definition);
	    saved = true;
	    break;
	  }

	/* If the entity is an inherited component (in the case of extended
	   tagged record types), just return the original entity, which must
	   be a FIELD_DECL.  Likewise for discriminants.  If the entity is a
	   non-stored discriminant (in the case of derived untagged record
	   types), return the stored discriminant it renames.  */
	if (Present (Original_Record_Component (gnat_entity))
	    && Original_Record_Component (gnat_entity) != gnat_entity)
	  {
	    gnu_decl
	      = gnat_to_gnu_entity (Original_Record_Component (gnat_entity),
				    gnu_expr, definition);
	    /* GNU_DECL contains a PLACEHOLDER_EXPR for discriminants.  */
	    if (kind == E_Discriminant)
	      saved = true;
	    break;
	  }

	/* Otherwise, if we are not defining this and we have no GCC type
	   for the containing record, make one for it.  Then we should
	   have made our own equivalent.  */
	if (!definition && !present_gnu_tree (gnat_record))
	  {
	    /* ??? If this is in a record whose scope is a protected
	       type and we have an Original_Record_Component, use it.
	       This is a workaround for major problems in protected type
	       handling.  */
	    Entity_Id Scop = Scope (Scope (gnat_entity));
	    if (Is_Protected_Type (Underlying_Type (Scop))
		&& Present (Original_Record_Component (gnat_entity)))
	      {
		gnu_decl
		  = gnat_to_gnu_entity (Original_Record_Component
					(gnat_entity),
					gnu_expr, false);
	      }
	    else
	      {
		gnat_to_gnu_entity (Scope (gnat_entity), NULL_TREE, false);
		gnu_decl = get_gnu_tree (gnat_entity);
	      }

	    saved = true;
	    break;
	  }

	/* Here we have no GCC type and this is a reference rather than a
	   definition.  This should never happen.  Most likely the cause is
	   reference before declaration in the GNAT tree for gnat_entity.  */
	gcc_unreachable ();
      }

    case E_Named_Integer:
    case E_Named_Real:
      {
	tree gnu_ext_name = NULL_TREE;

	if (Is_Public (gnat_entity))
	  gnu_ext_name = create_concat_name (gnat_entity, NULL);

	/* All references are supposed to be folded in the front-end.  */
	gcc_assert (definition && gnu_expr);

	gnu_type = gnat_to_gnu_type (Etype (gnat_entity));
	gnu_expr = convert (gnu_type, gnu_expr);

	/* Build a CONST_DECL for debugging purposes exclusively.  */
	gnu_decl
	  = create_var_decl (gnu_entity_name, gnu_ext_name, gnu_type,
			     gnu_expr, true, Is_Public (gnat_entity),
			     false, false, false, artificial_p,
			     debug_info_p, NULL, gnat_entity);
      }
      break;

    case E_Constant:
      /* Ignore constant definitions already marked with the error node.  See
	 the N_Object_Declaration case of gnat_to_gnu for the rationale.  */
      if (definition
	  && present_gnu_tree (gnat_entity)
	  && get_gnu_tree (gnat_entity) == error_mark_node)
	{
	  maybe_present = true;
	  break;
	}

      /* Ignore deferred constant definitions without address clause since
	 they are processed fully in the front-end.  If No_Initialization
	 is set, this is not a deferred constant but a constant whose value
	 is built manually.  And constants that are renamings are handled
	 like variables.  */
      if (definition
	  && !gnu_expr
	  && No (Address_Clause (gnat_entity))
	  && !No_Initialization (gnat_decl)
	  && No (gnat_renamed_obj))
	{
	  gnu_decl = error_mark_node;
	  saved = true;
	  break;
	}

      /* If this is a use of a deferred constant without address clause,
	 get its full definition.  */
      if (!definition
	  && No (Address_Clause (gnat_entity))
	  && Present (Full_View (gnat_entity)))
	{
	  gnu_decl
	    = gnat_to_gnu_entity (Full_View (gnat_entity), gnu_expr, false);
	  saved = true;
	  break;
	}

      /* If we have a constant that we are not defining, get the expression it
	 was defined to represent.  This is necessary to avoid generating dumb
	 elaboration code in simple cases, and we may throw it away later if it
	 is not a constant.  But do not do it for dispatch tables because they
	 are only referenced indirectly and we need to have a consistent view
	 of the exported and of the imported declarations of the tables from
	 external units for them to be properly merged in LTO mode.  Moreover
	 simply do not retrieve the expression if it is an allocator because
	 the designated type might still be dummy at this point.  Note that we
	 invoke gnat_to_gnu_external and not gnat_to_gnu because the expression
	 may contain N_Expression_With_Actions nodes and thus declarations of
	 objects from other units that we need to discard.  Note also that we
	 need to do it even if we are only annotating types, so as to be able
	 to validate representation clauses using constants.  */
      if (!definition
	  && !No_Initialization (gnat_decl)
	  && !Is_Dispatch_Table_Entity (gnat_entity)
	  && Present (gnat_temp = Expression (gnat_decl))
	  && Nkind (gnat_temp) != N_Allocator
	  && (Is_Elementary_Type (Etype (gnat_entity)) || !type_annotate_only))
	gnu_expr = gnat_to_gnu_external (gnat_temp);

      /* ... fall through ... */

    case E_Exception:
    case E_Loop_Parameter:
    case E_Out_Parameter:
    case E_Variable:
      {
	const Entity_Id gnat_type = Etype (gnat_entity);
	/* Always create a variable for volatile objects and variables seen
	   constant but with a Linker_Section pragma.  */
	bool const_flag
	  = ((kind == E_Constant || kind == E_Variable)
	     && Is_True_Constant (gnat_entity)
	     && !(kind == E_Variable
		  && Present (Linker_Section_Pragma (gnat_entity)))
	     && !Treat_As_Volatile (gnat_entity)
	     && (((Nkind (gnat_decl) == N_Object_Declaration)
		  && Present (Expression (gnat_decl)))
		 || Present (gnat_renamed_obj)
		 || imported_p));
	bool inner_const_flag = const_flag;
	bool static_flag = Is_Statically_Allocated (gnat_entity);
	/* We implement RM 13.3(19) for exported and imported (non-constant)
	   objects by making them volatile.  */
	bool volatile_flag
	  = (Treat_As_Volatile (gnat_entity)
	     || (!const_flag && (Is_Exported (gnat_entity) || imported_p)));
	bool mutable_p = false;
	bool used_by_ref = false;
	tree gnu_ext_name = NULL_TREE;
	tree gnu_ada_size = NULL_TREE;

	/* We need to translate the renamed object even though we are only
	   referencing the renaming.  But it may contain a call for which
	   we'll generate a temporary to hold the return value and which
	   is part of the definition of the renaming, so discard it.  */
	if (Present (gnat_renamed_obj) && !definition)
	  {
	    if (kind == E_Exception)
	      gnu_expr = gnat_to_gnu_entity (Renamed_Entity (gnat_entity),
					     NULL_TREE, false);
	    else
	      gnu_expr = gnat_to_gnu_external (gnat_renamed_obj);
	  }

	/* Get the type after elaborating the renamed object.  */
	if (foreign && Is_Descendant_Of_Address (Underlying_Type (gnat_type)))
	  gnu_type = ptr_type_node;
	else
	  gnu_type = gnat_to_gnu_type (gnat_type);

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
	    TREE_STATIC (gnu_decl) = global_bindings_p ();
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
	     && No (gnat_renamed_obj))
	    || TYPE_IS_DUMMY_P (gnu_type)
	    || TREE_CODE (gnu_type) == VOID_TYPE)
	  {
	    gcc_assert (type_annotate_only);
	    if (this_global)
	      force_global--;
	    return error_mark_node;
	  }

	/* If an alignment is specified, use it if valid.  Note that exceptions
	   are objects but don't have an alignment and there is also no point in
	   setting it for an address clause, since the final type of the object
	   will be a reference type.  */
	if (Known_Alignment (gnat_entity)
	    && kind != E_Exception
	    && No (Address_Clause (gnat_entity)))
	  align = validate_alignment (Alignment (gnat_entity), gnat_entity,
				      TYPE_ALIGN (gnu_type));

	/* Likewise, if a size is specified, use it if valid.  */
	if (Known_Esize (gnat_entity))
	  gnu_size
	    = validate_size (Esize (gnat_entity), gnu_type, gnat_entity,
			     VAR_DECL, false, Has_Size_Clause (gnat_entity),
			     NULL, NULL);
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
	if (No (gnat_renamed_obj)
	    && CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_type)))
	  {
	    if (gnu_expr && kind == E_Constant)
	      {
		gnu_size = TYPE_SIZE (TREE_TYPE (gnu_expr));
		gnu_ada_size = TYPE_ADA_SIZE (TREE_TYPE (gnu_expr));
		if (CONTAINS_PLACEHOLDER_P (gnu_size))
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
		      {
			gnu_size = DECL_SIZE (TREE_OPERAND (gnu_expr, 0));
			gnu_ada_size = gnu_size;
		      }
		    else
		      {
			gnu_size
			  = SUBSTITUTE_PLACEHOLDER_IN_EXPR (gnu_size,
							    gnu_expr);
			gnu_ada_size
			  = SUBSTITUTE_PLACEHOLDER_IN_EXPR (gnu_ada_size,
							    gnu_expr);
		      }
		  }
	      }
	    /* We may have no GNU_EXPR because No_Initialization is
	       set even though there's an Expression.  */
	    else if (kind == E_Constant
		     && Nkind (gnat_decl) == N_Object_Declaration
		     && Present (Expression (gnat_decl)))
	      {
		tree gnu_expr_type
		  = gnat_to_gnu_type (Etype (Expression (gnat_decl)));
		gnu_size = TYPE_SIZE (gnu_expr_type);
		gnu_ada_size = TYPE_ADA_SIZE (gnu_expr_type);
	      }
	    else
	      {
		gnu_size = max_size (TYPE_SIZE (gnu_type), true);
		/* We can be called on unconstrained arrays in this mode.  */
		if (!type_annotate_only)
		  gnu_ada_size = max_size (TYPE_ADA_SIZE (gnu_type), true);
		mutable_p = true;
	      }

	    /* If the size isn't constant and we are at global level, call
	       elaborate_expression_1 to make a variable for it rather than
	       calculating it each time.  */
	    if (!TREE_CONSTANT (gnu_size) && global_bindings_p ())
	      gnu_size = elaborate_expression_1 (gnu_size, gnat_entity,
						 "SIZE", definition, false);
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
	    && !Is_Constr_Subt_For_UN_Aliased (gnat_type)
	    && No (gnat_renamed_obj)
	    && No (Address_Clause (gnat_entity)))
	  gnu_size = bitsize_unit_node;

	/* If this is an object with no specified size and alignment, and
	   if either it is full access or we are not optimizing alignment for
	   space and it is composite and not an exception, an Out parameter
	   or a reference to another object, and the size of its type is a
	   constant, set the alignment to the smallest one which is not
	   smaller than the size, with an appropriate cap.  */
	if (!Known_Esize (gnat_entity)
	    && !Known_Alignment (gnat_entity)
	    && (Is_Full_Access (gnat_entity)
		|| (!Optimize_Alignment_Space (gnat_entity)
		    && kind != E_Exception
		    && kind != E_Out_Parameter
		    && Is_Composite_Type (gnat_type)
		    && !Is_Constr_Subt_For_UN_Aliased (gnat_type)
		    && !Is_Exported (gnat_entity)
		    && !imported_p
		    && No (gnat_renamed_obj)
		    && No (Address_Clause (gnat_entity))))
	    && (TREE_CODE (TYPE_SIZE (gnu_type)) == INTEGER_CST || gnu_size))
	  align = promote_object_alignment (gnu_type, gnu_size, gnat_entity);

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

	    check_ok_for_atomic_type (gnu_inner, gnat_entity, true);
	  }

	/* If this is an aliased object with an unconstrained array nominal
	   subtype, make a type that includes the template.  We will either
	   allocate or create a variable of that type, see below.  */
	if (Is_Constr_Subt_For_UN_Aliased (gnat_type)
	    && Is_Array_Type (Underlying_Type (gnat_type))
	    && !type_annotate_only)
	  {
	    tree gnu_array = gnat_to_gnu_type (Base_Type (gnat_type));
	    gnu_type
	      = build_unc_object_type_from_ptr (TREE_TYPE (gnu_array),
						gnu_type,
						concat_name (gnu_entity_name,
							     "UNC"),
						debug_info_p);
	  }

	/* ??? If this is an object of CW type initialized to a value, try to
	   ensure that the object is sufficient aligned for this value, but
	   without pessimizing the allocation.  This is a kludge necessary
	   because we don't support dynamic alignment.  */
	if (align == 0
	    && Ekind (gnat_type) == E_Class_Wide_Subtype
	    && No (gnat_renamed_obj)
	    && No (Address_Clause (gnat_entity)))
	  align = get_target_system_allocator_alignment () * BITS_PER_UNIT;

#ifdef MINIMUM_ATOMIC_ALIGNMENT
	/* If the size is a constant and no alignment is specified, force
	   the alignment to be the minimum valid atomic alignment.  The
	   restriction on constant size avoids problems with variable-size
	   temporaries; if the size is variable, there's no issue with
	   atomic access.  Also don't do this for a constant, since it isn't
	   necessary and can interfere with constant replacement.  Finally,
	   do not do it for Out parameters since that creates an
	   size inconsistency with In parameters.  */
	if (align == 0
	    && MINIMUM_ATOMIC_ALIGNMENT > TYPE_ALIGN (gnu_type)
	    && !FLOAT_TYPE_P (gnu_type)
	    && !const_flag && No (gnat_renamed_obj)
	    && !imported_p && No (Address_Clause (gnat_entity))
	    && kind != E_Out_Parameter
	    && (gnu_size ? TREE_CODE (gnu_size) == INTEGER_CST
		: TREE_CODE (TYPE_SIZE (gnu_type)) == INTEGER_CST))
	  align = MINIMUM_ATOMIC_ALIGNMENT;
#endif

	/* Do not take into account aliased adjustments or alignment promotions
	   to compute the size of the object.  */
	tree gnu_object_size = gnu_size ? gnu_size : TYPE_SIZE (gnu_type);

	/* If the object is aliased, of a constrained nominal subtype and its
	   size might be zero at run time, we force at least the unit size.  */
	if (Is_Aliased (gnat_entity)
	    && !Is_Constr_Subt_For_UN_Aliased (gnat_type)
	    && Is_Array_Type (Underlying_Type (gnat_type))
	    && !TREE_CONSTANT (gnu_object_size))
	  gnu_size = size_binop (MAX_EXPR, gnu_object_size, bitsize_unit_node);

	/* Make a new type with the desired size and alignment, if needed.  */
	if (gnu_size || align > 0)
	  {
	    tree orig_type = gnu_type;

	    gnu_type = maybe_pad_type (gnu_type, gnu_size, align, gnat_entity,
				       false, definition, true);

	    /* If the nominal subtype of the object is unconstrained and its
	       size is not fixed, compute the Ada size from the Ada size of
	       the subtype and/or the expression; this will make it possible
	       for gnat_type_max_size to easily compute a maximum size.  */
	    if (gnu_ada_size && gnu_size && !TREE_CONSTANT (gnu_size))
	      SET_TYPE_ADA_SIZE (gnu_type, gnu_ada_size);

	    /* If a padding record was made, declare it now since it will
	       never be declared otherwise.  This is necessary to ensure
	       that its subtrees are properly marked.  */
	    if (gnu_type != orig_type && !DECL_P (TYPE_NAME (gnu_type)))
	      create_type_decl (TYPE_NAME (gnu_type), gnu_type, true,
				debug_info_p, gnat_entity);
	  }

	/* Now check if the type of the object allows atomic access.  */
	if (Is_Full_Access (gnat_entity))
	  check_ok_for_atomic_type (gnu_type, gnat_entity, false);

	/* If this is a renaming, avoid as much as possible to create a new
	   object.  However, in some cases, creating it is required because
	   renaming can be applied to objects that are not names in Ada.
	   This processing needs to be applied to the raw expression so as
	   to make it more likely to rename the underlying object.  */
	if (Present (gnat_renamed_obj))
	  {
	    /* If the renamed object had padding, strip off the reference to
	       the inner object and reset our type.  */
	    if ((TREE_CODE (gnu_expr) == COMPONENT_REF
		 && TYPE_IS_PADDING_P (TREE_TYPE (TREE_OPERAND (gnu_expr, 0))))
		/* Strip useless conversions around the object.  */
		|| gnat_useless_type_conversion (gnu_expr))
	      {
		gnu_expr = TREE_OPERAND (gnu_expr, 0);
		gnu_type = TREE_TYPE (gnu_expr);
	      }

	    /* Or else, if the renamed object has an unconstrained type with
	       default discriminant, use the padded type.  */
	    else if (type_is_padding_self_referential (TREE_TYPE (gnu_expr)))
	      gnu_type = TREE_TYPE (gnu_expr);

	    /* If this is a constant renaming stemming from a function call,
	       treat it as a normal object whose initial value is what is being
	       renamed.  RM 3.3 says that the result of evaluating a function
	       call is a constant object.  Therefore, it can be the inner
	       object of a constant renaming and the renaming must be fully
	       instantiated, i.e. it cannot be a reference to (part of) an
	       existing object.  And treat other rvalues the same way.  */
	    tree inner = gnu_expr;
	    while (handled_component_p (inner) || CONVERT_EXPR_P (inner))
	      inner = TREE_OPERAND (inner, 0);
	    /* Expand_Dispatching_Call can prepend a comparison of the tags
	       before the call to "=".  */
	    if (TREE_CODE (inner) == TRUTH_ANDIF_EXPR
		|| TREE_CODE (inner) == COMPOUND_EXPR)
	      inner = TREE_OPERAND (inner, 1);
	    if ((TREE_CODE (inner) == CALL_EXPR
		 && !call_is_atomic_load (inner))
		|| TREE_CODE (inner) == CONSTRUCTOR
		|| CONSTANT_CLASS_P (inner)
		|| COMPARISON_CLASS_P (inner)
		|| BINARY_CLASS_P (inner)
		|| EXPRESSION_CLASS_P (inner)
		/* We need to detect the case where a temporary is created to
		   hold the return value, since we cannot safely rename it at
		   top level as it lives only in the elaboration routine.  */
		|| (TREE_CODE (inner) == VAR_DECL
		    && DECL_RETURN_VALUE_P (inner))
		/* We also need to detect the case where the front-end creates
		   a dangling 'reference to a function call at top level and
		   substitutes it in the renaming, for example:

		     q__b : boolean renames r__f.e (1);

	           can be rewritten into:

		     q__R1s : constant q__A2s := r__f'reference;
		     [...]
		     q__b : boolean renames q__R1s.all.e (1);

		   We cannot safely rename the rewritten expression since the
		   underlying object lives only in the elaboration routine.  */
		|| (TREE_CODE (inner) == INDIRECT_REF
		    && (inner
			= remove_conversions (TREE_OPERAND (inner, 0), true))
		    && TREE_CODE (inner) == VAR_DECL
		    && DECL_RETURN_VALUE_P (inner)))
	      ;

	    /* Otherwise, this is an lvalue being renamed, so it needs to be
	       elaborated as a reference and substituted for the entity.  But
	       this means that we must evaluate the address of the renaming
	       in the definition case to instantiate the SAVE_EXPRs.  */
	    else
	      {
		tree gnu_init = NULL_TREE;

		if (type_annotate_only && TREE_CODE (gnu_expr) == ERROR_MARK)
		  break;

		gnu_expr
		  = elaborate_reference (gnu_expr, gnat_entity, definition,
					 &gnu_init);

		/* No DECL_EXPR might be created so the expression needs to be
		   marked manually because it will likely be shared.  */
		if (global_bindings_p ())
		  MARK_VISITED (gnu_expr);

		/* This assertion will fail if the renamed object isn't aligned
		   enough as to make it possible to honor the alignment set on
		   the renaming.  */
		if (align)
		  {
		    const unsigned int ralign
		      = DECL_P (gnu_expr)
			? DECL_ALIGN (gnu_expr)
			: TYPE_ALIGN (TREE_TYPE (gnu_expr));
		    gcc_assert (ralign >= align);
		  }

		/* The expression might not be a DECL so save it manually.  */
		gnu_decl = gnu_expr;
		save_gnu_tree (gnat_entity, gnu_decl, true);
		saved = true;
		annotate_object (gnat_entity, gnu_type, NULL_TREE, false);

		/* If this is only a reference to the entity, we are done.  */
		if (!definition)
		  break;

		/* Otherwise, emit the initialization statement, if any.  */
		if (gnu_init)
		  add_stmt (gnu_init);

		/* If it needs to be materialized for debugging purposes, build
		   the entity as indirect reference to the renamed object.  */
		if (Materialize_Entity (gnat_entity))
		  {
		    gnu_type = build_reference_type (gnu_type);
		    const_flag = true;
		    volatile_flag = false;

		    gnu_expr = build_unary_op (ADDR_EXPR, gnu_type, gnu_expr);

		    create_var_decl (gnu_entity_name, gnu_ext_name,
				     TREE_TYPE (gnu_expr), gnu_expr,
				     const_flag, Is_Public (gnat_entity),
				     imported_p, static_flag, volatile_flag,
				     artificial_p, debug_info_p, attr_list,
				     gnat_entity, false);
		  }

		/* Otherwise, instantiate the SAVE_EXPRs if needed.  */
		else if (TREE_SIDE_EFFECTS (gnu_expr))
		  add_stmt (build_unary_op (ADDR_EXPR, NULL_TREE, gnu_expr));

		break;
	      }
	  }

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
	    vec<constructor_elt, va_gc> *v;
	    vec_alloc (v, 1);
	    tree t = build_template (TREE_TYPE (template_field),
				     TREE_TYPE (DECL_CHAIN (template_field)),
				     NULL_TREE);
	    CONSTRUCTOR_APPEND_ELT (v, template_field, t);
	    gnu_expr = gnat_build_constructor (gnu_type, v);
	  }

	/* Convert the expression to the type of the object if need be.  */
	if (gnu_expr && initial_value_needs_conversion (gnu_type, gnu_expr))
	  gnu_expr = convert (gnu_type, gnu_expr);

	/* If this is a pointer that doesn't have an initializing expression,
	   initialize it to NULL, unless the object is declared imported as
	   per RM B.1(24).  */
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
	    const Node_Id gnat_clause = Address_Clause (gnat_entity);
	    const Node_Id gnat_address = Expression (gnat_clause);
	    tree gnu_address = present_gnu_tree (gnat_entity)
			       ? TREE_OPERAND (get_gnu_tree (gnat_entity), 0)
			       : gnat_to_gnu (gnat_address);

	    save_gnu_tree (gnat_entity, NULL_TREE, false);

	    /* Convert the type of the object to a reference type that can
	       alias everything as per RM 13.3(19).  */
	    if (volatile_flag && !TYPE_VOLATILE (gnu_type))
	      gnu_type = change_qualified_type (gnu_type, TYPE_QUAL_VOLATILE);
	    gnu_type
	      = build_reference_type_for_mode (gnu_type, ptr_mode, true);
	    gnu_address = convert (gnu_type, gnu_address);
	    used_by_ref = true;
	    const_flag
	      = (!Is_Public (gnat_entity)
		 || compile_time_known_address_p (gnat_address));
	    volatile_flag = false;
	    gnu_size = NULL_TREE;

	    /* If this is an aliased object with an unconstrained array nominal
	       subtype, then it can overlay only another aliased object with an
	       unconstrained array nominal subtype and compatible template.  */
	    if (Is_Constr_Subt_For_UN_Aliased (gnat_type)
		&& Is_Array_Type (Underlying_Type (gnat_type))
		&& !type_annotate_only)
	      {
		tree rec_type = TREE_TYPE (gnu_type);
		tree off = byte_position (DECL_CHAIN (TYPE_FIELDS (rec_type)));

		/* This is the pattern built for a regular object.  */
		if (TREE_CODE (gnu_address) == POINTER_PLUS_EXPR
		    && TREE_OPERAND (gnu_address, 1) == off)
		  gnu_address = TREE_OPERAND (gnu_address, 0);

		/* This is the pattern built for an overaligned object.  */
		else if (TREE_CODE (gnu_address) == POINTER_PLUS_EXPR
			 && TREE_CODE (TREE_OPERAND (gnu_address, 1))
			    == PLUS_EXPR
			 && TREE_OPERAND (TREE_OPERAND (gnu_address, 1), 1)
			    == off)
		  gnu_address
		    = build2 (POINTER_PLUS_EXPR, gnu_type,
			      TREE_OPERAND (gnu_address, 0),
			      TREE_OPERAND (TREE_OPERAND (gnu_address, 1), 0));

		/* We make an exception for an absolute address but we warn
		   that there is a descriptor at the start of the object.  */
		else if (TREE_CODE (gnu_address) == INTEGER_CST)
		  {
		    post_error_ne ("??aliased object& with unconstrained "
				   "array nominal subtype", gnat_clause,
				   gnat_entity);
		    post_error ("\\starts with a descriptor whose size is "
				"given by ''Descriptor_Size", gnat_clause);
		  }

		else
		  {
		    post_error_ne ("aliased object& with unconstrained array "
				   "nominal subtype", gnat_clause,
				   gnat_entity);
		    post_error ("\\can overlay only aliased object with "
				"compatible subtype", gnat_clause);
		  }
	      }

	    /* If we don't have an initializing expression for the underlying
	       variable, the initializing expression for the pointer is the
	       specified address.  Otherwise, we have to make a COMPOUND_EXPR
	       to assign both the address and the initial value.  */
	    if (!gnu_expr)
	      gnu_expr = gnu_address;
	    else
	      gnu_expr
		= build2 (COMPOUND_EXPR, gnu_type,
			  build_binary_op (INIT_EXPR, NULL_TREE,
					   build_unary_op (INDIRECT_REF,
							   NULL_TREE,
							   gnu_address),
					   gnu_expr),
			  gnu_address);
	  }

	/* If it has an address clause and we are not defining it, mark it
	   as an indirect object.  Likewise for Stdcall objects that are
	   imported.  */
	if ((!definition && Present (Address_Clause (gnat_entity)))
	    || (imported_p && Has_Stdcall_Convention (gnat_entity)))
	  {
	    /* Convert the type of the object to a reference type that can
	       alias everything as per RM 13.3(19).  */
	    if (volatile_flag && !TYPE_VOLATILE (gnu_type))
	      gnu_type = change_qualified_type (gnu_type, TYPE_QUAL_VOLATILE);
	    gnu_type
	      = build_reference_type_for_mode (gnu_type, ptr_mode, true);
	    used_by_ref = true;
	    const_flag = false;
	    volatile_flag = false;
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
				 || static_flag)
	    || (gnu_size
		&& !allocatable_size_p (convert (sizetype,
						 size_binop
						 (EXACT_DIV_EXPR, gnu_size,
						  bitsize_unit_node)),
					global_bindings_p ()
					|| !definition
					|| static_flag)))
	  {
	    if (volatile_flag && !TYPE_VOLATILE (gnu_type))
	      gnu_type = change_qualified_type (gnu_type, TYPE_QUAL_VOLATILE);
	    gnu_type = build_reference_type (gnu_type);
	    used_by_ref = true;
	    const_flag = true;
	    volatile_flag = false;
	    gnu_size = NULL_TREE;

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
			&& CONSTRUCTOR_NELTS (gnu_expr) == 1)
		      gnu_expr = NULL_TREE;
		    else
		      gnu_expr
			= build_component_ref
			    (gnu_expr,
			     DECL_CHAIN (TYPE_FIELDS (TREE_TYPE (gnu_expr))),
			     false);
		  }

		if (TREE_CODE (TYPE_SIZE_UNIT (gnu_alloc_type)) == INTEGER_CST
		    && !valid_constant_size_p (TYPE_SIZE_UNIT (gnu_alloc_type)))
		  post_error ("??`Storage_Error` will be raised at run time!",
			      gnat_entity);

		gnu_expr
		  = build_allocator (gnu_alloc_type, gnu_expr, gnu_type,
				     Empty, Empty, gnat_entity, mutable_p);
	      }
	    else
	      gnu_expr = NULL_TREE;
	  }

	/* If this object would go into the stack and has an alignment larger
	   than the largest stack alignment the back-end can honor, resort to
	   a variable of "aligning type".  */
	if (definition
	    && TYPE_ALIGN (gnu_type) > BIGGEST_ALIGNMENT
	    && !imported_p
	    && !static_flag
	    && !global_bindings_p ())
	  {
	    /* Create the new variable.  No need for extra room before the
	       aligned field as this is in automatic storage.  */
	    tree gnu_new_type
	      = make_aligning_type (gnu_type, TYPE_ALIGN (gnu_type),
				    TYPE_SIZE_UNIT (gnu_type),
				    BIGGEST_ALIGNMENT, 0, gnat_entity);
	    tree gnu_new_var
	      = create_var_decl (create_concat_name (gnat_entity, "ALIGN"),
				 NULL_TREE, gnu_new_type, NULL_TREE,
				 false, false, false, false, false,
				 true, debug_info_p && definition, NULL,
				 gnat_entity);

	    /* Initialize the aligned field if we have an initializer.  */
	    if (gnu_expr)
	      add_stmt_with_node
		(build_binary_op (INIT_EXPR, NULL_TREE,
				  build_component_ref
				  (gnu_new_var, TYPE_FIELDS (gnu_new_type),
				   false),
				  gnu_expr),
		 gnat_entity);

	    /* And setup this entity as a reference to the aligned field.  */
	    gnu_type = build_reference_type (gnu_type);
	    gnu_expr
	      = build_unary_op
		(ADDR_EXPR, NULL_TREE,
		 build_component_ref (gnu_new_var, TYPE_FIELDS (gnu_new_type),
				      false));
	    TREE_CONSTANT (gnu_expr) = 1;

	    used_by_ref = true;
	    const_flag = true;
	    volatile_flag = false;
	    gnu_size = NULL_TREE;
	  }

	/* If this is an aggregate constant initialized to a constant, force it
	   to be statically allocated.  This saves an initialization copy.  */
	if (!static_flag
	    && const_flag
	    && gnu_expr
	    && TREE_CONSTANT (gnu_expr)
	    && AGGREGATE_TYPE_P (gnu_type)
	    && tree_fits_uhwi_p (TYPE_SIZE_UNIT (gnu_type))
	    && !(TYPE_IS_PADDING_P (gnu_type)
		 && !tree_fits_uhwi_p (TYPE_SIZE_UNIT
				       (TREE_TYPE (TYPE_FIELDS (gnu_type))))))
	  static_flag = true;

	/* If this is an aliased object with an unconstrained array nominal
	   subtype, we make its type a thin reference, i.e. the reference
	   counterpart of a thin pointer, so it points to the array part.
	   This is aimed to make it easier for the debugger to decode the
	   object.  Note that we have to do it this late because of the
	   couple of allocation adjustments that might be made above.  */
	if (Is_Constr_Subt_For_UN_Aliased (gnat_type)
	    && Is_Array_Type (Underlying_Type (gnat_type))
	    && !type_annotate_only)
	  {
	    /* In case the object with the template has already been allocated
	       just above, we have nothing to do here.  */
	    if (!TYPE_IS_THIN_POINTER_P (gnu_type))
	      {
		/* This variable is a GNAT encoding used by Workbench: let it
		   go through the debugging information but mark it as
		   artificial: users are not interested in it.  */
		tree gnu_unc_var
		   = create_var_decl (concat_name (gnu_entity_name, "UNC"),
				      NULL_TREE, gnu_type, gnu_expr,
				      const_flag, Is_Public (gnat_entity),
				      imported_p || !definition, static_flag,
				      volatile_flag, true,
				      debug_info_p && definition,
				      NULL, gnat_entity);
		gnu_expr = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_unc_var);
		TREE_CONSTANT (gnu_expr) = 1;

		used_by_ref = true;
		const_flag = true;
		volatile_flag = false;
		inner_const_flag = TREE_READONLY (gnu_unc_var);
		gnu_size = NULL_TREE;
	      }

	    tree gnu_array = gnat_to_gnu_type (Base_Type (gnat_type));
	    gnu_type
	      = build_reference_type (TYPE_OBJECT_RECORD_TYPE (gnu_array));
	  }

	/* Convert the expression to the type of the object if need be.  */
	if (gnu_expr && initial_value_needs_conversion (gnu_type, gnu_expr))
	  gnu_expr = convert (gnu_type, gnu_expr);

	/* If this name is external or a name was specified, use it, but don't
	   use the Interface_Name with an address clause (see cd30005).  */
	if ((Is_Public (gnat_entity) && !Is_Imported (gnat_entity))
	    || (Present (Interface_Name (gnat_entity))
		&& No (Address_Clause (gnat_entity))))
	  gnu_ext_name = create_concat_name (gnat_entity, NULL);

	/* Deal with a pragma Linker_Section on a constant or variable.  */
	if ((kind == E_Constant || kind == E_Variable)
	    && Present (Linker_Section_Pragma (gnat_entity)))
	  prepend_one_attribute_pragma (&attr_list,
					Linker_Section_Pragma (gnat_entity));

	/* Now create the variable or the constant and set various flags.  */
	gnu_decl
	  = create_var_decl (gnu_entity_name, gnu_ext_name, gnu_type,
			     gnu_expr, const_flag, Is_Public (gnat_entity),
			     imported_p || !definition, static_flag,
			     volatile_flag, artificial_p,
			     debug_info_p && definition, attr_list,
			     gnat_entity);
	DECL_BY_REF_P (gnu_decl) = used_by_ref;
	DECL_POINTS_TO_READONLY_P (gnu_decl) = used_by_ref && inner_const_flag;
	DECL_CAN_NEVER_BE_NULL_P (gnu_decl) = Can_Never_Be_Null (gnat_entity);

	/* If we are defining an Out parameter and optimization isn't enabled,
	   create a fake PARM_DECL for debugging purposes and make it point to
	   the VAR_DECL.  Suppress debug info for the latter but make sure it
	   will live in memory so that it can be accessed from within the
	   debugger through the PARM_DECL.  */
	if (kind == E_Out_Parameter
	    && definition
	    && debug_info_p
	    && !optimize
	    && !flag_generate_lto)
	  {
	    tree param = create_param_decl (gnu_entity_name, gnu_type);
	    gnat_pushdecl (param, gnat_entity);
	    SET_DECL_VALUE_EXPR (param, gnu_decl);
	    DECL_HAS_VALUE_EXPR_P (param) = 1;
	    DECL_IGNORED_P (gnu_decl) = 1;
	    TREE_ADDRESSABLE (gnu_decl) = 1;
	  }

	/* If this is a loop parameter, set the corresponding flag.  */
	else if (kind == E_Loop_Parameter)
	  DECL_LOOP_PARM_P (gnu_decl) = 1;

	/* If this is a constant and we are defining it or it generates a real
	   symbol at the object level and we are referencing it, we may want
	   or need to have a true variable to represent it:
	     - if the constant is public and not overlaid on something else,
	     - if its address is taken,
	     - if it is aliased,
	     - if optimization isn't enabled, for debugging purposes.  */
	if (TREE_CODE (gnu_decl) == CONST_DECL
	    && (definition || Sloc (gnat_entity) > Standard_Location)
	    && ((Is_Public (gnat_entity) && No (Address_Clause (gnat_entity)))
		|| Address_Taken (gnat_entity)
		|| Is_Aliased (gnat_entity)
		|| (!optimize && debug_info_p)))
	  {
	    tree gnu_corr_var
	      = create_var_decl (gnu_entity_name, gnu_ext_name, gnu_type,
				 gnu_expr, true, Is_Public (gnat_entity),
				 !definition, static_flag, volatile_flag,
				 artificial_p, debug_info_p && definition,
				 attr_list, gnat_entity, false);

	    SET_DECL_CONST_CORRESPONDING_VAR (gnu_decl, gnu_corr_var);
	    DECL_IGNORED_P (gnu_decl) = 1;
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
	if (Front_End_Exceptions ()
	    && Has_Nested_Block_With_Handler (Scope (gnat_entity)))
	  TREE_ADDRESSABLE (gnu_decl) = 1;

	/* If this is a local variable with non-BLKmode and aggregate type,
	   and optimization isn't enabled, then force it in memory so that
	   a register won't be allocated to it with possible subparts left
	   uninitialized and reaching the register allocator.  */
	else if (TREE_CODE (gnu_decl) == VAR_DECL
		 && !DECL_EXTERNAL (gnu_decl)
		 && !TREE_STATIC (gnu_decl)
		 && DECL_MODE (gnu_decl) != BLKmode
		 && AGGREGATE_TYPE_P (TREE_TYPE (gnu_decl))
		 && !TYPE_IS_FAT_POINTER_P (TREE_TYPE (gnu_decl))
		 && !optimize)
	  TREE_ADDRESSABLE (gnu_decl) = 1;

	/* If we are defining an object with variable size or an object with
	   fixed size that will be dynamically allocated, and we are using the
	   front-end setjmp/longjmp exception mechanism, update the setjmp
	   buffer.  */
	if (definition
	    && Exception_Mechanism == Front_End_SJLJ
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
			 used_by_ref);
      }
      break;

    case E_Void:
      /* Return a TYPE_DECL for "void" that we previously made.  */
      gnu_decl = TYPE_NAME (void_type_node);
      break;

    case E_Enumeration_Type:
      /* A special case: for the types Character and Wide_Character in
	 Standard, we do not list all the literals.  So if the literals
	 are not specified, make this an integer type.  */
      if (No (First_Literal (gnat_entity)))
	{
	  if (esize == CHAR_TYPE_SIZE && flag_signed_char)
	    gnu_type = make_signed_type (CHAR_TYPE_SIZE);
	  else
	    gnu_type = make_unsigned_type (esize);
	  TYPE_NAME (gnu_type) = gnu_entity_name;

	  /* Set TYPE_STRING_FLAG for Character and Wide_Character types.
	     This is needed by the DWARF-2 back-end to distinguish between
	     unsigned integer types and character types.  */
	  TYPE_STRING_FLAG (gnu_type) = 1;

	  /* This flag is needed by the call just below.  */
	  TYPE_ARTIFICIAL (gnu_type) = artificial_p;

	  finish_character_type (gnu_type);
	}
      else
	{
	  /* We have a list of enumeral constants in First_Literal.  We make a
	     CONST_DECL for each one and build into GNU_LITERAL_LIST the list
	     to be placed into TYPE_FIELDS.  Each node is itself a TREE_LIST
	     whose TREE_VALUE is the literal name and whose TREE_PURPOSE is the
	     value of the literal.  But when we have a regular boolean type, we
	     simplify this a little by using a BOOLEAN_TYPE.  */
	  const bool is_boolean = Is_Boolean_Type (gnat_entity)
				  && !Has_Non_Standard_Rep (gnat_entity);
	  const bool is_unsigned = Is_Unsigned_Type (gnat_entity);
	  tree gnu_list = NULL_TREE;
	  Entity_Id gnat_literal;

	  /* Boolean types with foreign convention have precision 1.  */
	  if (is_boolean && foreign)
	    esize = 1;

	  gnu_type = make_node (is_boolean ? BOOLEAN_TYPE : ENUMERAL_TYPE);
	  TYPE_PRECISION (gnu_type) = esize;
	  TYPE_UNSIGNED (gnu_type) = is_unsigned;
	  set_min_and_max_values_for_integral_type (gnu_type, esize,
						    TYPE_SIGN (gnu_type));
	  process_attributes (&gnu_type, &attr_list, true, gnat_entity);
	  layout_type (gnu_type);

	  for (gnat_literal = First_Literal (gnat_entity);
	       Present (gnat_literal);
	       gnat_literal = Next_Literal (gnat_literal))
	    {
	      tree gnu_value
		= UI_To_gnu (Enumeration_Rep (gnat_literal), gnu_type);
	      /* Do not generate debug info for individual enumerators.  */
	      tree gnu_literal
		= create_var_decl (get_entity_name (gnat_literal), NULL_TREE,
				   gnu_type, gnu_value, true, false, false,
				   false, false, artificial_p, false,
				   NULL, gnat_literal);
	      save_gnu_tree (gnat_literal, gnu_literal, false);
	      gnu_list
	        = tree_cons (DECL_NAME (gnu_literal), gnu_value, gnu_list);
	    }

	  if (!is_boolean)
	    TYPE_VALUES (gnu_type) = nreverse (gnu_list);

	  /* Note that the bounds are updated at the end of this function
	     to avoid an infinite recursion since they refer to the type.  */
	  goto discrete_type;
	}
      break;

    case E_Signed_Integer_Type:
      /* For integer types, just make a signed type the appropriate number
	 of bits.  */
      gnu_type = make_signed_type (esize);
      goto discrete_type;

    case E_Ordinary_Fixed_Point_Type:
    case E_Decimal_Fixed_Point_Type:
      {
	/* Small_Value is the scale factor.  */
	const Ureal gnat_small_value = Small_Value (gnat_entity);
	tree scale_factor = NULL_TREE;

	gnu_type = make_signed_type (esize);

	/* When encoded as 1/2**N or 1/10**N, describe the scale factor as a
	   binary or decimal scale: it is easier to read for humans.  */
	if (UI_Eq (Numerator (gnat_small_value), Uint_1)
	    && (Rbase (gnat_small_value) == 2
		|| Rbase (gnat_small_value) == 10))
	  {
	    tree base
	      = build_int_cst (integer_type_node, Rbase (gnat_small_value));
	    tree exponent
	      = build_int_cst (integer_type_node,
			       UI_To_Int (Denominator (gnat_small_value)));
	    scale_factor
	      = build2 (RDIV_EXPR, integer_type_node,
			integer_one_node,
			build2 (POWER_EXPR, integer_type_node,
				base, exponent));
	  }

	/* Use the arbitrary scale factor description.  Note that we support
	   a Small_Value whose magnitude is larger than 64-bit even on 32-bit
	   platforms, so we unconditionally use a (dummy) 128-bit type.  */
	else
	  {
	    const Uint gnat_num = Norm_Num (gnat_small_value);
	    const Uint gnat_den = Norm_Den (gnat_small_value);
	    tree gnu_small_type = make_unsigned_type (128);
	    tree gnu_num = UI_To_gnu (gnat_num, gnu_small_type);
	    tree gnu_den = UI_To_gnu (gnat_den, gnu_small_type);

	    scale_factor
	      = build2 (RDIV_EXPR, gnu_small_type, gnu_num, gnu_den);
	  }

	TYPE_FIXED_POINT_P (gnu_type) = 1;
	SET_TYPE_SCALE_FACTOR (gnu_type, scale_factor);
      }
      goto discrete_type;

    case E_Modular_Integer_Type:
      {
	/* Packed Array Impl. Types are supposed to be subtypes only.  */
	gcc_assert (!Is_Packed_Array_Impl_Type (gnat_entity));

	/* For modular types, make the unsigned type of the proper number
	   of bits and then set up the modulus, if required.  */
	gnu_type = make_unsigned_type (esize);

	/* Get the modulus in this type.  If the modulus overflows, assume
	   that this is because it was equal to 2**Esize.  Note that there
	   is no overflow checking done on unsigned types, so we detect the
	   overflow by looking for a modulus of zero, which is invalid.  */
	tree gnu_modulus = UI_To_gnu (Modulus (gnat_entity), gnu_type);

	/* If the modulus is not 2**Esize, then this also means that the upper
	   bound of the type, i.e. modulus - 1, is not maximal, so we create an
	   extra subtype to carry it and set the modulus on the base type.  */
	if (!integer_zerop (gnu_modulus))
	  {
	    TYPE_NAME (gnu_type) = create_concat_name (gnat_entity, "UMT");
	    TYPE_MODULAR_P (gnu_type) = 1;
	    SET_TYPE_MODULUS (gnu_type, gnu_modulus);
	    tree gnu_high = fold_build2 (MINUS_EXPR, gnu_type, gnu_modulus,
					 build_int_cst (gnu_type, 1));
	    gnu_type
	      = create_extra_subtype (gnu_type, TYPE_MIN_VALUE (gnu_type),
				      gnu_high);
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
	gnat_to_gnu_entity (Ancestor_Subtype (gnat_entity), gnu_expr, false);

      /* Set the precision to the Esize except for bit-packed arrays.  */
      if (Is_Packed_Array_Impl_Type (gnat_entity))
	esize = UI_To_Int (RM_Size (gnat_entity));

      /* Boolean types with foreign convention have precision 1.  */
      if (Is_Boolean_Type (gnat_entity) && foreign)
	{
	  gnu_type = make_node (BOOLEAN_TYPE);
	  TYPE_PRECISION (gnu_type) = 1;
	  TYPE_UNSIGNED (gnu_type) = 1;
	  set_min_and_max_values_for_integral_type (gnu_type, 1, UNSIGNED);
	  layout_type (gnu_type);
	}
      /* First subtypes of Character are treated as Character; otherwise
	 this should be an unsigned type if the base type is unsigned or
	 if the lower bound is constant and non-negative or if the type
	 is biased.  However, even if the lower bound is constant and
	 non-negative, we use a signed type for a subtype with the same
	 size as its signed base type, because this eliminates useless
	 conversions to it and gives more leeway to the optimizer; but
	 this means that we will need to explicitly test for this case
	 when we change the representation based on the RM size.  */
      else if (kind == E_Enumeration_Subtype
	  && No (First_Literal (Etype (gnat_entity)))
	  && Esize (gnat_entity) == RM_Size (gnat_entity)
	  && esize == CHAR_TYPE_SIZE
	  && flag_signed_char)
	gnu_type = make_signed_type (CHAR_TYPE_SIZE);
      else if (Is_Unsigned_Type (Underlying_Type (Etype (gnat_entity)))
	       || (Esize (Etype (gnat_entity)) != Esize (gnat_entity)
		   && Is_Unsigned_Type (gnat_entity))
	       || Has_Biased_Representation (gnat_entity))
	gnu_type = make_unsigned_type (esize);
      else
	gnu_type = make_signed_type (esize);
      TREE_TYPE (gnu_type) = get_unpadded_type (Etype (gnat_entity));

      SET_TYPE_RM_MIN_VALUE
	(gnu_type, elaborate_expression (Type_Low_Bound (gnat_entity),
					 gnat_entity, "L", definition, true,
					 debug_info_p));

      SET_TYPE_RM_MAX_VALUE
	(gnu_type, elaborate_expression (Type_High_Bound (gnat_entity),
					 gnat_entity, "U", definition, true,
					 debug_info_p));

      if (TREE_CODE (gnu_type) == INTEGER_TYPE)
	TYPE_BIASED_REPRESENTATION_P (gnu_type)
	  = Has_Biased_Representation (gnat_entity);

      /* Do the same processing for Character subtypes as for types.  */
      if (TREE_CODE (TREE_TYPE (gnu_type)) == INTEGER_TYPE
	  && TYPE_STRING_FLAG (TREE_TYPE (gnu_type)))
	{
	  TYPE_NAME (gnu_type) = gnu_entity_name;
	  TYPE_STRING_FLAG (gnu_type) = 1;
	  TYPE_ARTIFICIAL (gnu_type) = artificial_p;
	  finish_character_type (gnu_type);
	}

      /* Inherit our alias set from what we're a subtype of.  Subtypes
	 are not different types and a pointer can designate any instance
	 within a subtype hierarchy.  */
      relate_alias_sets (gnu_type, TREE_TYPE (gnu_type), ALIAS_SET_COPY);

      /* One of the above calls might have caused us to be elaborated,
	 so don't blow up if so.  */
      if (present_gnu_tree (gnat_entity))
	{
	  maybe_present = true;
	  break;
	}

      /* Attach the TYPE_STUB_DECL in case we have a parallel type.  */
      TYPE_STUB_DECL (gnu_type)
	= create_type_stub_decl (gnu_entity_name, gnu_type);

    discrete_type:

      /* We have to handle clauses that under-align the type specially.  */
      if ((Present (Alignment_Clause (gnat_entity))
	   || (Is_Packed_Array_Impl_Type (gnat_entity)
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
      if (Is_Packed_Array_Impl_Type (gnat_entity))
	{
	  tree gnu_field_type, gnu_field, t;

	  gcc_assert (Is_Bit_Packed_Array (Original_Array_Type (gnat_entity)));
	  TYPE_BIT_PACKED_ARRAY_TYPE_P (gnu_type) = 1;

	  /* Make the original array type a parallel/debug type.  */
	  if (debug_info_p)
	    {
	      tree gnu_name
		= associate_original_type_to_packed_array (gnu_type,
							   gnat_entity);
	      if (gnu_name)
		gnu_entity_name = gnu_name;
	    }

	  /* Set the RM size before wrapping up the original type.  */
	  SET_TYPE_RM_SIZE (gnu_type,
			    UI_To_gnu (RM_Size (gnat_entity), bitsizetype));

	  /* Create a stripped-down declaration, mainly for debugging.  */
	  t = create_type_decl (gnu_entity_name, gnu_type, true, debug_info_p,
				gnat_entity);

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
	  SET_TYPE_ALIGN (gnu_type,
			  align > 0 ? align : TYPE_ALIGN (gnu_field_type));

	  /* Propagate the reverse storage order flag to the record type so
	     that the required byte swapping is performed when retrieving the
	     enclosed modular value.  */
	  TYPE_REVERSE_STORAGE_ORDER (gnu_type)
	    = Reverse_Storage_Order (Original_Array_Type (gnat_entity));

	  relate_alias_sets (gnu_type, gnu_field_type, ALIAS_SET_COPY);

	  /* Don't declare the field as addressable since we won't be taking
	     its address and this would prevent create_field_decl from making
	     a bitfield.  */
	  gnu_field
	    = create_field_decl (get_identifier ("OBJECT"), gnu_field_type,
				 gnu_type, NULL_TREE, bitsize_zero_node, 1, 0);

	  /* We will output additional debug info manually below.  */
	  finish_record_type (gnu_type, gnu_field, 2, false);
	  TYPE_JUSTIFIED_MODULAR_P (gnu_type) = 1;

	  /* Make the original array type a parallel/debug type.  Note that
	     gnat_get_array_descr_info needs a TYPE_IMPL_PACKED_ARRAY_P type
	     so we use an intermediate step for standard DWARF.  */
	  if (debug_info_p)
	    {
	      if (gnat_encodings != DWARF_GNAT_ENCODINGS_ALL)
		SET_TYPE_DEBUG_TYPE (gnu_type, gnu_field_type);
	      else if (DECL_PARALLEL_TYPE (t))
		add_parallel_type (gnu_type, DECL_PARALLEL_TYPE (t));
	    }
	}

      /* If the type we are dealing with has got a smaller alignment than the
	 natural one, we need to wrap it up in a record type and misalign the
	 latter; we reuse the padding machinery for this purpose.  */
      else if (align > 0)
	{
	  tree gnu_size = UI_To_gnu (RM_Size (gnat_entity), bitsizetype);

	  /* Set the RM size before wrapping the type.  */
	  SET_TYPE_RM_SIZE (gnu_type, gnu_size);

	  /* Create a stripped-down declaration, mainly for debugging.  */
	  create_type_decl (gnu_entity_name, gnu_type, true, debug_info_p,
			    gnat_entity);

	  gnu_type
	    = maybe_pad_type (gnu_type, TYPE_SIZE (gnu_type), align,
			      gnat_entity, false, definition, false);

	  TYPE_PACKED (gnu_type) = 1;
	  SET_TYPE_ADA_SIZE (gnu_type, gnu_size);
	}

      break;

    case E_Floating_Point_Type:
      /* The type of the Low and High bounds can be our type if this is
	 a type from Standard, so set them at the end of the function.  */
      gnu_type = make_node (REAL_TYPE);
      TYPE_PRECISION (gnu_type) = fp_size_to_prec (esize);
      layout_type (gnu_type);
      break;

    case E_Floating_Point_Subtype:
      /* See the E_Signed_Integer_Subtype case for the rationale.  */
      if (!definition
	  && Present (Ancestor_Subtype (gnat_entity))
	  && !In_Extended_Main_Code_Unit (Ancestor_Subtype (gnat_entity))
	  && (!Compile_Time_Known_Value (Type_Low_Bound (gnat_entity))
	      || !Compile_Time_Known_Value (Type_High_Bound (gnat_entity))))
	gnat_to_gnu_entity (Ancestor_Subtype (gnat_entity), gnu_expr, false);

      gnu_type = make_node (REAL_TYPE);
      TREE_TYPE (gnu_type) = get_unpadded_type (Etype (gnat_entity));
      TYPE_PRECISION (gnu_type) = fp_size_to_prec (esize);
      TYPE_GCC_MIN_VALUE (gnu_type)
	= TYPE_GCC_MIN_VALUE (TREE_TYPE (gnu_type));
      TYPE_GCC_MAX_VALUE (gnu_type)
	= TYPE_GCC_MAX_VALUE (TREE_TYPE (gnu_type));
      layout_type (gnu_type);

      SET_TYPE_RM_MIN_VALUE
	(gnu_type, elaborate_expression (Type_Low_Bound (gnat_entity),
					 gnat_entity, "L", definition, true,
					 debug_info_p));

      SET_TYPE_RM_MAX_VALUE
	(gnu_type, elaborate_expression (Type_High_Bound (gnat_entity),
					 gnat_entity, "U", definition, true,
					 debug_info_p));

      /* Inherit our alias set from what we're a subtype of, as for
	 integer subtypes.  */
      relate_alias_sets (gnu_type, TREE_TYPE (gnu_type), ALIAS_SET_COPY);

      /* One of the above calls might have caused us to be elaborated,
	 so don't blow up if so.  */
      maybe_present = true;
      break;

      /* Array Types and Subtypes

	 In GNAT unconstrained array types are represented by E_Array_Type and
	 constrained array types are represented by E_Array_Subtype.  They are
	 translated into UNCONSTRAINED_ARRAY_TYPE and ARRAY_TYPE respectively.
	 But there are no actual objects of an unconstrained array type; all we
	 have are pointers to that type.  In addition to the type node itself,
	 4 other types associated with it are built in the process:

	   1. the array type (suffix XUA) containing the actual data,

	   2. the template type (suffix XUB) containng the bounds,

	   3. the fat pointer type (suffix XUP) representing a pointer or a
	      reference to the unconstrained array type:
		XUP = struct { XUA *, XUB * }

	   4. the object record type (suffix XUT) containing bounds and data:
		XUT = struct { XUB, XUA }

	 The bounds of the array type XUA (de)reference the XUB * field of a
	 PLACEHOLDER_EXPR for the fat pointer type XUP, so the array type XUA
	 is to be interpreted in the context of the fat pointer type XUB for
	 debug info purposes.  */

    case E_Array_Type:
      {
	const Entity_Id PAT = Packed_Array_Impl_Type (gnat_entity);
	const bool convention_fortran_p
	  = (Convention (gnat_entity) == Convention_Fortran);
	const int ndim = Number_Dimensions (gnat_entity);
	tree gnu_template_type;
	tree gnu_ptr_template;
	tree gnu_template_reference, gnu_template_fields, gnu_fat_type;
	tree *gnu_index_types = XALLOCAVEC (tree, ndim);
	tree *gnu_temp_fields = XALLOCAVEC (tree, ndim);
	tree gnu_max_size = size_one_node, tem, obj;
	Entity_Id gnat_index;
	int index;
	tree comp_type;

	/* Create the type for the component now, as it simplifies breaking
	   type reference loops.  */
	comp_type
	  = gnat_to_gnu_component_type (gnat_entity, definition, debug_info_p);
	if (present_gnu_tree (gnat_entity))
	  {
	    /* As a side effect, the type may have been translated.  */
	    maybe_present = true;
	    break;
	  }

	/* We complete an existing dummy fat pointer type in place.  This both
	   avoids further complex adjustments in update_pointer_to and yields
	   better debugging information in DWARF by leveraging the support for
	   incomplete declarations of "tagged" types in the DWARF back-end.  */
	gnu_type = get_dummy_type (gnat_entity);
	if (gnu_type && TYPE_POINTER_TO (gnu_type))
	  {
	    gnu_fat_type = TYPE_MAIN_VARIANT (TYPE_POINTER_TO (gnu_type));
	    TYPE_NAME (gnu_fat_type) = NULL_TREE;
	    gnu_ptr_template =
	      TREE_TYPE (DECL_CHAIN (TYPE_FIELDS (gnu_fat_type)));
	    gnu_template_type = TREE_TYPE (gnu_ptr_template);

	    /* Save the contents of the dummy type for update_pointer_to.  */
	    TYPE_POINTER_TO (gnu_type) = copy_type (gnu_fat_type);
	    TYPE_FIELDS (TYPE_POINTER_TO (gnu_type))
	      = copy_node (TYPE_FIELDS (gnu_fat_type));
	    DECL_CHAIN (TYPE_FIELDS (TYPE_POINTER_TO (gnu_type)))
	      = copy_node (DECL_CHAIN (TYPE_FIELDS (gnu_fat_type)));
	  }
	else
	  {
	    gnu_fat_type = make_node (RECORD_TYPE);
	    gnu_template_type = make_node (RECORD_TYPE);
	    gnu_ptr_template = build_pointer_type (gnu_template_type);
	  }

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
	   yet (it will reference the fat pointer via the bounds).  Note
	   that we reuse the existing fields of a dummy type because for:

	     type Arr is array (Positive range <>) of Element_Type;
	     type Array_Ref is access Arr;
	     Var : Array_Ref := Null;

	   in a declarative part, Arr will be frozen only after Var, which
	   means that the fields used in the CONSTRUCTOR built for Null are
	   those of the dummy type, which in turn means that COMPONENT_REFs
	   of Var may be built with these fields.  Now if COMPONENT_REFs of
	   Var are also built later with the fields of the final type, the
	   aliasing machinery may consider that the accesses are distinct
	   if the FIELD_DECLs are distinct as objects.  */
	if (COMPLETE_TYPE_P (gnu_fat_type))
	  {
	    tem = TYPE_FIELDS (gnu_fat_type);
	    TREE_TYPE (tem) = ptr_type_node;
	    TREE_TYPE (DECL_CHAIN (tem)) = gnu_ptr_template;
	    TYPE_DECL_SUPPRESS_DEBUG (TYPE_STUB_DECL (gnu_fat_type)) = 0;
	    for (tree t = gnu_fat_type; t; t = TYPE_NEXT_VARIANT (t))
	      SET_TYPE_UNCONSTRAINED_ARRAY (t, gnu_type);
	  }
	else
	  {
	    /* We make the fields addressable for the sake of compatibility
	       with languages for which the regular fields are addressable.  */
	    tem
	      = create_field_decl (get_identifier ("P_ARRAY"),
				   ptr_type_node, gnu_fat_type,
				   NULL_TREE, NULL_TREE, 0, 1);
	    DECL_CHAIN (tem)
	      = create_field_decl (get_identifier ("P_BOUNDS"),
				   gnu_ptr_template, gnu_fat_type,
				   NULL_TREE, NULL_TREE, 0, 1);
	    finish_fat_pointer_type (gnu_fat_type, tem);
	    SET_TYPE_UNCONSTRAINED_ARRAY (gnu_fat_type, gnu_type);
	  }

	/* If the GNAT encodings are used, give the fat pointer type a name.
	   If this is a packed type implemented specially, tell the debugger
	   how to interpret the underlying bits by fetching the name of the
	   implementation type.  But, in any case, mark it as artificial so
	   the debugger can skip it.  */
	const Entity_Id gnat_name
	  = Present (PAT) && gnat_encodings == DWARF_GNAT_ENCODINGS_ALL
	    ? PAT
	    : gnat_entity;
	tree xup_name
	  = gnat_encodings == DWARF_GNAT_ENCODINGS_ALL
	    ? create_concat_name (gnat_name, "XUP")
	    : gnu_entity_name;
	create_type_decl (xup_name, gnu_fat_type, true, debug_info_p,
			  gnat_entity);

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
	     IN_RANGE (index, 0, ndim - 1);
	     index += (convention_fortran_p ? - 1 : 1),
	     gnat_index = Next_Index (gnat_index))
	  {
	    const bool is_flb
	      = Is_Fixed_Lower_Bound_Index_Subtype (Etype (gnat_index));
	    tree gnu_index_type = get_unpadded_type (Etype (gnat_index));
	    tree gnu_orig_min = TYPE_MIN_VALUE (gnu_index_type);
	    tree gnu_orig_max = TYPE_MAX_VALUE (gnu_index_type);
	    tree gnu_index_base_type = get_base_type (gnu_index_type);
	    tree gnu_lb_field, gnu_hb_field;
	    tree gnu_min, gnu_max, gnu_high;
	    char field_name[16];

	    /* Update the maximum size of the array in elements.  */
	    if (gnu_max_size)
	      gnu_max_size
		= update_n_elem (gnu_max_size, gnu_orig_min, gnu_orig_max);

	    /* Now build the self-referential bounds of the index type.  */
	    gnu_index_type = maybe_character_type (gnu_index_type);
	    gnu_index_base_type = maybe_character_type (gnu_index_base_type);

	    /* Make the FIELD_DECLs for the low and high bounds of this
	       type and then make extractions of these fields from the
	       template.  */
	    sprintf (field_name, "LB%d", index);
	    gnu_lb_field = create_field_decl (get_identifier (field_name),
					      gnu_index_type,
					      gnu_template_type, NULL_TREE,
					      NULL_TREE, 0, 0);
	    Sloc_to_locus (Sloc (gnat_entity),
			   &DECL_SOURCE_LOCATION (gnu_lb_field));

	    field_name[0] = 'U';
	    gnu_hb_field = create_field_decl (get_identifier (field_name),
					      gnu_index_type,
					      gnu_template_type, NULL_TREE,
					      NULL_TREE, 0, 0);
	    Sloc_to_locus (Sloc (gnat_entity),
			   &DECL_SOURCE_LOCATION (gnu_hb_field));

	    gnu_temp_fields[index] = chainon (gnu_lb_field, gnu_hb_field);

	    /* We can't use build_component_ref here since the template type
	       isn't complete yet.  */
	    if (!is_flb)
	      {
		gnu_orig_min = build3 (COMPONENT_REF, TREE_TYPE (gnu_lb_field),
				       gnu_template_reference, gnu_lb_field,
				       NULL_TREE);
		TREE_READONLY (gnu_orig_min) = 1;
	      }

	    gnu_orig_max = build3 (COMPONENT_REF, TREE_TYPE (gnu_hb_field),
				   gnu_template_reference, gnu_hb_field,
				   NULL_TREE);
	    TREE_READONLY (gnu_orig_max) = 1;

	    gnu_min = convert (sizetype, gnu_orig_min);
	    gnu_max = convert (sizetype, gnu_orig_max);

	    /* Compute the size of this dimension.  See the E_Array_Subtype
	       case below for the rationale.  */
	    if (is_flb
		&& Nkind (gnat_index) == N_Subtype_Indication
	        && flb_cannot_be_superflat (gnat_index))
	      gnu_high = gnu_max;

	    else
	      gnu_high
		= build3 (COND_EXPR, sizetype,
			  build2 (GE_EXPR, boolean_type_node,
				  gnu_orig_max, gnu_orig_min),
			  gnu_max,
			  TREE_CODE (gnu_min) == INTEGER_CST
			  ? int_const_binop (MINUS_EXPR, gnu_min, size_one_node)
			  : size_binop (MINUS_EXPR, gnu_min, size_one_node));

	    /* Make a range type with the new range in the Ada base type.
	       Then make an index type with the size range in sizetype.  */
	    gnu_index_types[index]
	      = create_index_type (gnu_min, gnu_high,
				   create_range_type (gnu_index_base_type,
						      gnu_orig_min,
						      gnu_orig_max),
				   gnat_entity);

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
	TYPE_CONTEXT (gnu_template_type) = current_function_decl;

	/* If Component_Size is not already specified, annotate it with the
	   size of the component.  */
	if (!Known_Component_Size (gnat_entity))
	  Set_Component_Size (gnat_entity,
                              annotate_value (TYPE_SIZE (comp_type)));

	/* Compute the maximum size of the array in units.  */
	if (gnu_max_size)
	  gnu_max_size
	    = size_binop (MULT_EXPR, gnu_max_size, TYPE_SIZE_UNIT (comp_type));

	/* Now build the array type.  */
        tem = comp_type;
	for (index = ndim - 1; index >= 0; index--)
	  {
	    tem = build_nonshared_array_type (tem, gnu_index_types[index]);
	    TYPE_MULTI_ARRAY_P (tem) = (index > 0);
	    TYPE_CONVENTION_FORTRAN_P (tem) = convention_fortran_p;
	    if (index == ndim - 1 && Reverse_Storage_Order (gnat_entity))
	      set_reverse_storage_order_on_array_type (tem);
	    if (array_type_has_nonaliased_component (tem, gnat_entity))
	      set_nonaliased_component_on_array_type (tem);
	  }

	/* If this is a packed type implemented specially, then process the
	   implementation type so it is elaborated in the proper scope.  */
	if (Present (PAT))
	  gnat_to_gnu_entity (PAT, NULL_TREE, false);

	/* Otherwise, if an alignment is specified, use it if valid and, if
	   the alignment was requested with an explicit clause, state so.  */
	else if (Known_Alignment (gnat_entity))
	  {
	    SET_TYPE_ALIGN (tem,
			    validate_alignment (Alignment (gnat_entity),
						gnat_entity,
						TYPE_ALIGN (tem)));
	    if (Present (Alignment_Clause (gnat_entity)))
	      TYPE_USER_ALIGN (tem) = 1;
	  }

	/* Tag top-level ARRAY_TYPE nodes for packed arrays and their
	   implementation types as such so that the debug information back-end
	   can output the appropriate description for them.  */
	TYPE_PACKED (tem)
	  = (Is_Packed (gnat_entity)
	     || Is_Packed_Array_Impl_Type (gnat_entity));

	if (Treat_As_Volatile (gnat_entity))
	  tem = change_qualified_type (tem, TYPE_QUAL_VOLATILE);

	/* Adjust the type of the pointer-to-array field of the fat pointer
	   and record the aliasing relationships if necessary.  If this is
	   a packed type implemented specially, then use a ref-all pointer
	   type since the implementation type may vary between constrained
	   subtypes and unconstrained base type.  */
	if (Present (PAT))
	  TREE_TYPE (TYPE_FIELDS (gnu_fat_type))
	    = build_pointer_type_for_mode (tem, ptr_mode, true);
	else
	  TREE_TYPE (TYPE_FIELDS (gnu_fat_type)) = build_pointer_type (tem);
	if (TYPE_ALIAS_SET_KNOWN_P (gnu_fat_type))
	  record_component_aliases (gnu_fat_type);

	/* If the maximum size doesn't overflow, use it.  */
	if (gnu_max_size
	    && TREE_CODE (gnu_max_size) == INTEGER_CST
	    && !TREE_OVERFLOW (gnu_max_size)
	    && compare_tree_int (gnu_max_size, TYPE_ARRAY_SIZE_LIMIT) <= 0)
	  TYPE_ARRAY_MAX_SIZE (tem) = gnu_max_size;

	/* See the above description for the rationale.  */
	create_type_decl (create_concat_name (gnat_entity, "XUA"), tem,
			  artificial_p, debug_info_p, gnat_entity);
	TYPE_CONTEXT (tem) = gnu_fat_type;
	TYPE_CONTEXT (TYPE_POINTER_TO (tem)) = gnu_fat_type;

	/* Create the type to be designated by thin pointers: a record type for
	   the array and its template.  We used to shift the fields to have the
	   template at a negative offset, but this was somewhat of a kludge; we
	   now shift thin pointer values explicitly but only those which have a
	   TYPE_UNCONSTRAINED_ARRAY attached to the designated RECORD_TYPE.
	   If the GNAT encodings are used, give it a name.  */
	tree xut_name
	  = (gnat_encodings == DWARF_GNAT_ENCODINGS_ALL)
	    ? create_concat_name (gnat_name, "XUT")
	    : gnu_entity_name;
	obj = build_unc_object_type (gnu_template_type, tem, xut_name,
				     debug_info_p);

	SET_TYPE_UNCONSTRAINED_ARRAY (obj, gnu_type);
	TYPE_OBJECT_RECORD_TYPE (gnu_type) = obj;

	/* The result type is an UNCONSTRAINED_ARRAY_TYPE that indicates the
	   corresponding fat pointer.  */
	TREE_TYPE (gnu_type) = gnu_fat_type;
	TYPE_POINTER_TO (gnu_type) = gnu_fat_type;
	TYPE_REFERENCE_TO (gnu_type) = gnu_fat_type;
	SET_TYPE_MODE (gnu_type, BLKmode);
	SET_TYPE_ALIGN (gnu_type, TYPE_ALIGN (tem));
      }
      break;

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

      gnu_type = TYPE_MAIN_VARIANT (gnat_to_gnu_type (Etype (gnat_entity)));
      if (!Is_Constrained (gnat_entity))
	;
      else
	{
	  const Entity_Id PAT = Packed_Array_Impl_Type (gnat_entity);
	  Entity_Id gnat_index, gnat_base_index;
	  const bool convention_fortran_p
	    = (Convention (gnat_entity) == Convention_Fortran);
	  const int ndim = Number_Dimensions (gnat_entity);
	  tree gnu_base_type = gnu_type;
	  tree *gnu_index_types = XALLOCAVEC (tree, ndim);
	  tree gnu_max_size = size_one_node;
	  bool need_index_type_struct = false;
	  int index;

	  /* First create the GCC type for each index and find out whether
	     special types are needed for debugging information.  */
	  for (index = (convention_fortran_p ? ndim - 1 : 0),
	       gnat_index = First_Index (gnat_entity),
	       gnat_base_index
		 = First_Index (Implementation_Base_Type (gnat_entity));
	       IN_RANGE (index, 0, ndim - 1);
	       index += (convention_fortran_p ? - 1 : 1),
	       gnat_index = Next_Index (gnat_index),
	       gnat_base_index = Next_Index (gnat_base_index))
	    {
	      tree gnu_index_type = get_unpadded_type (Etype (gnat_index));
	      tree gnu_orig_min = TYPE_MIN_VALUE (gnu_index_type);
	      tree gnu_orig_max = TYPE_MAX_VALUE (gnu_index_type);
	      tree gnu_index_base_type = get_base_type (gnu_index_type);
	      tree gnu_base_index_type
		= get_unpadded_type (Etype (gnat_base_index));
	      tree gnu_base_orig_min = TYPE_MIN_VALUE (gnu_base_index_type);
	      tree gnu_base_orig_max = TYPE_MAX_VALUE (gnu_base_index_type);
	      tree gnu_min, gnu_max, gnu_high;

	      /* We try to create subtypes for discriminants used as bounds
		 that are more restrictive than those declared, by using the
		 bounds of the index type of the base array type.  This will
		 make it possible to calculate the maximum size of the record
		 type more conservatively.  This may have already been done by
		 the front-end (Exp_Ch3.Adjust_Discriminants), in which case
		 there will be a conversion that needs to be removed first.  */
	      if (CONTAINS_PLACEHOLDER_P (gnu_orig_min)
		  && TYPE_RM_SIZE (gnu_base_index_type)
		  && tree_int_cst_lt (TYPE_RM_SIZE (gnu_base_index_type),
				      TYPE_RM_SIZE (gnu_index_type)))
		{
		  gnu_orig_min = remove_conversions (gnu_orig_min, false);
		  TREE_TYPE (gnu_orig_min)
		    = create_extra_subtype (TREE_TYPE (gnu_orig_min),
					    gnu_base_orig_min,
					    gnu_base_orig_max);
		}

	      if (CONTAINS_PLACEHOLDER_P (gnu_orig_max)
		  && TYPE_RM_SIZE (gnu_base_index_type)
		  && tree_int_cst_lt (TYPE_RM_SIZE (gnu_base_index_type),
				      TYPE_RM_SIZE (gnu_index_type)))
		{
		  gnu_orig_max = remove_conversions (gnu_orig_max, false);
		  TREE_TYPE (gnu_orig_max)
		    = create_extra_subtype (TREE_TYPE (gnu_orig_max),
					    gnu_base_orig_min,
					    gnu_base_orig_max);
		}

	      /* Update the maximum size of the array in elements.  Here we
		 see if any constraint on the index type of the base type
		 can be used in the case of self-referential bounds on the
		 index type of the array type. We look for a non-"infinite"
		 and non-self-referential bound from any type involved and
		 handle each bound separately.  */
	      if (gnu_max_size)
		{
		  if (CONTAINS_PLACEHOLDER_P (gnu_orig_min))
		    gnu_min = gnu_base_orig_min;
		  else
		    gnu_min = gnu_orig_min;

		  if (TREE_CODE (gnu_min) != INTEGER_CST
		      || TREE_OVERFLOW (gnu_min))
		    gnu_min = TYPE_MIN_VALUE (TREE_TYPE (gnu_min));

		  if (CONTAINS_PLACEHOLDER_P (gnu_orig_max))
		    gnu_max = gnu_base_orig_max;
		  else
		    gnu_max = gnu_orig_max;

		  if (TREE_CODE (gnu_max) != INTEGER_CST
		      || TREE_OVERFLOW (gnu_max))
		    gnu_max = TYPE_MAX_VALUE (TREE_TYPE (gnu_max));

		  gnu_max_size
		    = update_n_elem (gnu_max_size, gnu_min, gnu_max);
		}

	      /* Convert the bounds to the base type for consistency below.  */
	      gnu_index_base_type = maybe_character_type (gnu_index_base_type);
	      gnu_orig_min = convert (gnu_index_base_type, gnu_orig_min);
	      gnu_orig_max = convert (gnu_index_base_type, gnu_orig_max);

	      gnu_min = convert (sizetype, gnu_orig_min);
	      gnu_max = convert (sizetype, gnu_orig_max);

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
				     fold_build2 (MINUS_EXPR,
						  gnu_index_base_type,
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
		        && range_cannot_be_superflat (gnat_index))
		       /* Bit-Packed Array Impl. Types are never superflat.  */
		       || (Is_Packed_Array_Impl_Type (gnat_entity)
			   && Is_Bit_Packed_Array
			      (Original_Array_Type (gnat_entity))))
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
				       int_const_binop (PLUS_EXPR, gnu_max,
							size_one_node));
		}

	      /* Finally we use (hb >= lb) ? hb : lb - 1 for the upper bound
		 in all the other cases.  Note that we use int_const_binop for
		 the shift by 1 if the bound is constant to avoid any unwanted
		 overflow.  */
	      else
		gnu_high
		  = build_cond_expr (sizetype,
				     build_binary_op (GE_EXPR,
						      boolean_type_node,
						      gnu_orig_max,
						      gnu_orig_min),
				     gnu_max,
				     TREE_CODE (gnu_min) == INTEGER_CST
				     ? int_const_binop (MINUS_EXPR, gnu_min,
							size_one_node)
				     : size_binop (MINUS_EXPR, gnu_min,
						   size_one_node));

	      /* Reuse the index type for the range type.  Then make an index
		 type with the size range in sizetype.  */
	      gnu_index_types[index]
		= create_index_type (gnu_min, gnu_high, gnu_index_type,
				     gnat_entity);

	      /* We need special types for debugging information to point to
		 the index types if they have variable bounds, are not integer
		 types, are biased or are wider than sizetype.  These are GNAT
		 encodings, so we have to include them only when all encodings
		 are requested.  */
	      if ((TREE_CODE (gnu_orig_min) != INTEGER_CST
		   || TREE_CODE (gnu_orig_max) != INTEGER_CST
		   || TREE_CODE (gnu_index_type) != INTEGER_TYPE
		   || (TREE_TYPE (gnu_index_type)
		       && TREE_CODE (TREE_TYPE (gnu_index_type))
			  != INTEGER_TYPE)
		   || TYPE_BIASED_REPRESENTATION_P (gnu_index_type))
		  && gnat_encodings == DWARF_GNAT_ENCODINGS_ALL)
		need_index_type_struct = true;
	    }

	  /* Then flatten: create the array of arrays.  For an array type
	     used to implement a packed array, get the component type from
	     the original array type since the representation clauses that
	     can affect it are on the latter.  */
	  if (Is_Packed_Array_Impl_Type (gnat_entity)
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

	  /* Compute the maximum size of the array in units.  */
	  if (gnu_max_size)
	    gnu_max_size
	      = size_binop (MULT_EXPR, gnu_max_size, TYPE_SIZE_UNIT (gnu_type));

	  /* Now build the array type.  */
	  for (index = ndim - 1; index >= 0; index --)
	    {
	      gnu_type = build_nonshared_array_type (gnu_type,
						     gnu_index_types[index]);
	      TYPE_MULTI_ARRAY_P (gnu_type) = (index > 0);
	      TYPE_CONVENTION_FORTRAN_P (gnu_type) = convention_fortran_p;
	      if (index == ndim - 1 && Reverse_Storage_Order (gnat_entity))
		set_reverse_storage_order_on_array_type (gnu_type);
	      if (array_type_has_nonaliased_component (gnu_type, gnat_entity))
		set_nonaliased_component_on_array_type (gnu_type);

	      /* Kludge to remove the TREE_OVERFLOW flag for the sake of LTO
		 on maximally-sized array types designed by access types.  */
	      if (integer_zerop (TYPE_SIZE (gnu_type))
		  && TREE_OVERFLOW (TYPE_SIZE (gnu_type))
		  && Is_Itype (gnat_entity)
		  && (gnat_temp = Associated_Node_For_Itype (gnat_entity))
		  && IN (Nkind (gnat_temp), N_Declaration)
		  && Is_Access_Type (Defining_Entity (gnat_temp))
		  && Is_Entity_Name (First_Index (gnat_entity))
		  && UI_To_Int (RM_Size (Entity (First_Index (gnat_entity))))
		     == BITS_PER_WORD)
		{
		  TYPE_SIZE (gnu_type) = bitsize_zero_node;
		  TYPE_SIZE_UNIT (gnu_type) = size_zero_node;
		}
	    }

	  /* Attach the TYPE_STUB_DECL in case we have a parallel type.  */
	  TYPE_STUB_DECL (gnu_type)
	    = create_type_stub_decl (gnu_entity_name, gnu_type);

	  /* If this is a multi-dimensional array and we are at global level,
	     we need to make a variable corresponding to the stride of the
	     inner dimensions.   */
	  if (ndim > 1 && global_bindings_p ())
	    {
	      tree gnu_arr_type;

	      for (gnu_arr_type = TREE_TYPE (gnu_type), index = 1;
		   TREE_CODE (gnu_arr_type) == ARRAY_TYPE;
		   gnu_arr_type = TREE_TYPE (gnu_arr_type), index++)
		{
		  tree eltype = TREE_TYPE (gnu_arr_type);
		  char stride_name[32];

		  sprintf (stride_name, "ST%d", index);
		  TYPE_SIZE (gnu_arr_type)
		    = elaborate_expression_1 (TYPE_SIZE (gnu_arr_type),
					      gnat_entity, stride_name,
					      definition, false);

		  /* ??? For now, store the size as a multiple of the
		     alignment of the element type in bytes so that we
		     can see the alignment from the tree.  */
		  sprintf (stride_name, "ST%d_A_UNIT", index);
		  TYPE_SIZE_UNIT (gnu_arr_type)
		    = elaborate_expression_2 (TYPE_SIZE_UNIT (gnu_arr_type),
					      gnat_entity, stride_name,
					      definition, false,
					      TYPE_ALIGN (eltype));

		  /* ??? create_type_decl is not invoked on the inner types so
		     the MULT_EXPR node built above will never be marked.  */
		  MARK_VISITED (TYPE_SIZE_UNIT (gnu_arr_type));
		}
	    }

	  /* Set the TYPE_PACKED flag on packed array types and also on their
	     implementation types, so that the DWARF back-end can output the
	     appropriate description for them.  */
	  TYPE_PACKED (gnu_type)
	    = (Is_Packed (gnat_entity)
	       || Is_Packed_Array_Impl_Type (gnat_entity));

	  TYPE_BIT_PACKED_ARRAY_TYPE_P (gnu_type)
	    = (Is_Packed_Array_Impl_Type (gnat_entity)
	       && Is_Bit_Packed_Array (Original_Array_Type (gnat_entity)));

	  /* If the maximum size doesn't overflow, use it.  */
	  if (gnu_max_size
	      && TREE_CODE (gnu_max_size) == INTEGER_CST
	      && !TREE_OVERFLOW (gnu_max_size)
	      && compare_tree_int (gnu_max_size, TYPE_ARRAY_SIZE_LIMIT) <= 0)
	    TYPE_ARRAY_MAX_SIZE (gnu_type) = gnu_max_size;

	  /* If we need to write out a record type giving the names of the
	     bounds for debugging purposes, do it now and make the record
	     type a parallel type.  This is not needed for a packed array
	     since the bounds are conveyed by the original array type.  */
	  if (need_index_type_struct
	      && debug_info_p
	      && !Is_Packed_Array_Impl_Type (gnat_entity))
	    {
	      tree gnu_bound_rec = make_node (RECORD_TYPE);
	      tree gnu_field_list = NULL_TREE;
	      tree gnu_field;

	      TYPE_NAME (gnu_bound_rec)
		= create_concat_name (gnat_entity, "XA");

	      for (index = ndim - 1; index >= 0; index--)
		{
		  tree gnu_index = TYPE_INDEX_TYPE (gnu_index_types[index]);
		  tree gnu_index_name = TYPE_IDENTIFIER (gnu_index);

		  /* Make sure to reference the types themselves, and not just
		     their names, as the debugger may fall back on them.  */
		  gnu_field = create_field_decl (gnu_index_name, gnu_index,
						 gnu_bound_rec, NULL_TREE,
						 NULL_TREE, 0, 0);
		  DECL_CHAIN (gnu_field) = gnu_field_list;
		  gnu_field_list = gnu_field;
		}

	      finish_record_type (gnu_bound_rec, gnu_field_list, 0, true);
	      add_parallel_type (gnu_type, gnu_bound_rec);
	    }

	  /* If this is a packed array type, make the original array type a
	     parallel/debug type.  Otherwise, if GNAT encodings are used, do
	     it for the base array type if it is not artificial to make sure
	     that it is kept in the debug info.  */
	  if (debug_info_p)
	    {
	      if (Is_Packed_Array_Impl_Type (gnat_entity))
		{
		  tree gnu_name
		    = associate_original_type_to_packed_array (gnu_type,
							       gnat_entity);
		  if (gnu_name)
		    gnu_entity_name = gnu_name;
		}

	      else if (gnat_encodings == DWARF_GNAT_ENCODINGS_ALL)
		{
		  tree gnu_base_decl
		    = gnat_to_gnu_entity (Etype (gnat_entity), NULL_TREE,
					  false);

		  if (!DECL_ARTIFICIAL (gnu_base_decl))
		    add_parallel_type (gnu_type,
				       TREE_TYPE (TREE_TYPE (gnu_base_decl)));
		}
	    }

	  /* Set our alias set to that of our base type.  This gives all
	     array subtypes the same alias set.  */
	  relate_alias_sets (gnu_type, gnu_base_type, ALIAS_SET_COPY);

	  /* If this is a packed type implemented specially, then replace our
	     type with the implementation type.  */
	  if (Present (PAT))
	    {
	      /* First finish the type we had been making so that we output
		 debugging information for it.  */
	      process_attributes (&gnu_type, &attr_list, false, gnat_entity);
	      if (Treat_As_Volatile (gnat_entity))
		{
		  const int quals
		    = TYPE_QUAL_VOLATILE
		      | (Is_Full_Access (gnat_entity) ? TYPE_QUAL_ATOMIC : 0);
		  gnu_type = change_qualified_type (gnu_type, quals);
		}
	      /* Make it artificial only if the base type was artificial too.
		 That's sort of "morally" true and will make it possible for
		 the debugger to look it up by name in DWARF, which is needed
		 in order to decode the packed array type.  */
	      tree gnu_tmp_decl
		= create_type_decl (gnu_entity_name, gnu_type,
				    !Comes_From_Source (Etype (gnat_entity))
				    && artificial_p, debug_info_p,
				    gnat_entity);
	      /* Save it as our equivalent in case the call below elaborates
		 this type again.  */
	      save_gnu_tree (gnat_entity, gnu_tmp_decl, false);

	      gnu_type = gnat_to_gnu_type (PAT);
	      save_gnu_tree (gnat_entity, NULL_TREE, false);

	      /* Set the ___XP suffix for GNAT encodings.  */
	      if (gnat_encodings == DWARF_GNAT_ENCODINGS_ALL)
		gnu_entity_name = DECL_NAME (TYPE_NAME (gnu_type));

	      tree gnu_inner = gnu_type;
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
			gnu_inner
			  = create_extra_subtype (gnu_inner,
						  TYPE_MIN_VALUE (gnu_inner),
						  TYPE_MAX_VALUE (gnu_inner));

		      TYPE_HAS_ACTUAL_BOUNDS_P (gnu_inner) = 1;

		      /* Check for other cases of overloading.  */
		      gcc_checking_assert (!TYPE_ACTUAL_BOUNDS (gnu_inner));
		    }

		  for (Entity_Id gnat_index = First_Index (gnat_entity);
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
	}
      break;

    case E_String_Literal_Subtype:
      /* Create the type for a string literal.  */
      {
	Entity_Id gnat_full_type
	  = (Is_Private_Type (Etype (gnat_entity))
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
	tree gnu_length
	  = UI_To_gnu (String_Literal_Length (gnat_entity),
		       gnu_string_index_type);
	tree gnu_upper_bound
	  = build_binary_op (PLUS_EXPR, gnu_string_index_type,
			     gnu_lower_bound,
			     int_const_binop (MINUS_EXPR, gnu_length,
					      convert (gnu_string_index_type,
						       integer_one_node)));
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
	  set_nonaliased_component_on_array_type (gnu_type);
	relate_alias_sets (gnu_type, gnu_string_type, ALIAS_SET_COPY);
      }
      break;

    /* Record Types and Subtypes

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
      {
	Node_Id record_definition = Type_Definition (gnat_decl);

	if (Has_Complex_Representation (gnat_entity))
	  {
	    const Node_Id first_component
	      = First (Component_Items (Component_List (record_definition)));
	    tree gnu_component_type
	      = get_unpadded_type (Etype (Defining_Entity (first_component)));
	    gnu_type = build_complex_type (gnu_component_type);
	    break;
	  }

	Node_Id gnat_constr;
	Entity_Id gnat_field, gnat_parent_type;
	tree gnu_field, gnu_field_list = NULL_TREE;
	tree gnu_get_parent;
	/* Set PACKED in keeping with gnat_to_gnu_field.  */
	const int packed
	  = Is_Packed (gnat_entity)
	    ? 1
	    : Component_Alignment (gnat_entity) == Calign_Storage_Unit
	      ? -1
	      : 0;
	const bool has_align = Known_Alignment (gnat_entity);
	const bool has_discr = Has_Discriminants (gnat_entity);
	const bool is_extension
	  = (Is_Tagged_Type (gnat_entity)
	     && Nkind (record_definition) == N_Derived_Type_Definition);
	const bool has_rep
	  = is_extension
	    ? Has_Record_Rep_Clause (gnat_entity)
	    : Has_Specified_Layout (gnat_entity);
	const bool is_unchecked_union = Is_Unchecked_Union (gnat_entity);
	bool all_rep = has_rep;

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

	    gcc_assert (Present (Parent_Subtype (gnat_entity))
		        || type_annotate_only);
	  }

	/* Make a node for the record type.  */
	gnu_type = make_node (tree_code_for_record_type (gnat_entity));
	TYPE_NAME (gnu_type) = gnu_entity_name;
	TYPE_PACKED (gnu_type) = (packed != 0) || has_align || has_rep;
	TYPE_REVERSE_STORAGE_ORDER (gnu_type)
	  = Reverse_Storage_Order (gnat_entity);

	/* If the record type has discriminants, pointers to it may also point
	   to constrained subtypes of it, so mark it as may_alias for LTO.  */
	if (has_discr)
	  prepend_one_attribute
	    (&attr_list, ATTR_MACHINE_ATTRIBUTE,
	     get_identifier ("may_alias"), NULL_TREE,
	     gnat_entity);

	process_attributes (&gnu_type, &attr_list, true, gnat_entity);

	/* If we are not defining it, suppress expanding incomplete types.  */
	if (!definition)
	  {
	    defer_incomplete_level++;
	    this_deferred = true;
	  }

	/* If both a size and rep clause were specified, put the size on
	   the record type now so that it can get the proper layout.  */
	if (has_rep && Known_RM_Size (gnat_entity))
	  TYPE_SIZE (gnu_type)
	    = UI_To_gnu (RM_Size (gnat_entity), bitsizetype);

	/* Always set the alignment on the record type here so that it can
	   get the proper layout.  */
	if (has_align)
	  SET_TYPE_ALIGN (gnu_type,
			  validate_alignment (Alignment (gnat_entity),
					      gnat_entity, 0));
	else
	  {
	    SET_TYPE_ALIGN (gnu_type, 0);

	    /* If a type needs strict alignment, then its type size will also
	       be the RM size (see below).  Cap the alignment if needed, lest
	       it may cause this type size to become too large.  */
	    if (Strict_Alignment (gnat_entity) && Known_RM_Size (gnat_entity))
	      {
		unsigned int max_size = UI_To_Int (RM_Size (gnat_entity));
		unsigned int max_align = max_size & -max_size;
		if (max_align < BIGGEST_ALIGNMENT)
		  TYPE_MAX_ALIGN (gnu_type) = max_align;
	      }

	    /* Similarly if an Object_Size clause has been specified.  */
	    else if (Known_Esize (gnat_entity))
	      {
		unsigned int max_size = UI_To_Int (Esize (gnat_entity));
		unsigned int max_align = max_size & -max_size;
		if (max_align < BIGGEST_ALIGNMENT)
		  TYPE_MAX_ALIGN (gnu_type) = max_align;
	      }
	  }

	/* If we have a Parent_Subtype, make a field for the parent.  If
	   this record has rep clauses, force the position to zero.  */
	if (Present (Parent_Subtype (gnat_entity)))
	  {
	    Entity_Id gnat_parent = Parent_Subtype (gnat_entity);
	    tree gnu_dummy_parent_type = make_node (RECORD_TYPE);
	    tree gnu_parent;
	    int parent_packed = 0;

	    /* A major complexity here is that the parent subtype will
	       reference our discriminants in its Stored_Constraint list.
	       But those must reference the parent component of this record
	       which is precisely of the parent subtype we have not built yet!
	       To break the circle we first build a dummy COMPONENT_REF which
	       represents the "get to the parent" operation and initialize
	       each of those discriminants to a COMPONENT_REF of the above
	       dummy parent referencing the corresponding discriminant of the
	       base type of the parent subtype.  */
	    gnu_get_parent = build3 (COMPONENT_REF, gnu_dummy_parent_type,
				     build0 (PLACEHOLDER_EXPR, gnu_type),
				     build_decl (input_location,
						 FIELD_DECL, NULL_TREE,
						 gnu_dummy_parent_type),
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
		  gnat_to_gnu_entity (gnat_uview, NULL_TREE, false);

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

	    /* The parent field needs strict alignment so, if it is to
	       be created with a component clause below, then we need
	       to apply the same adjustment as in gnat_to_gnu_field.  */
	    if (has_rep && TYPE_ALIGN (gnu_type) < TYPE_ALIGN (gnu_parent))
	      {
		/* ??? For historical reasons, we do it on strict-alignment
		   platforms only, where it is really required.  This means
		   that a confirming representation clause will change the
		   behavior of the compiler on the other platforms.  */
		if (STRICT_ALIGNMENT)
		  SET_TYPE_ALIGN (gnu_type, TYPE_ALIGN (gnu_parent));
		else
		  parent_packed
		    = adjust_packed (gnu_parent, gnu_type, parent_packed);
	      }

	    /* Finally we fix up both kinds of twisted COMPONENT_REF we have
	       initially built.  The discriminants must reference the fields
	       of the parent subtype and not those of its base type for the
	       placeholder machinery to properly work.  */
	    if (has_discr)
	      {
		/* The actual parent subtype is the full view.  */
		if (Is_Private_Type (gnat_parent))
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
		      Entity_Id field;
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
				   parent_packed, 1);
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
	      if (is_extension
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

	/* If we have a derived untagged type that renames discriminants in
	   the parent type, the (stored) discriminants are just a copy of the
	   discriminants of the parent type.  This means that any constraints
	   added by the renaming in the derivation are disregarded as far as
	   the layout of the derived type is concerned.  To rescue them, we
	   change the type of the (stored) discriminants to a subtype with
	   the bounds of the type of the visible discriminants.  */
	if (has_discr
	    && !is_extension
	    && Stored_Constraint (gnat_entity) != No_Elist)
	  for (gnat_constr = First_Elmt (Stored_Constraint (gnat_entity));
	       gnat_constr != No_Elmt;
	       gnat_constr = Next_Elmt (gnat_constr))
	    if (Nkind (Node (gnat_constr)) == N_Identifier
		/* Ignore access discriminants.  */
		&& !Is_Access_Type (Etype (Node (gnat_constr)))
		&& Ekind (Entity (Node (gnat_constr))) == E_Discriminant)
	      {
		const Entity_Id gnat_discr = Entity (Node (gnat_constr));
		tree gnu_discr_type = gnat_to_gnu_type (Etype (gnat_discr));
		tree gnu_ref
		  = gnat_to_gnu_entity (Original_Record_Component (gnat_discr),
					NULL_TREE, false);

		/* GNU_REF must be an expression using a PLACEHOLDER_EXPR built
		   just above for one of the stored discriminants.  */
		gcc_assert (TREE_TYPE (TREE_OPERAND (gnu_ref, 0)) == gnu_type);

		if (gnu_discr_type != TREE_TYPE (gnu_ref))
		  TREE_TYPE (gnu_ref)
		    = create_extra_subtype (TREE_TYPE (gnu_ref),
					    TYPE_MIN_VALUE (gnu_discr_type),
					    TYPE_MAX_VALUE (gnu_discr_type));
	      }

	/* If this is a derived type with discriminants and these discriminants
	   affect the initial shape it has inherited, factor them in.  */
	if (has_discr
	    && !is_extension
	    && !Has_Record_Rep_Clause (gnat_entity)
	    && Stored_Constraint (gnat_entity) != No_Elist
	    && (gnat_parent_type = Underlying_Type (Etype (gnat_entity)))
	    && Is_Record_Type (gnat_parent_type)
	    && Is_Unchecked_Union (gnat_entity)
	       == Is_Unchecked_Union (gnat_parent_type)
	    && No_Reordering (gnat_entity) == No_Reordering (gnat_parent_type))
	  {
	    tree gnu_parent_type
	      = TYPE_MAIN_VARIANT (gnat_to_gnu_type (gnat_parent_type));

	    if (TYPE_IS_PADDING_P (gnu_parent_type))
	      gnu_parent_type = TREE_TYPE (TYPE_FIELDS (gnu_parent_type));

	    vec<subst_pair> gnu_subst_list
	      = build_subst_list (gnat_entity, gnat_parent_type, definition);

	    /* Set the layout of the type to match that of the parent type,
	       doing required substitutions.  Note that, if we do not use the
	       GNAT encodings, we don't need debug info for the inner record
	       types, as they will be part of the embedding variant record's
	       debug info.  */
	    copy_and_substitute_in_layout
	      (gnat_entity, gnat_parent_type, gnu_type, gnu_parent_type,
	       gnu_subst_list,
	       debug_info_p && gnat_encodings == DWARF_GNAT_ENCODINGS_ALL);
	  }
	else
	  {
	    /* Add the fields into the record type and finish it up.  */
	    components_to_record (Component_List (record_definition),
				  gnat_entity, gnu_field_list, gnu_type,
				  packed, definition, false, all_rep,
				  is_unchecked_union, artificial_p,
				  debug_info_p, false,
				  all_rep ? NULL_TREE : bitsize_zero_node,
				  NULL);

	    /* Empty classes have the size of a storage unit in C++.  */
	    if (TYPE_SIZE (gnu_type) == bitsize_zero_node
		&& Convention (gnat_entity) == Convention_CPP)
	      {
		TYPE_SIZE (gnu_type) = bitsize_unit_node;
		TYPE_SIZE_UNIT (gnu_type) = size_one_node;
		compute_record_mode (gnu_type);
	      }

	    /* If the type needs strict alignment, then no object of the type
	       may have a size smaller than the natural size, which means that
	       the RM size of the type is equal to the type size.  */
	    if (Strict_Alignment (gnat_entity))
	      SET_TYPE_ADA_SIZE (gnu_type, TYPE_SIZE (gnu_type));

	    /* If there are entities in the chain corresponding to components
	       that we did not elaborate, ensure we elaborate their types if
	       they are itypes.  */
	    for (gnat_temp = First_Entity (gnat_entity);
		 Present (gnat_temp);
		 gnat_temp = Next_Entity (gnat_temp))
	      if ((Ekind (gnat_temp) == E_Component
		   || Ekind (gnat_temp) == E_Discriminant)
		  && Is_Itype (Etype (gnat_temp))
		  && !present_gnu_tree (gnat_temp))
		gnat_to_gnu_entity (Etype (gnat_temp), NULL_TREE, false);
	  }

	/* Fill in locations of fields.  */
	annotate_rep (gnat_entity, gnu_type);
      }
      break;

    case E_Class_Wide_Subtype:
      /* If an equivalent type is present, that is what we should use.
	 Otherwise, fall through to handle this like a record subtype
	 since it may have constraints.  */
      if (gnat_equiv_type != gnat_entity)
	{
	  gnu_decl = gnat_to_gnu_entity (gnat_equiv_type, NULL_TREE, false);
	  maybe_present = true;
	  break;
	}

      /* ... fall through ... */

    case E_Record_Subtype:
      /* If Cloned_Subtype is Present it means this record subtype has
	 identical layout to that type or subtype and we should use
	 that GCC type for this one.  The front-end guarantees that
	 the component list is shared.  */
      if (Present (Cloned_Subtype (gnat_entity)))
	{
	  gnu_decl = gnat_to_gnu_entity (Cloned_Subtype (gnat_entity),
					 NULL_TREE, false);
	  gnat_annotate_type = Cloned_Subtype (gnat_entity);
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

	  if (!definition)
	    {
	      defer_incomplete_level++;
	      this_deferred = true;
	    }

	  tree gnu_base_type
	    = TYPE_MAIN_VARIANT (gnat_to_gnu_type (gnat_base_type));

	  if (present_gnu_tree (gnat_entity))
	    {
	      maybe_present = true;
	      break;
	    }

	  /* When the subtype has discriminants and these discriminants affect
	     the initial shape it has inherited, factor them in.  But for an
	     Unchecked_Union (it must be an itype), just return the type.  */
	  if (Has_Discriminants (gnat_entity)
	      && Stored_Constraint (gnat_entity) != No_Elist
	      && Is_Record_Type (gnat_base_type)
	      && !Is_Unchecked_Union (gnat_base_type))
	    {
	      vec<subst_pair> gnu_subst_list
		= build_subst_list (gnat_entity, gnat_base_type, definition);
	      tree gnu_unpad_base_type;

	      gnu_type = make_node (RECORD_TYPE);
	      TYPE_NAME (gnu_type) = gnu_entity_name;
	      TYPE_PACKED (gnu_type) = TYPE_PACKED (gnu_base_type);
	      TYPE_REVERSE_STORAGE_ORDER (gnu_type)
		= Reverse_Storage_Order (gnat_entity);
	      process_attributes (&gnu_type, &attr_list, true, gnat_entity);

	      /* Set the size, alignment and alias set of the type to match
		 those of the base type, doing required substitutions.  */
	      copy_and_substitute_in_size (gnu_type, gnu_base_type,
					   gnu_subst_list);

	      if (TYPE_IS_PADDING_P (gnu_base_type))
		gnu_unpad_base_type = TREE_TYPE (TYPE_FIELDS (gnu_base_type));
	      else
		gnu_unpad_base_type = gnu_base_type;

	      /* Set the layout of the type to match that of the base type,
	         doing required substitutions.  We will output debug info
	         manually below so pass false as last argument.  */
	      copy_and_substitute_in_layout (gnat_entity, gnat_base_type,
					     gnu_type, gnu_unpad_base_type,
					     gnu_subst_list, false);

	      /* Fill in locations of fields.  */
	      annotate_rep (gnat_entity, gnu_type);

	      /* If debugging information is being written for the type and if
		 we are asked to output GNAT encodings, write a record that
		 shows what we are a subtype of and also make a variable that
		 indicates our size, if still variable.  */
	      if (debug_info_p
		  && gnat_encodings == DWARF_GNAT_ENCODINGS_ALL)
		{
		  tree gnu_subtype_marker = make_node (RECORD_TYPE);
		  tree gnu_unpad_base_name
		    = TYPE_IDENTIFIER (gnu_unpad_base_type);
		  tree gnu_size_unit = TYPE_SIZE_UNIT (gnu_type);

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

		  add_parallel_type (gnu_type, gnu_subtype_marker);

		  if (definition
		      && TREE_CODE (gnu_size_unit) != INTEGER_CST
		      && !CONTAINS_PLACEHOLDER_P (gnu_size_unit))
		    TYPE_SIZE_UNIT (gnu_subtype_marker)
		      = create_var_decl (create_concat_name (gnat_entity,
							     "XVZ"),
					 NULL_TREE, sizetype, gnu_size_unit,
					 true, false, false, false, false,
					 true, true, NULL, gnat_entity, false);
		}

	      /* Or else, if the subtype is artificial and GNAT encodings are
		 not used, use the base record type as the debug type.  */
	      else if (debug_info_p
		       && artificial_p
		       && gnat_encodings != DWARF_GNAT_ENCODINGS_ALL)
		SET_TYPE_DEBUG_TYPE (gnu_type, gnu_unpad_base_type);
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
    case E_Anonymous_Access_Subprogram_Type:
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
	const bool is_from_limited_with
	  = (Is_Incomplete_Type (gnat_desig_equiv)
	     && From_Limited_With (gnat_desig_equiv));
	/* Whether it is a completed Taft Amendment type.  Such a type is to
	   be treated as coming from a limited with clause if it is not in
	   the main unit, i.e. we break potential circularities here in case
	   the body of an external unit is loaded for inter-unit inlining.  */
        const bool is_completed_taft_type
	  = (Is_Incomplete_Type (gnat_desig_equiv)
	     && Has_Completion_In_Body (gnat_desig_equiv)
	     && Present (Full_View (gnat_desig_equiv)));
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
	     : (Is_Incomplete_Or_Private_Type (gnat_desig_equiv)
		? Full_View (gnat_desig_equiv) : Empty));
	Entity_Id gnat_desig_full_direct
	  = ((is_from_limited_with
	      && Present (gnat_desig_full_direct_first)
	      && Is_Private_Type (gnat_desig_full_direct_first))
	     ? Full_View (gnat_desig_full_direct_first)
	     : gnat_desig_full_direct_first);
	Entity_Id gnat_desig_full
	  = Gigi_Equivalent_Type (gnat_desig_full_direct);
	/* The type actually used to represent the designated type, either
	   gnat_desig_full or gnat_desig_equiv.  */
	Entity_Id gnat_desig_rep;
	/* We want to know if we'll be seeing the freeze node for any
	   incomplete type we may be pointing to.  */
	const bool in_main_unit
	  = (Present (gnat_desig_full)
	     ? In_Extended_Main_Code_Unit (gnat_desig_full)
	     : In_Extended_Main_Code_Unit (gnat_desig_type));
	/* True if we make a dummy type here.  */
	bool made_dummy = false;
	/* The mode to be used for the pointer type.  */
	scalar_int_mode p_mode;
	/* The GCC type used for the designated type.  */
	tree gnu_desig_type = NULL_TREE;

	if (!int_mode_for_size (esize, 0).exists (&p_mode)
	    || !targetm.valid_pointer_mode (p_mode))
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

	/* Set the type that's the representation of the designated type.  */
	gnat_desig_rep
	  = Present (gnat_desig_full) ? gnat_desig_full : gnat_desig_equiv;

	/* If we already know what the full type is, use it.  */
	if (Present (gnat_desig_full) && present_gnu_tree (gnat_desig_full))
	  gnu_desig_type = TREE_TYPE (get_gnu_tree (gnat_desig_full));

	/* Get the type of the thing we are to point to and build a pointer to
	   it.  If it is a reference to an incomplete or private type with a
	   full view that is a record, an array or an access, make a dummy type
	   and get the actual type later when we have verified it is safe.  */
	else if ((!in_main_unit
		  && !present_gnu_tree (gnat_desig_equiv)
		  && Present (gnat_desig_full)
		  && (Is_Record_Type (gnat_desig_full)
		      || Is_Array_Type (gnat_desig_full)
		      || Is_Access_Type (gnat_desig_full)))
		 /* Likewise if this is a reference to a record, an array or a
		    subprogram type and we are to defer elaborating incomplete
		    types.  We do this because this access type may be the full
		    view of a private type.  */
		 || ((!in_main_unit || imported_p)
		     && defer_incomplete_level != 0
		     && !present_gnu_tree (gnat_desig_equiv)
		     && (Is_Record_Type (gnat_desig_rep)
			 || Is_Array_Type (gnat_desig_rep)
			 || Ekind (gnat_desig_rep) == E_Subprogram_Type))
		 /* If this is a reference from a limited_with type back to our
		    main unit and there's a freeze node for it, either we have
		    already processed the declaration and made the dummy type,
		    in which case we just reuse the latter, or we have not yet,
		    in which case we make the dummy type and it will be reused
		    when the declaration is finally processed.  In both cases,
		    the pointer eventually created below will be automatically
		    adjusted when the freeze node is processed.  */
		 || (in_main_unit
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
	   is absent, so we use the void pointer type.  */
	else if (type_annotate_only && No (gnat_desig_equiv))
	  gnu_type = ptr_type_node;

	/* If the ultimately designated type is an incomplete type with no full
	   view, we use the void pointer type in LTO mode to avoid emitting a
	   dummy type in the GIMPLE IR.  We cannot do that in regular mode as
	   the name of the dummy type in used by GDB for a global lookup.  */
	else if (Ekind (gnat_desig_rep) == E_Incomplete_Type
		 && No (Full_View (gnat_desig_rep))
		 && flag_generate_lto)
	  gnu_type = ptr_type_node;

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

	/* Access-to-unconstrained-array types need a special treatment.  */
	if (Is_Array_Type (gnat_desig_rep) && !Is_Constrained (gnat_desig_rep))
	  {
	    /* If the processing above got something that has a pointer, then
	       we are done.  This could have happened either because the type
	       was elaborated or because somebody else executed the code.  */
	    if (!TYPE_POINTER_TO (gnu_desig_type))
	      build_dummy_unc_pointer_types (gnat_desig_equiv, gnu_desig_type);

	    gnu_type = TYPE_POINTER_TO (gnu_desig_type);
	  }

	/* If we haven't done it yet, build the pointer type the usual way.  */
	else if (!gnu_type)
	  {
	    /* Modify the designated type if we are pointing only to constant
	       objects, but don't do it for a dummy type.  */
	    if (Is_Access_Constant (gnat_entity)
		&& !TYPE_IS_DUMMY_P (gnu_desig_type))
	      gnu_desig_type
		= change_qualified_type (gnu_desig_type, TYPE_QUAL_CONST);

	    gnu_type
	      = build_pointer_type_for_mode (gnu_desig_type, p_mode,
					     No_Strict_Aliasing (gnat_entity));
	  }

	/* If the designated type is not declared in the main unit and we made
	   a dummy node for it, save our definition, elaborate the actual type
	   and replace the dummy type we made with the actual one.  But if we
	   are to defer actually looking up the actual type, make an entry in
	   the deferred list instead.  If this is from a limited with, we may
	   have to defer until the end of the current unit.  */
	if (!in_main_unit && made_dummy)
	  {
	    if (TYPE_IS_FAT_POINTER_P (gnu_type) && esize == POINTER_SIZE)
	      gnu_type
		= build_pointer_type (TYPE_OBJECT_RECORD_TYPE (gnu_desig_type));

	    process_attributes (&gnu_type, &attr_list, false, gnat_entity);
	    gnu_decl = create_type_decl (gnu_entity_name, gnu_type,
					 artificial_p, debug_info_p,
					 gnat_entity);
	    this_made_decl = true;
	    gnu_type = TREE_TYPE (gnu_decl);
	    save_gnu_tree (gnat_entity, gnu_decl, false);
	    saved = true;

	    if (defer_incomplete_level == 0
		&& !is_from_limited_with
		&& !is_completed_taft_type)
	      {
		update_pointer_to (TYPE_MAIN_VARIANT (gnu_desig_type),
				   gnat_to_gnu_type (gnat_desig_equiv));
	      }
	    else
	      {
		struct incomplete *p = XNEW (struct incomplete);
		struct incomplete **head
		  = (is_from_limited_with || is_completed_taft_type
		     ? &defer_limited_with_list : &defer_incomplete_list);

		p->old_type = gnu_desig_type;
		p->full_type = gnat_desig_equiv;
		p->next = *head;
		*head = p;
	      }
	  }
      }
      break;

    case E_Access_Protected_Subprogram_Type:
    case E_Anonymous_Access_Protected_Subprogram_Type:
      /* If we are just annotating types and have no equivalent record type,
	 just use the void pointer type.  */
      if (type_annotate_only && gnat_equiv_type == gnat_entity)
	gnu_type = ptr_type_node;

      /* The run-time representation is the equivalent type.  */
      else
	{
	  gnu_type = gnat_to_gnu_type (gnat_equiv_type);
	  maybe_present = true;
	}

      /* The designated subtype must be elaborated as well, if it does
	 not have its own freeze node.  */
      if (Is_Itype (Directly_Designated_Type (gnat_entity))
	  && !present_gnu_tree (Directly_Designated_Type (gnat_entity))
	  && No (Freeze_Node (Directly_Designated_Type (gnat_entity)))
	  && !Is_Record_Type (Scope (Directly_Designated_Type (gnat_entity))))
	gnat_to_gnu_entity (Directly_Designated_Type (gnat_entity),
			    NULL_TREE, false);

      break;

    case E_Access_Subtype:
      /* We treat this as identical to its base type; any constraint is
	 meaningful only to the front-end.  */
      gnu_decl = gnat_to_gnu_entity (gnat_equiv_type, NULL_TREE, false);
      maybe_present = true;

      /* The designated subtype must be elaborated as well, if it does
	 not have its own freeze node.  But designated subtypes created
	 for constrained components of records with discriminants are
	 not frozen by the front-end and not elaborated here, because
	 their use may appear before the base type is frozen and it is
	 not clear that they are needed in gigi.  With the current model,
	 there is no correct place where they could be elaborated.  */
      if (Is_Itype (Directly_Designated_Type (gnat_entity))
	  && !present_gnu_tree (Directly_Designated_Type (gnat_entity))
	  && Is_Frozen (Directly_Designated_Type (gnat_entity))
	  && No (Freeze_Node (Directly_Designated_Type (gnat_entity))))
	{
	  /* If we are to defer elaborating incomplete types, make a dummy
	     type node and elaborate it later.  */
	  if (defer_incomplete_level != 0)
	    {
	      struct incomplete *p = XNEW (struct incomplete);

	      p->old_type
		= make_dummy_type (Directly_Designated_Type (gnat_entity));
	      p->full_type = Directly_Designated_Type (gnat_entity);
	      p->next = defer_incomplete_list;
	      defer_incomplete_list = p;
	    }
	  else if (!Is_Incomplete_Or_Private_Type
		      (Base_Type (Directly_Designated_Type (gnat_entity))))
	    gnat_to_gnu_entity (Directly_Designated_Type (gnat_entity),
				NULL_TREE, false);
	}
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
	tree gnu_ext_name
	  = gnu_ext_name_for_subprog (gnat_entity, gnu_entity_name);
	const enum inline_status_t inline_status
	  = inline_status_for_subprog (gnat_entity);
	bool public_flag = Is_Public (gnat_entity) || imported_p;
	/* Subprograms marked both Intrinsic and Always_Inline need not
	   have a body of their own.  */
	bool extern_flag
	  = ((Is_Public (gnat_entity) && !definition)
	     || imported_p
	     || (Is_Intrinsic_Subprogram (gnat_entity)
		 && Has_Pragma_Inline_Always (gnat_entity)));
	tree gnu_param_list;

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
	    const Entity_Id gnat_alias = Alias (gnat_entity);

	    if (Ekind (gnat_alias) == E_Enumeration_Literal)
	      gnat_to_gnu_entity (Etype (gnat_alias), NULL_TREE, false);

	    gnu_decl = gnat_to_gnu_entity (gnat_alias, gnu_expr, false);

	    /* Elaborate any itypes in the parameters of this entity.  */
	    for (gnat_temp = First_Formal_With_Extras (gnat_entity);
		 Present (gnat_temp);
		 gnat_temp = Next_Formal_With_Extras (gnat_temp))
	      if (Is_Itype (Etype (gnat_temp)))
		gnat_to_gnu_entity (Etype (gnat_temp), NULL_TREE, false);

	    /* Materialize renamed subprograms in the debugging information
	       when the renamed object is known at compile time; we consider
	       such renamings as imported declarations.

	       Because the parameters in generic instantiations are generally
	       materialized as renamings, we often end up having both the
	       renamed subprogram and the renaming in the same context and with
	       the same name; in this case, renaming is both useless debug-wise
	       and potentially harmful as name resolution in the debugger could
	       return twice the same entity!  So avoid this case.  */
	    if (debug_info_p
		&& !artificial_p
		&& (Ekind (gnat_alias) == E_Function
		    || Ekind (gnat_alias) == E_Procedure)
		&& !(get_debug_scope (gnat_entity, NULL)
		     == get_debug_scope (gnat_alias, NULL)
		     && Name_Equals (Chars (gnat_entity), Chars (gnat_alias)))
		&& TREE_CODE (gnu_decl) == FUNCTION_DECL)
	      {
		tree decl = build_decl (input_location, IMPORTED_DECL,
					gnu_entity_name, void_type_node);
		IMPORTED_DECL_ASSOCIATED_DECL (decl) = gnu_decl;
		gnat_pushdecl (decl, gnat_entity);
	      }

	    break;
	  }

	/* Get the GCC tree for the (underlying) subprogram type.  If the
	   entity is an actual subprogram, also get the parameter list.  */
	gnu_type
	  = gnat_to_gnu_subprog_type (gnat_entity, definition, debug_info_p,
				      &gnu_param_list);
	if (DECL_P (gnu_type))
	  {
	    gnu_decl = gnu_type;
	    gnu_type = TREE_TYPE (gnu_decl);
	    break;
	  }

	/* Deal with platform-specific calling conventions.  */
	if (Has_Stdcall_Convention (gnat_entity))
	  prepend_one_attribute
	    (&attr_list, ATTR_MACHINE_ATTRIBUTE,
	     get_identifier ("stdcall"), NULL_TREE,
	     gnat_entity);

	/* If we should request stack realignment for a foreign convention
	   subprogram, do so.  Note that this applies to task entry points
	   in particular.  */
	if (FOREIGN_FORCE_REALIGN_STACK && foreign)
	  prepend_one_attribute
	    (&attr_list, ATTR_MACHINE_ATTRIBUTE,
	     get_identifier ("force_align_arg_pointer"), NULL_TREE,
	     gnat_entity);

	/* Deal with a pragma Linker_Section on a subprogram.  */
	if ((kind == E_Function || kind == E_Procedure)
	    && Present (Linker_Section_Pragma (gnat_entity)))
	  prepend_one_attribute_pragma (&attr_list,
					Linker_Section_Pragma (gnat_entity));

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
	       alias everything as per RM 13.3(19).  */
	    gnu_type
	      = build_reference_type_for_mode (gnu_type, ptr_mode, true);
	    if (gnu_address)
	      gnu_address = convert (gnu_type, gnu_address);

	    gnu_decl
	      = create_var_decl (gnu_entity_name, gnu_ext_name, gnu_type,
				 gnu_address, false, Is_Public (gnat_entity),
				 extern_flag, false, false, artificial_p,
				 debug_info_p, NULL, gnat_entity);
	    DECL_BY_REF_P (gnu_decl) = 1;
	  }

	/* If this is a mere subprogram type, just create the declaration.  */
	else if (kind == E_Subprogram_Type)
	  {
	    process_attributes (&gnu_type, &attr_list, false, gnat_entity);

	    gnu_decl
	      = create_type_decl (gnu_entity_name, gnu_type, artificial_p,
				  debug_info_p, gnat_entity);
	  }

	/* Otherwise create the subprogram declaration with the external name,
	   the type and the parameter list.  However, if this a reference to
	   the allocation routines, reuse the canonical declaration nodes as
	   they come with special properties.  */
	else
	  {
	    if (extern_flag && gnu_ext_name == DECL_NAME (malloc_decl))
	      gnu_decl = malloc_decl;
	    else if (extern_flag && gnu_ext_name == DECL_NAME (realloc_decl))
	      gnu_decl = realloc_decl;
	    else
	      {
		gnu_decl
		  = create_subprog_decl (gnu_entity_name, gnu_ext_name,
					 gnu_type, gnu_param_list,
					 inline_status, public_flag,
					 extern_flag, artificial_p,
					 debug_info_p,
					 definition && imported_p, attr_list,
					 gnat_entity);

		DECL_STUBBED_P (gnu_decl)
		  = (Convention (gnat_entity) == Convention_Stubbed);
	      }
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
	const bool is_from_limited_with
	  = (IN (kind, Incomplete_Kind) && From_Limited_With (gnat_entity));
	/* Get the "full view" of this entity.  If this is an incomplete
	   entity from a limited with, treat its non-limited view as the
	   full view.  Otherwise, use either the full view or the underlying
	   full view, whichever is present.  This is used in all the tests
	   below.  */
	const Entity_Id full_view
	  = is_from_limited_with
	    ? Non_Limited_View (gnat_entity)
	    : Present (Full_View (gnat_entity))
	      ? Full_View (gnat_entity)
	      : IN (kind, Private_Kind)
		? Underlying_Full_View (gnat_entity)
		: Empty;

	/* If this is an incomplete type with no full view, it must be a Taft
	   Amendment type or an incomplete type coming from a limited context,
	   in which cases we return a dummy type.  Otherwise, we just get the
	   type from its Etype.  */
	if (No (full_view))
	  {
	    if (kind == E_Incomplete_Type)
	      {
		gnu_type = make_dummy_type (gnat_entity);
		gnu_decl = TYPE_STUB_DECL (gnu_type);
	      }
	    else
	      {
		gnu_decl
		  = gnat_to_gnu_entity (Etype (gnat_entity), NULL_TREE, false);
		maybe_present = true;
	      }
	  }

	/* Or else, if we already made a type for the full view, reuse it.  */
	else if (present_gnu_tree (full_view))
	  gnu_decl = get_gnu_tree (full_view);

	/* Or else, if we are not defining the type or there is no freeze
	   node on it, get the type for the full view.  Likewise if this is
	   a limited_with'ed type not declared in the main unit, which can
	   happen for incomplete formal types instantiated on a type coming
	   from a limited_with clause.  */
	else if (!definition
		 || No (Freeze_Node (full_view))
		 || (is_from_limited_with
		     && !In_Extended_Main_Code_Unit (full_view)))
	  {
	    gnu_decl = gnat_to_gnu_entity (full_view, NULL_TREE, false);
	    maybe_present = true;
	  }

	/* Otherwise, make a dummy type entry which will be replaced later.
	   Save it as the full declaration's type so we can do any needed
	   updates when we see it.  */
	else
	  {
	    gnu_type = make_dummy_type (gnat_entity);
	    gnu_decl = TYPE_STUB_DECL (gnu_type);
	    if (Has_Completion_In_Body (gnat_entity))
	      DECL_TAFT_TYPE_P (gnu_decl) = 1;
	    save_gnu_tree (full_view, gnu_decl, false);
	  }
      }
      break;

    case E_Class_Wide_Type:
      /* Class-wide types are always transformed into their root type.  */
      gnu_decl = gnat_to_gnu_entity (gnat_equiv_type, NULL_TREE, false);
      maybe_present = true;
      break;

    case E_Protected_Type:
    case E_Protected_Subtype:
    case E_Task_Type:
    case E_Task_Subtype:
      /* If we are just annotating types and have no equivalent record type,
	 just return void_type, except for root types that have discriminants
	 because the discriminants will very likely be used in the declarative
	 part of the associated body so they need to be translated.  */
      if (type_annotate_only && gnat_equiv_type == gnat_entity)
	{
	  if (definition
	      && Has_Discriminants (gnat_entity)
	      && Root_Type (gnat_entity) == gnat_entity)
	    {
	      tree gnu_field_list = NULL_TREE;
	      Entity_Id gnat_field;

	      /* This is a minimal version of the E_Record_Type handling.  */
	      gnu_type = make_node (RECORD_TYPE);
	      TYPE_NAME (gnu_type) = gnu_entity_name;

	      for (gnat_field = First_Stored_Discriminant (gnat_entity);
		   Present (gnat_field);
		   gnat_field = Next_Stored_Discriminant (gnat_field))
		{
		  tree gnu_field
		    = gnat_to_gnu_field (gnat_field, gnu_type, false,
					 definition, debug_info_p);

		  save_gnu_tree (gnat_field,
				 build3 (COMPONENT_REF, TREE_TYPE (gnu_field),
					 build0 (PLACEHOLDER_EXPR, gnu_type),
					 gnu_field, NULL_TREE),
				 true);

		  DECL_CHAIN (gnu_field) = gnu_field_list;
		  gnu_field_list = gnu_field;
		}

	      finish_record_type (gnu_type, nreverse (gnu_field_list), 0,
				  false);
	    }
	  else
	    gnu_type = void_type_node;
	}

      /* Concurrent types are always transformed into their record type.  */
      else
	gnu_decl = gnat_to_gnu_entity (gnat_equiv_type, NULL_TREE, false);
      maybe_present = true;
      break;

    case E_Label:
      gnu_decl = create_label_decl (gnu_entity_name, gnat_entity);
      break;

    case E_Block:
    case E_Loop:
      /* Nothing at all to do here, so just return an ERROR_MARK and claim
	 we've already saved it, so we don't try to.  */
      gnu_decl = error_mark_node;
      saved = true;
      break;

    case E_Abstract_State:
      /* This is a SPARK annotation that only reaches here when compiling in
	 ASIS mode.  */
      gcc_assert (type_annotate_only);
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

  /* If we are processing a type and there is either no DECL for it or
     we just made one, do some common processing for the type, such as
     handling alignment and possible padding.  */
  if (is_type && (!gnu_decl || this_made_decl))
    {
      const bool is_by_ref = Is_By_Reference_Type (gnat_entity);

      gcc_assert (!TYPE_IS_DUMMY_P (gnu_type));

      /* Process the attributes, if not already done.  Note that the type is
	 already defined so we cannot pass true for IN_PLACE here.  */
      process_attributes (&gnu_type, &attr_list, false, gnat_entity);

      /* See if a size was specified, by means of either an Object_Size or
         a regular Size clause, and validate it if so.

	 ??? Don't set the size for a String_Literal since it is either
	 confirming or we don't handle it properly (if the low bound is
	 non-constant).  */
      if (!gnu_size && kind != E_String_Literal_Subtype)
	{
	  const char *size_s = "size for %s too small{, minimum allowed is ^}";
	  const char *type_s = is_by_ref ? "by-reference type &" : "&";

	  if (Known_Esize (gnat_entity))
	    gnu_size
	      = validate_size (Esize (gnat_entity), gnu_type, gnat_entity,
			       VAR_DECL, false, false, size_s, type_s);

	  /* ??? The test on Has_Size_Clause must be removed when "unknown" is
	     no longer represented as Uint_0 (i.e. Use_New_Unknown_Rep).  */
	  else if (Known_RM_Size (gnat_entity)
		   || Has_Size_Clause (gnat_entity))
	    gnu_size
	      = validate_size (RM_Size (gnat_entity), gnu_type, gnat_entity,
			       TYPE_DECL, false, Has_Size_Clause (gnat_entity),
			       size_s, type_s);
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
	    gnu_size = NULL_TREE;
	}

      /* If the alignment has not already been processed and this is not
	 an unconstrained array type, see if an alignment is specified.
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
	      if (tree_fits_uhwi_p (size)
		  && align >= tree_to_uhwi (size) * BITS_PER_UNIT)
		post_error_ne ("??suspiciously large alignment specified for&",
			       Expression (Alignment_Clause (gnat_entity)),
			       gnat_entity);
	    }
	}
      else if (Is_Full_Access (gnat_entity) && !gnu_size
	       && tree_fits_uhwi_p (TYPE_SIZE (gnu_type))
	       && integer_pow2p (TYPE_SIZE (gnu_type)))
	align = MIN (BIGGEST_ALIGNMENT,
		     tree_to_uhwi (TYPE_SIZE (gnu_type)));
      else if (Is_Full_Access (gnat_entity) && gnu_size
	       && tree_fits_uhwi_p (gnu_size)
	       && integer_pow2p (gnu_size))
	align = MIN (BIGGEST_ALIGNMENT, tree_to_uhwi (gnu_size));

      /* See if we need to pad the type.  If we did and built a new type,
	 then create a stripped-down declaration for the original type,
	 mainly for debugging, unless there was already one.  */
      if (gnu_size || align > 0)
	{
	  tree orig_type = gnu_type;

	  gnu_type = maybe_pad_type (gnu_type, gnu_size, align, gnat_entity,
				     false, definition, false);

	  if (gnu_type != orig_type && !gnu_decl)
	    create_type_decl (gnu_entity_name, orig_type, true, debug_info_p,
			      gnat_entity);
	}

      /* Now set the RM size of the type.  We cannot do it before padding
	 because we need to accept arbitrary RM sizes on integral types.  */
      if (Known_RM_Size (gnat_entity))
	set_rm_size (RM_Size (gnat_entity), gnu_type, gnat_entity);

      /* Back-annotate the alignment of the type if not already set.  */
      if (!Known_Alignment (gnat_entity))
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

      /* Likewise for the size, if any.  */
      if (!Known_Esize (gnat_entity) && TYPE_SIZE (gnu_type))
	{
	  tree size = TYPE_SIZE (gnu_type);

	  /* If the size is self-referential, annotate the maximum value
	     after saturating it, if need be, to avoid a No_Uint value.  */
	  if (CONTAINS_PLACEHOLDER_P (size))
	    {
	      const unsigned int align
		= UI_To_Int (Alignment (gnat_entity)) * BITS_PER_UNIT;
	      size = maybe_saturate_size (max_size (size, true), align);
	    }

	  /* If we are just annotating types and the type is tagged, the tag
	     and the parent components are not generated by the front-end so
	     alignment and sizes must be adjusted.  */
	  if (type_annotate_only && Is_Tagged_Type (gnat_entity))
	    {
	      const bool derived_p = Is_Derived_Type (gnat_entity);
	      const Entity_Id gnat_parent
		= derived_p ? Etype (Base_Type (gnat_entity)) : Empty;
	      /* The following test for Known_Alignment preserves the old behavior,
		 but is probably wrong. */
	      const unsigned int inherited_align
		= derived_p
		  ? (Known_Alignment (gnat_parent)
		     ? UI_To_Int (Alignment (gnat_parent)) * BITS_PER_UNIT
		     : 0)
		  : POINTER_SIZE;
	      const unsigned int align
		= MAX (TYPE_ALIGN (gnu_type), inherited_align);

	      Set_Alignment (gnat_entity, UI_From_Int (align / BITS_PER_UNIT));

	      /* If there is neither size clause nor representation clause, the
		 sizes need to be adjusted.  */
	      if (!Known_RM_Size (gnat_entity)
		  && !VOID_TYPE_P (gnu_type)
		  && (!TYPE_FIELDS (gnu_type)
		      || integer_zerop (bit_position (TYPE_FIELDS (gnu_type)))))
		{
		  tree offset
		    = derived_p
		      ? UI_To_gnu (Esize (gnat_parent), bitsizetype)
		      : bitsize_int (POINTER_SIZE);
		  if (TYPE_FIELDS (gnu_type))
		    offset
		      = round_up (offset, DECL_ALIGN (TYPE_FIELDS (gnu_type)));
		  size = size_binop (PLUS_EXPR, size, offset);
		}

	      size = maybe_saturate_size (round_up (size, align), align);
	      Set_Esize (gnat_entity, annotate_value (size));

	      /* Tagged types are Strict_Alignment so RM_Size = Esize.  */
	      if (!Known_RM_Size (gnat_entity))
		Set_RM_Size (gnat_entity, Esize (gnat_entity));
	    }

	  /* Otherwise no adjustment is needed.  */
	  else
	    Set_Esize (gnat_entity, No_Uint_To_0 (annotate_value (size)));
	}

      /* Likewise for the RM size, if any.  */
      if (!Known_RM_Size (gnat_entity) && TYPE_SIZE (gnu_type))
	Set_RM_Size (gnat_entity,
		     annotate_value (rm_size (gnu_type)));

      /* If we are at global level, GCC applied variable_size to the size but
	 this has done nothing.  So, if it's not constant or self-referential,
	 call elaborate_expression_1 to make a variable for it rather than
	 calculating it each time.  */
      if (TYPE_SIZE (gnu_type)
	  && !TREE_CONSTANT (TYPE_SIZE (gnu_type))
	  && !CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_type))
	  && global_bindings_p ())
	{
	  tree orig_size = TYPE_SIZE (gnu_type);

	  TYPE_SIZE (gnu_type)
	    = elaborate_expression_1 (TYPE_SIZE (gnu_type), gnat_entity,
				      "SIZE", definition, false);

	  /* ??? For now, store the size as a multiple of the alignment in
	     bytes so that we can see the alignment from the tree.  */
	  TYPE_SIZE_UNIT (gnu_type)
	    = elaborate_expression_2 (TYPE_SIZE_UNIT (gnu_type), gnat_entity,
				      "SIZE_A_UNIT", definition, false,
				      TYPE_ALIGN (gnu_type));

	  /* ??? gnu_type may come from an existing type so the MULT_EXPR node
	     may not be marked by the call to create_type_decl below.  */
	  MARK_VISITED (TYPE_SIZE_UNIT (gnu_type));

	  /* For a record type, deal with the variant part, if any, and handle
	     the Ada size as well.  */
	  if (RECORD_OR_UNION_TYPE_P (gnu_type))
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
						  gnat_entity, "VSIZE",
						  definition, false);

		      /* ??? For now, store the size as a multiple of the
			 alignment in bytes so that we can see the alignment
			 from the tree.  */
		      TYPE_SIZE_UNIT (union_type)
			= elaborate_expression_2 (TYPE_SIZE_UNIT (union_type),
						  gnat_entity, "VSIZE_A_UNIT",
						  definition, false,
						  TYPE_ALIGN (union_type));

		      /* ??? For now, store the offset as a multiple of the
			 alignment in bytes so that we can see the alignment
			 from the tree.  */
		      DECL_FIELD_OFFSET (variant_part)
			= elaborate_expression_2 (offset, gnat_entity,
						  "VOFFSET", definition, false,
						  DECL_OFFSET_ALIGN
						  (variant_part));
		    }

		  DECL_SIZE (variant_part) = TYPE_SIZE (union_type);
		  DECL_SIZE_UNIT (variant_part) = TYPE_SIZE_UNIT (union_type);
		}

	      if (operand_equal_p (ada_size, orig_size, 0))
		ada_size = TYPE_SIZE (gnu_type);
	      else
		ada_size
		  = elaborate_expression_1 (ada_size, gnat_entity, "RM_SIZE",
					    definition, false);
	      SET_TYPE_ADA_SIZE (gnu_type, ada_size);
	    }
	}

      /* Similarly, if this is a record type or subtype at global level, call
	 elaborate_expression_2 on any field position.  Skip any fields that
	 we haven't made trees for to avoid problems with class-wide types.  */
      if (Is_In_Record_Kind (kind) && global_bindings_p ())
	for (gnat_temp = First_Entity (gnat_entity); Present (gnat_temp);
	     gnat_temp = Next_Entity (gnat_temp))
	  if (Ekind (gnat_temp) == E_Component && present_gnu_tree (gnat_temp))
	    {
	      tree gnu_field = get_gnu_tree (gnat_temp);

	      /* ??? For now, store the offset as a multiple of the alignment
		 in bytes so that we can see the alignment from the tree.  */
	      if (!TREE_CONSTANT (DECL_FIELD_OFFSET (gnu_field))
		  && !CONTAINS_PLACEHOLDER_P (DECL_FIELD_OFFSET (gnu_field)))
		{
		  DECL_FIELD_OFFSET (gnu_field)
		    = elaborate_expression_2 (DECL_FIELD_OFFSET (gnu_field),
					      gnat_temp, "OFFSET", definition,
					      false,
					      DECL_OFFSET_ALIGN (gnu_field));

		  /* ??? The context of gnu_field is not necessarily gnu_type
		     so the MULT_EXPR node built above may not be marked by
		     the call to create_type_decl below.  */
		  MARK_VISITED (DECL_FIELD_OFFSET (gnu_field));
		}
	    }

      /* Now check if the type allows atomic access.  */
      if (Is_Full_Access (gnat_entity))
	check_ok_for_atomic_type (gnu_type, gnat_entity, false);

      /* If this is not an unconstrained array type, set some flags.  */
      if (TREE_CODE (gnu_type) != UNCONSTRAINED_ARRAY_TYPE)
	{
	  /* Record the property that objects of tagged types are guaranteed to
	     be properly aligned.  This is necessary because conversions to the
	     class-wide type are translated into conversions to the root type,
	     which can be less aligned than some of its derived types.  */
	  if (Is_Tagged_Type (gnat_entity)
	      || Is_Class_Wide_Equivalent_Type (gnat_entity))
	    TYPE_ALIGN_OK (gnu_type) = 1;

	  /* Record whether the type is passed by reference.  */
	  if (is_by_ref && !VOID_TYPE_P (gnu_type))
	    TYPE_BY_REFERENCE_P (gnu_type) = 1;

	  /* Record whether an alignment clause was specified.  */
	  if (Present (Alignment_Clause (gnat_entity)))
	    TYPE_USER_ALIGN (gnu_type) = 1;

	  /* Record whether a pragma Universal_Aliasing was specified.  */
	  if (Universal_Aliasing (gnat_entity) && !TYPE_IS_DUMMY_P (gnu_type))
	    TYPE_UNIVERSAL_ALIASING_P (gnu_type) = 1;

	  /* If it is passed by reference, force BLKmode to ensure that
	     objects of this type will always be put in memory.  */
	  if (AGGREGATE_TYPE_P (gnu_type) && TYPE_BY_REFERENCE_P (gnu_type))
	    SET_TYPE_MODE (gnu_type, BLKmode);
	}

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
      if (kind != E_Subprogram_Type
	  && Is_Derived_Type (gnat_entity)
	  && !type_annotate_only)
	{
	  Entity_Id gnat_parent_type = Underlying_Type (Etype (gnat_entity));
	  /* For constrained packed array subtypes, the implementation type is
	     used instead of the nominal type.  */
	  if (kind == E_Array_Subtype
	      && Is_Constrained (gnat_entity)
	      && Present (Packed_Array_Impl_Type (gnat_parent_type)))
	    gnat_parent_type = Packed_Array_Impl_Type (gnat_parent_type);
	  relate_alias_sets (gnu_type, gnat_to_gnu_type (gnat_parent_type),
			     Is_Composite_Type (gnat_entity)
			     ? ALIAS_SET_COPY : ALIAS_SET_SUPERSET);
	}

      /* Finally get to the appropriate variant, except for the implementation
	 type of a packed array because the GNU type might be further adjusted
	 when the original array type is itself processed.  */
      if (Treat_As_Volatile (gnat_entity)
	  && !Is_Packed_Array_Impl_Type (gnat_entity))
	{
	  const int quals
	    = TYPE_QUAL_VOLATILE
	      | (Is_Full_Access (gnat_entity) ? TYPE_QUAL_ATOMIC : 0);
	  /* This is required by free_lang_data_in_type to disable the ODR.  */
	  if (TREE_CODE (gnu_type) == ENUMERAL_TYPE)
	    TYPE_STUB_DECL (gnu_type)
	      = create_type_stub_decl (TYPE_NAME (gnu_type), gnu_type);
	  gnu_type = change_qualified_type (gnu_type, quals);
	}

      /* If we already made a decl, just set the type, otherwise create it.  */
      if (gnu_decl)
	{
	  TREE_TYPE (gnu_decl) = gnu_type;
	  TYPE_STUB_DECL (gnu_type) = gnu_decl;
	}
      else
	gnu_decl = create_type_decl (gnu_entity_name, gnu_type, artificial_p,
				     debug_info_p, gnat_entity);

      /* For vector types, make the representative array the debug type.  */
      if (VECTOR_TYPE_P (gnu_type))
	{
	  tree rep = TYPE_REPRESENTATIVE_ARRAY (gnu_type);
	  TYPE_NAME (rep) = DECL_NAME (gnu_decl);
	  SET_TYPE_DEBUG_TYPE (gnu_type, rep);
	}
    }

  /* Otherwise, for a type reusing an existing DECL, back-annotate values.  */
  else if (is_type
	   && !TYPE_IS_DUMMY_P (TREE_TYPE (gnu_decl))
	   && Present (gnat_annotate_type))
    {
      if (!Known_Alignment (gnat_entity))
	Copy_Alignment (gnat_entity, gnat_annotate_type);
      if (!Known_Esize (gnat_entity))
	Copy_Esize (gnat_entity, gnat_annotate_type);
      if (!Known_RM_Size (gnat_entity))
	Copy_RM_Size (gnat_entity, gnat_annotate_type);
    }

  /* If we haven't already, associate the ..._DECL node that we just made with
     the input GNAT entity node.  */
  if (!saved)
    save_gnu_tree (gnat_entity, gnu_decl, false);

  /* Now we are sure gnat_entity has a corresponding ..._DECL node,
     eliminate as many deferred computations as possible.  */
  process_deferred_decl_context (false);

  /* If this is an enumeration or floating-point type, we were not able to set
     the bounds since they refer to the type.  These are always static.  */
  if ((kind == E_Enumeration_Type && Present (First_Literal (gnat_entity)))
      || (kind == E_Floating_Point_Type))
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

      for (; p; p = next)
	{
	  next = p->next;

	  if (p->old_type)
	    update_pointer_to (TYPE_MAIN_VARIANT (p->old_type),
			       gnat_to_gnu_type (p->full_type));
	  free (p);
	}
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

      for (p = defer_limited_with_list; p; p = p->next)
	if (p->old_type
	    && (Non_Limited_View (p->full_type) == gnat_entity
		|| Full_View (p->full_type) == gnat_entity))
	  {
	    update_pointer_to (TYPE_MAIN_VARIANT (p->old_type),
			       TREE_TYPE (gnu_decl));
	    if (TYPE_DUMMY_IN_PROFILE_P (p->old_type))
	      update_profiles_with (p->old_type);
	    p->old_type = NULL_TREE;
	  }
    }

  if (this_global)
    force_global--;

  /* If this is a packed array type whose original array type is itself
     an itype without freeze node, make sure the latter is processed.  */
  if (Is_Packed_Array_Impl_Type (gnat_entity)
      && Is_Itype (Original_Array_Type (gnat_entity))
      && No (Freeze_Node (Original_Array_Type (gnat_entity)))
      && !present_gnu_tree (Original_Array_Type (gnat_entity)))
    gnat_to_gnu_entity (Original_Array_Type (gnat_entity), NULL_TREE, false);

  return gnu_decl;
}

/* Similar, but if the returned value is a COMPONENT_REF, return the
   FIELD_DECL.  */

tree
gnat_to_gnu_field_decl (Entity_Id gnat_entity)
{
  tree gnu_field = gnat_to_gnu_entity (gnat_entity, NULL_TREE, false);

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

  gnu_decl = gnat_to_gnu_entity (gnat_entity, NULL_TREE, false);
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

/* Return whether the E_Subprogram_Type/E_Function/E_Procedure GNAT_ENTITY is
   a C++ imported method or equivalent.

   We use the predicate to find out whether we need to use METHOD_TYPE instead
   of FUNCTION_TYPE for GNAT_ENTITY for the sake compatibility with C++.  This
   in turn determines whether the "thiscall" calling convention is used by the
   back-end for GNAT_ENTITY on 32-bit x86/Windows.  */

static bool
is_cplusplus_method (Entity_Id gnat_entity)
{
  /* A constructor is a method on the C++ side.  We deal with it now because
     it is declared without the 'this' parameter in the sources and, although
     the front-end will create a version with the 'this' parameter for code
     generation purposes, we want to return true for both versions.  */
  if (Is_Constructor (gnat_entity))
    return true;

  /* Check that the subprogram has C++ convention.  */
  if (Convention (gnat_entity) != Convention_CPP)
    return false;

  /* And that the type of the first parameter (indirectly) has it too, but
     we make an exception for Interfaces because they need not be imported.  */
  Entity_Id gnat_first = First_Formal (gnat_entity);
  if (No (gnat_first))
    return false;
  Entity_Id gnat_type = Etype (gnat_first);
  if (Is_Access_Type (gnat_type))
    gnat_type = Directly_Designated_Type (gnat_type);
  if (Convention (gnat_type) != Convention_CPP && !Is_Interface (gnat_type))
    return false;

  /* This is the main case: a C++ virtual method imported as a primitive
     operation of a tagged type.  */
  if (Is_Dispatching_Operation (gnat_entity))
    return true;

  /* This is set on the E_Subprogram_Type built for a dispatching call.  */
  if (Is_Dispatch_Table_Entity (gnat_entity))
    return true;

  /* A thunk needs to be handled like its associated primitive operation.  */
  if (Is_Subprogram (gnat_entity) && Is_Thunk (gnat_entity))
    return true;

  /* Now on to the annoying case: a C++ non-virtual method, imported either
     as a non-primitive operation of a tagged type or as a primitive operation
     of an untagged type.  We cannot reliably differentiate these cases from
     their static member or regular function equivalents in Ada, so we ask
     the C++ side through the mangled name of the function, as the implicit
     'this' parameter is not encoded in the mangled name of a method.  */
  if (Is_Subprogram (gnat_entity) && Present (Interface_Name (gnat_entity)))
    {
      String_Pointer sp = { NULL, NULL };
      Get_External_Name (gnat_entity, false, sp);

      void *mem;
      struct demangle_component *cmp
	= cplus_demangle_v3_components (Name_Buffer,
					DMGL_GNU_V3
					| DMGL_TYPES
					| DMGL_PARAMS
					| DMGL_RET_DROP,
					&mem);
      if (!cmp)
	return false;

      /* We need to release MEM once we have a successful demangling.  */
      bool ret = false;

      if (cmp->type == DEMANGLE_COMPONENT_TYPED_NAME
	  && cmp->u.s_binary.right->type == DEMANGLE_COMPONENT_FUNCTION_TYPE
	  && (cmp = cmp->u.s_binary.right->u.s_binary.right) != NULL
	  && cmp->type == DEMANGLE_COMPONENT_ARGLIST)
	{
	  /* Make sure there is at least one parameter in C++ too.  */
	  if (cmp->u.s_binary.left)
	    {
	      unsigned int n_ada_args = 0;
	      do {
		n_ada_args++;
		gnat_first = Next_Formal (gnat_first);
	      } while (Present (gnat_first));

	      unsigned int n_cpp_args = 0;
	      do {
		n_cpp_args++;
		cmp = cmp->u.s_binary.right;
	      } while (cmp);

	      if (n_cpp_args < n_ada_args)
		ret = true;
	    }
	  else
	    ret = true;
	}

      free (mem);

      return ret;
    }

  return false;
}

/* Return the inlining status of the GNAT subprogram SUBPROG.  */

static enum inline_status_t
inline_status_for_subprog (Entity_Id subprog)
{
  if (Has_Pragma_No_Inline (subprog))
    return is_suppressed;

  if (Has_Pragma_Inline_Always (subprog))
    return is_required;

  if (Is_Inlined (subprog))
    {
      tree gnu_type;

      /* This is a kludge to work around a pass ordering issue: for small
	 record types with many components, i.e. typically bit-fields, the
	 initialization routine can contain many assignments that will be
	 merged by the GIMPLE store merging pass.  But this pass runs very
	 late in the pipeline, in particular after the inlining decisions
	 are made, so the inlining heuristics cannot take its outcome into
	 account.  Therefore, we optimistically override the heuristics for
	 the initialization routine in this case.  */
      if (Is_Init_Proc (subprog)
	  && flag_store_merging
	  && Is_Record_Type (Etype (First_Formal (subprog)))
	  && (gnu_type = gnat_to_gnu_type (Etype (First_Formal (subprog))))
	  && !TYPE_IS_BY_REFERENCE_P (gnu_type)
	  && tree_fits_uhwi_p (TYPE_SIZE (gnu_type))
	  && compare_tree_int (TYPE_SIZE (gnu_type), MAX_FIXED_MODE_SIZE) <= 0)
	return is_prescribed;

      return is_requested;
    }

  return is_default;
}

/* Finalize the processing of From_Limited_With incomplete types.  */

void
finalize_from_limited_with (void)
{
  struct incomplete *p, *next;

  p = defer_limited_with_list;
  defer_limited_with_list = NULL;

  for (; p; p = next)
    {
      next = p->next;

      if (p->old_type)
	{
	  update_pointer_to (TYPE_MAIN_VARIANT (p->old_type),
			     gnat_to_gnu_type (p->full_type));
	  if (TYPE_DUMMY_IN_PROFILE_P (p->old_type))
	    update_profiles_with (p->old_type);
	}

      free (p);
    }
}

/* Return the equivalent type to be used for GNAT_ENTITY, if it's a kind
   of type (such E_Task_Type) that has a different type which Gigi uses
   for its representation.  If the type does not have a special type for
   its representation, return GNAT_ENTITY.  */

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
      if (Present (Equivalent_Type (gnat_entity)))
	gnat_equiv = Equivalent_Type (gnat_entity);
      break;

    case E_Access_Subtype:
      gnat_equiv = Etype (gnat_entity);
      break;

    case E_Array_Subtype:
      if (!Is_Constrained (gnat_entity))
	gnat_equiv = Etype (gnat_entity);
      break;

    case E_Class_Wide_Type:
      gnat_equiv = Root_Type (gnat_entity);
      break;

    case E_Protected_Type:
    case E_Protected_Subtype:
    case E_Task_Type:
    case E_Task_Subtype:
      if (Present (Corresponding_Record_Type (gnat_entity)))
	gnat_equiv = Corresponding_Record_Type (gnat_entity);
      break;

    default:
      break;
    }

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
  const bool is_bit_packed = Is_Bit_Packed_Array (gnat_array);
  tree gnu_type = gnat_to_gnu_type (gnat_type);
  tree gnu_comp_size;
  bool has_packed_components;
  unsigned int max_align;

  /* If an alignment is specified, use it as a cap on the component type
     so that it can be honored for the whole type, but ignore it for the
     original type of packed array types.  */
  if (No (Packed_Array_Impl_Type (gnat_array))
      && Known_Alignment (gnat_array))
    max_align = validate_alignment (Alignment (gnat_array), gnat_array, 0);
  else
    max_align = 0;

  /* Try to get a packable form of the component if needed.  */
  if ((Is_Packed (gnat_array) || Has_Component_Size_Clause (gnat_array))
      && !is_bit_packed
      && !Has_Aliased_Components (gnat_array)
      && !Strict_Alignment (gnat_type)
      && RECORD_OR_UNION_TYPE_P (gnu_type)
      && !TYPE_FAT_POINTER_P (gnu_type)
      && tree_fits_uhwi_p (TYPE_SIZE (gnu_type)))
    {
      gnu_type = make_packable_type (gnu_type, false, max_align);
      has_packed_components = true;
    }
  else
    has_packed_components = is_bit_packed;

  /* Get and validate any specified Component_Size.  */
  gnu_comp_size
    = validate_size (Component_Size (gnat_array), gnu_type, gnat_array,
		     has_packed_components ? TYPE_DECL : VAR_DECL, true,
		     Has_Component_Size_Clause (gnat_array), NULL, NULL);

  /* If the component type is a RECORD_TYPE that has a self-referential size,
     then use the maximum size for the component size.  */
  if (!gnu_comp_size
      && TREE_CODE (gnu_type) == RECORD_TYPE
      && CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_type)))
    gnu_comp_size = max_size (TYPE_SIZE (gnu_type), true);

  /* If the array has aliased components and the component size is zero, force
     the unit size to ensure that the components have distinct addresses.  */
  if (!gnu_comp_size
      && Has_Aliased_Components (gnat_array)
      && integer_zerop (TYPE_SIZE (gnu_type)))
    gnu_comp_size = bitsize_unit_node;

  /* Honor the component size.  This is not needed for bit-packed arrays.  */
  if (gnu_comp_size && !is_bit_packed)
    {
      tree orig_type = gnu_type;
      unsigned int gnu_comp_align;

      gnu_type = make_type_from_size (gnu_type, gnu_comp_size, false);
      if (max_align > 0 && TYPE_ALIGN (gnu_type) > max_align)
	gnu_type = orig_type;
      else
	orig_type = gnu_type;

      /* We need to make sure that the size is a multiple of the alignment.
	 But we do not misalign the component type because of the alignment
	 of the array type here; this either must have been done earlier in
	 the packed case or should be rejected in the non-packed case.  */
      if (TREE_CODE (gnu_comp_size) == INTEGER_CST)
	{
	  const unsigned HOST_WIDE_INT int_size = tree_to_uhwi (gnu_comp_size);
	  gnu_comp_align = int_size & -int_size;
	  if (gnu_comp_align > TYPE_ALIGN (gnu_type))
	    gnu_comp_align = 0;
	}
       else
	 gnu_comp_align = 0;

      gnu_type = maybe_pad_type (gnu_type, gnu_comp_size, gnu_comp_align,
				 gnat_array, true, definition, true);

      /* If a padding record was made, declare it now since it will never be
	 declared otherwise.  This is necessary to ensure that its subtrees
	 are properly marked.  */
      if (gnu_type != orig_type && !DECL_P (TYPE_NAME (gnu_type)))
	create_type_decl (TYPE_NAME (gnu_type), gnu_type, true, debug_info_p,
			  gnat_array);
    }

  /* This is a very special case where the array has aliased components and the
     component size might be zero at run time.  As explained above, we force at
     least the unit size but we don't want to build a distinct padding type for
     each invocation (they are not canonicalized if they have variable size) so
     we cache this special padding type as TYPE_PADDING_FOR_COMPONENT.  */
  else if (Has_Aliased_Components (gnat_array)
	   && TREE_CODE (gnu_type) == ARRAY_TYPE
	   && !TREE_CONSTANT (TYPE_SIZE (gnu_type)))
    {
      if (TYPE_PADDING_FOR_COMPONENT (gnu_type))
	gnu_type = TYPE_PADDING_FOR_COMPONENT (gnu_type);
      else
	{
	  gnu_comp_size
	    = size_binop (MAX_EXPR, TYPE_SIZE (gnu_type), bitsize_unit_node);
	  TYPE_PADDING_FOR_COMPONENT (gnu_type)
	    = maybe_pad_type (gnu_type, gnu_comp_size, 0, gnat_array,
			      true, definition, true);
	  gnu_type = TYPE_PADDING_FOR_COMPONENT (gnu_type);
	  create_type_decl (TYPE_NAME (gnu_type), gnu_type, true, debug_info_p,
			    gnat_array);
	}
    }

  /* Now check if the type of the component allows atomic access.  */
  if (Has_Atomic_Components (gnat_array) || Is_Full_Access (gnat_type))
    check_ok_for_atomic_type (gnu_type, gnat_array, true);

  /* If the component type is a padded type made for a non-bit-packed array
     of scalars with reverse storage order, we need to propagate the reverse
     storage order to the padding type since it is the innermost enclosing
     aggregate type around the scalar.  */
  if (TYPE_IS_PADDING_P (gnu_type)
      && !is_bit_packed
      && Reverse_Storage_Order (gnat_array)
      && Is_Scalar_Type (gnat_type))
    gnu_type = set_reverse_storage_order_on_pad_type (gnu_type);

  if (Has_Volatile_Components (gnat_array))
    {
      const int quals
	= TYPE_QUAL_VOLATILE
	  | (Has_Atomic_Components (gnat_array) ? TYPE_QUAL_ATOMIC : 0);
      gnu_type = change_qualified_type (gnu_type, quals);
    }

  return gnu_type;
}

/* Return whether TYPE requires that formal parameters of TYPE be initialized
   when they are Out parameters passed by copy.

   This just implements the set of conditions listed in RM 6.4.1(12).  */

static bool
type_requires_init_of_formal (Entity_Id type)
{
  type = Underlying_Type (type);

  if (Is_Access_Type (type))
    return true;

  if (Is_Scalar_Type (type))
    return Has_Default_Aspect (type);

  if (Is_Array_Type (type))
    return Has_Default_Aspect (type)
	   || type_requires_init_of_formal (Component_Type (type));

  if (Is_Record_Type (type))
    for (Entity_Id field = First_Entity (type);
	 Present (field);
	 field = Next_Entity (field))
      {
	if (Ekind (field) == E_Discriminant && !Is_Unchecked_Union (type))
	  return true;

	if (Ekind (field) == E_Component
	    && (Present (Expression (Parent (field)))
		|| type_requires_init_of_formal (Etype (field))))
	  return true;
      }

  return false;
}

/* Return a GCC tree for a parameter corresponding to GNAT_PARAM, to be placed
   in the parameter list of GNAT_SUBPROG.  GNU_PARAM_TYPE is the GCC tree for
   the type of the parameter.  FIRST is true if this is the first parameter in
   the list of GNAT_SUBPROG.  Also set CICO to true if the parameter must use
   the copy-in copy-out implementation mechanism.

   The returned tree is a PARM_DECL, except for the cases where no parameter
   needs to be actually passed to the subprogram; the type of this "shadow"
   parameter is then returned instead.  */

static tree
gnat_to_gnu_param (Entity_Id gnat_param, tree gnu_param_type, bool first,
		   Entity_Id gnat_subprog, bool *cico)
{
  Mechanism_Type mech = Mechanism (gnat_param);
  tree gnu_param_name = get_entity_name (gnat_param);
  bool foreign = Has_Foreign_Convention (gnat_subprog);
  bool in_param = (Ekind (gnat_param) == E_In_Parameter);
  /* The parameter can be indirectly modified if its address is taken.  */
  bool ro_param = in_param && !Address_Taken (gnat_param);
  bool by_return = false, by_component_ptr = false;
  bool by_ref = false;
  bool forced_by_ref = false;
  bool restricted_aliasing_p = false;
  location_t saved_location = input_location;
  tree gnu_param;

  /* Make sure to use the proper SLOC for vector ABI warnings.  */
  if (VECTOR_TYPE_P (gnu_param_type))
    Sloc_to_locus (Sloc (gnat_subprog), &input_location);

  /* Builtins are expanded inline and there is no real call sequence involved.
     So the type expected by the underlying expander is always the type of the
     argument "as is".  */
  if (Is_Intrinsic_Subprogram (gnat_subprog)
      && Present (Interface_Name (gnat_subprog)))
    mech = By_Copy;

  /* Handle the first parameter of a valued procedure specially: it's a copy
     mechanism for which the parameter is never allocated.  */
  else if (first && Is_Valued_Procedure (gnat_subprog))
    {
      gcc_assert (Ekind (gnat_param) == E_Out_Parameter);
      mech = By_Copy;
      by_return = true;
    }

  /* Or else, see if a Mechanism was supplied that forced this parameter
     to be passed one way or another.  */
  else if (mech == Default || mech == By_Copy || mech == By_Reference)
    forced_by_ref
      = (mech == By_Reference
	 && !foreign
	 && !TYPE_IS_BY_REFERENCE_P (gnu_param_type)
	 && !Is_Aliased (gnat_param));

  /* Positive mechanism means by copy for sufficiently small parameters.  */
  else if (mech > 0)
    {
      if (TREE_CODE (gnu_param_type) == UNCONSTRAINED_ARRAY_TYPE
	  || TREE_CODE (TYPE_SIZE (gnu_param_type)) != INTEGER_CST
	  || compare_tree_int (TYPE_SIZE (gnu_param_type), mech) > 0)
	mech = By_Reference;
      else
	mech = By_Copy;
    }

  /* Otherwise, it's an unsupported mechanism so error out.  */
  else
    {
      post_error ("unsupported mechanism for&", gnat_param);
      mech = Default;
    }

  /* Either for foreign conventions, or if the underlying type is not passed
     by reference and is as large and aligned as the original type, strip off
     a possible padding type.  */
  if (TYPE_IS_PADDING_P (gnu_param_type))
    {
      tree inner_type = TREE_TYPE (TYPE_FIELDS (gnu_param_type));

      if (foreign
	  || (mech != By_Reference
	      && !must_pass_by_ref (inner_type)
	      && (mech == By_Copy || !default_pass_by_ref (inner_type))
	      && ((TYPE_SIZE (inner_type) == TYPE_SIZE (gnu_param_type)
		   && TYPE_ALIGN (inner_type) >= TYPE_ALIGN (gnu_param_type))
		  || Is_Init_Proc (gnat_subprog))))
	gnu_param_type = inner_type;
    }

  /* For foreign conventions, pass arrays as pointers to the element type.
     First check for unconstrained array and get the underlying array.  */
  if (foreign && TREE_CODE (gnu_param_type) == UNCONSTRAINED_ARRAY_TYPE)
    gnu_param_type
      = TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_param_type))));

  /* Arrays are passed as pointers to element type for foreign conventions.  */
  if (foreign && mech != By_Copy && TREE_CODE (gnu_param_type) == ARRAY_TYPE)
    {
      /* Strip off any multi-dimensional entries, then strip
	 off the last array to get the component type.  */
      while (TREE_CODE (TREE_TYPE (gnu_param_type)) == ARRAY_TYPE
	     && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_param_type)))
	gnu_param_type = TREE_TYPE (gnu_param_type);

      gnu_param_type = TREE_TYPE (gnu_param_type);
      gnu_param_type = build_pointer_type (gnu_param_type);
      by_component_ptr = true;
    }

  /* Fat pointers are passed as thin pointers for foreign conventions.  */
  else if (foreign && TYPE_IS_FAT_POINTER_P (gnu_param_type))
    gnu_param_type
      = make_type_from_size (gnu_param_type, size_int (POINTER_SIZE), 0);

  /* Use a pointer type for the "this" pointer of C++ constructors.  */
  else if (Chars (gnat_param) == Name_uInit && Is_Constructor (gnat_subprog))
    {
      gcc_assert (mech == By_Reference);
      gnu_param_type = build_pointer_type (gnu_param_type);
      by_ref = true;
    }

  /* If we were requested or muss pass by reference, do so.
     If we were requested to pass by copy, do so.
     Otherwise, for foreign conventions, pass In Out or Out parameters
     or aggregates by reference.  For COBOL and Fortran, pass all
     integer and FP types that way too.  For Convention Ada, use
     the standard Ada default.  */
  else if (mech == By_Reference
	   || must_pass_by_ref (gnu_param_type)
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
	 been forced to by-ref allow only a restricted form of aliasing.  */
      restricted_aliasing_p
	= !TYPE_IS_BY_REFERENCE_P (gnu_param_type) && mech != By_Reference;
      gnu_param_type = build_reference_type (gnu_param_type);
      by_ref = true;
    }

  /* Pass In Out or Out parameters using copy-in copy-out mechanism.  */
  else if (!in_param)
    *cico = true;

  input_location = saved_location;

  if (mech == By_Copy && (by_ref || by_component_ptr))
    post_error ("??cannot pass & by copy", gnat_param);

  /* If this is an Out parameter that isn't passed by reference and whose
     type doesn't require the initialization of formals, we don't make a
     PARM_DECL for it.  Instead, it will be a VAR_DECL created when we
     process the procedure, so just return its type here.  Likewise for
     the _Init parameter of an initialization procedure or the special
     parameter of a valued procedure, never pass them in.  */
  if (Ekind (gnat_param) == E_Out_Parameter
      && !by_ref
      && !by_component_ptr
      && (!type_requires_init_of_formal (Etype (gnat_param))
	  || Is_Init_Proc (gnat_subprog)
	  || by_return))
    {
      Set_Mechanism (gnat_param, By_Copy);
      return gnu_param_type;
    }

  gnu_param = create_param_decl (gnu_param_name, gnu_param_type);
  TREE_READONLY (gnu_param) = ro_param || by_ref || by_component_ptr;
  DECL_BY_REF_P (gnu_param) = by_ref;
  DECL_FORCED_BY_REF_P (gnu_param) = forced_by_ref;
  DECL_BY_COMPONENT_PTR_P (gnu_param) = by_component_ptr;
  DECL_POINTS_TO_READONLY_P (gnu_param)
    = (ro_param && (by_ref || by_component_ptr));
  DECL_CAN_NEVER_BE_NULL_P (gnu_param) = Can_Never_Be_Null (gnat_param);
  DECL_RESTRICTED_ALIASING_P (gnu_param) = restricted_aliasing_p;
  Sloc_to_locus (Sloc (gnat_param), &DECL_SOURCE_LOCATION (gnu_param));

  /* If no Mechanism was specified, indicate what we're using, then
     back-annotate it.  */
  if (mech == Default)
    mech = (by_ref || by_component_ptr) ? By_Reference : By_Copy;

  Set_Mechanism (gnat_param, mech);
  return gnu_param;
}

/* Associate GNAT_SUBPROG with GNU_TYPE, which must be a dummy type, so that
   GNAT_SUBPROG is updated when GNU_TYPE is completed.

   Ada 2012 (AI05-019) says that freezing a subprogram does not always freeze
   the corresponding profile, which means that, by the time the freeze node
   of the subprogram is encountered, types involved in its profile may still
   be not yet frozen.  That's why we need to update GNAT_SUBPROG when we see
   the freeze node of types involved in its profile, either types of formal
   parameters or the return type.  */

static void
associate_subprog_with_dummy_type (Entity_Id gnat_subprog, tree gnu_type)
{
  gcc_assert (TYPE_IS_DUMMY_P (gnu_type));

  struct tree_entity_vec_map in;
  in.base.from = gnu_type;
  struct tree_entity_vec_map **slot
    = dummy_to_subprog_map->find_slot (&in, INSERT);
  if (!*slot)
    {
      tree_entity_vec_map *e = ggc_alloc<tree_entity_vec_map> ();
      e->base.from = gnu_type;
      e->to = NULL;
      *slot = e;
    }

  /* Even if there is already a slot for GNU_TYPE, we need to set the flag
     because the vector might have been just emptied by update_profiles_with.
     This can happen when there are 2 freeze nodes associated with different
     views of the same type; the type will be really complete only after the
     second freeze node is encountered.  */
  TYPE_DUMMY_IN_PROFILE_P (gnu_type) = 1;

  vec<Entity_Id, va_gc_atomic> *v = (*slot)->to;

  /* Make sure GNAT_SUBPROG is not associated twice with the same dummy type,
     since this would mean updating twice its profile.  */
  if (v)
    {
      const unsigned len = v->length ();
      unsigned int l = 0, u = len;

      /* Entity_Id is a simple integer so we can implement a stable order on
	 the vector with an ordered insertion scheme and binary search.  */
      while (l < u)
	{
	  unsigned int m = (l + u) / 2;
	  int diff = (int) (*v)[m] - (int) gnat_subprog;
	  if (diff > 0)
	    u = m;
	  else if (diff < 0)
	    l = m + 1;
	  else
	    return;
	}

      /* l == u and therefore is the insertion point.  */
      vec_safe_insert (v, l, gnat_subprog);
    }
  else
    vec_safe_push (v, gnat_subprog);

  (*slot)->to = v;
}

/* Update the GCC tree previously built for the profile of GNAT_SUBPROG.  */

static void
update_profile (Entity_Id gnat_subprog)
{
  tree gnu_param_list;
  tree gnu_type = gnat_to_gnu_subprog_type (gnat_subprog, true,
					    Needs_Debug_Info (gnat_subprog),
					    &gnu_param_list);
  if (DECL_P (gnu_type))
    {
      /* Builtins cannot have their address taken so we can reset them.  */
      gcc_assert (fndecl_built_in_p (gnu_type));
      save_gnu_tree (gnat_subprog, NULL_TREE, false);
      save_gnu_tree (gnat_subprog, gnu_type, false);
      return;
    }

  tree gnu_subprog = get_gnu_tree (gnat_subprog);

  TREE_TYPE (gnu_subprog) = gnu_type;

  /* If GNAT_SUBPROG is an actual subprogram, GNU_SUBPROG is a FUNCTION_DECL
     and needs to be adjusted too.  */
  if (Ekind (gnat_subprog) != E_Subprogram_Type)
    {
      tree gnu_entity_name = get_entity_name (gnat_subprog);
      tree gnu_ext_name
	= gnu_ext_name_for_subprog (gnat_subprog, gnu_entity_name);

      DECL_ARGUMENTS (gnu_subprog) = gnu_param_list;
      finish_subprog_decl (gnu_subprog, gnu_ext_name, gnu_type);
    }
}

/* Update the GCC trees previously built for the profiles involving GNU_TYPE,
   a dummy type which appears in profiles.  */

void
update_profiles_with (tree gnu_type)
{
  struct tree_entity_vec_map in;
  in.base.from = gnu_type;
  struct tree_entity_vec_map *e = dummy_to_subprog_map->find (&in);
  gcc_assert (e);
  vec<Entity_Id, va_gc_atomic> *v = e->to;
  e->to = NULL;

  /* The flag needs to be reset before calling update_profile, in case
     associate_subprog_with_dummy_type is again invoked on GNU_TYPE.  */
  TYPE_DUMMY_IN_PROFILE_P (gnu_type) = 0;

  unsigned int i;
  Entity_Id *iter;
  FOR_EACH_VEC_ELT (*v, i, iter)
    update_profile (*iter);

  vec_free (v);
}

/* Return the GCC tree for GNAT_TYPE present in the profile of a subprogram.

   Ada 2012 (AI05-0151) says that incomplete types coming from a limited
   context may now appear as parameter and result types.  As a consequence,
   we may need to defer their translation until after a freeze node is seen
   or to the end of the current unit.  We also aim at handling temporarily
   incomplete types created by the usual delayed elaboration scheme.  */

static tree
gnat_to_gnu_profile_type (Entity_Id gnat_type)
{
  /* This is the same logic as the E_Access_Type case of gnat_to_gnu_entity
     so the rationale is exposed in that place.  These processings probably
     ought to be merged at some point.  */
  Entity_Id gnat_equiv = Gigi_Equivalent_Type (gnat_type);
  const bool is_from_limited_with
    = (Is_Incomplete_Type (gnat_equiv)
       && From_Limited_With (gnat_equiv));
  Entity_Id gnat_full_direct_first
    = (is_from_limited_with
       ? Non_Limited_View (gnat_equiv)
       : (Is_Incomplete_Or_Private_Type (gnat_equiv)
	  ? Full_View (gnat_equiv) : Empty));
  Entity_Id gnat_full_direct
    = ((is_from_limited_with
	&& Present (gnat_full_direct_first)
	&& Is_Private_Type (gnat_full_direct_first))
       ? Full_View (gnat_full_direct_first)
       : gnat_full_direct_first);
  Entity_Id gnat_full = Gigi_Equivalent_Type (gnat_full_direct);
  Entity_Id gnat_rep = Present (gnat_full) ? gnat_full : gnat_equiv;
  const bool in_main_unit = In_Extended_Main_Code_Unit (gnat_rep);
  tree gnu_type;

  if (Present (gnat_full) && present_gnu_tree (gnat_full))
    gnu_type = TREE_TYPE (get_gnu_tree (gnat_full));

  else if (is_from_limited_with
	   && ((!in_main_unit
	        && !present_gnu_tree (gnat_equiv)
		&& Present (gnat_full)
		&& (Is_Record_Type (gnat_full)
		    || Is_Array_Type (gnat_full)
		    || Is_Access_Type (gnat_full)))
	       || (in_main_unit && Present (Freeze_Node (gnat_rep)))))
    {
      gnu_type = make_dummy_type (gnat_equiv);

      if (!in_main_unit)
	{
	  struct incomplete *p = XNEW (struct incomplete);

	  p->old_type = gnu_type;
	  p->full_type = gnat_equiv;
	  p->next = defer_limited_with_list;
	  defer_limited_with_list = p;
	}
    }

  else if (type_annotate_only && No (gnat_equiv))
    gnu_type = void_type_node;

  else
    gnu_type = gnat_to_gnu_type (gnat_equiv);

  /* Access-to-unconstrained-array types need a special treatment.  */
  if (Is_Array_Type (gnat_rep) && !Is_Constrained (gnat_rep))
    {
      if (!TYPE_POINTER_TO (gnu_type))
	build_dummy_unc_pointer_types (gnat_equiv, gnu_type);
    }

  return gnu_type;
}

/* Return true if TYPE contains only integral data, recursively if need be.  */

static bool
type_contains_only_integral_data (tree type)
{
  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	if (!type_contains_only_integral_data (TREE_TYPE (field)))
	  return false;
      return true;

    case ARRAY_TYPE:
    case COMPLEX_TYPE:
      return type_contains_only_integral_data (TREE_TYPE (type));

    default:
      return INTEGRAL_TYPE_P (type);
    }

  gcc_unreachable ();
}

/* Return a GCC tree for a subprogram type corresponding to GNAT_SUBPROG.
   DEFINITION is true if this is for a subprogram being defined.  DEBUG_INFO_P
   is true if we need to write debug information for other types that we may
   create in the process.  Also set PARAM_LIST to the list of parameters.
   If GNAT_SUBPROG is bound to a GCC builtin, return the DECL for the builtin
   directly instead of its type.  */

static tree
gnat_to_gnu_subprog_type (Entity_Id gnat_subprog, bool definition,
			  bool debug_info_p, tree *param_list)
{
  const Entity_Kind kind = Ekind (gnat_subprog);
  const bool method_p = is_cplusplus_method (gnat_subprog);
  const bool variadic = IN (Convention (gnat_subprog), Convention_C_Variadic);
  Entity_Id gnat_return_type = Etype (gnat_subprog);
  Entity_Id gnat_param;
  tree gnu_type = present_gnu_tree (gnat_subprog)
		  ? TREE_TYPE (get_gnu_tree (gnat_subprog)) : NULL_TREE;
  tree gnu_return_type;
  tree gnu_param_type_list = NULL_TREE;
  tree gnu_param_list = NULL_TREE;
  /* Non-null for subprograms containing parameters passed by copy-in copy-out
     (In Out or Out parameters not passed by reference), in which case it is
     the list of nodes used to specify the values of the In Out/Out parameters
     that are returned as a record upon procedure return.  The TREE_PURPOSE of
     an element of this list is a FIELD_DECL of the record and the TREE_VALUE
     is the PARM_DECL corresponding to that field.  This list will be saved in
     the TYPE_CI_CO_LIST field of the FUNCTION_TYPE node we create.  */
  tree gnu_cico_list = NULL_TREE;
  tree gnu_cico_return_type = NULL_TREE;
  tree gnu_cico_field_list = NULL_TREE;
  bool gnu_cico_only_integral_type = true;
  /* Although the semantics of "pure" units in Ada essentially match those of
     "const" in GNU C, the semantics of the Is_Pure flag in GNAT do not say
     anything about access to global memory, that's why it needs to be mapped
     to "pure" instead of "const" in GNU C.  The property is orthogonal to the
     "nothrow" property only if the EH circuitry is explicit in the internal
     representation of the middle-end: if we are to completely hide the EH
     circuitry from it, we need to declare that calls to pure Ada subprograms
     that can throw have side effects, since they can trigger an "abnormal"
     transfer of control; therefore they cannot be "pure" in the GCC sense.  */
  bool pure_flag = Is_Pure (gnat_subprog) && Back_End_Exceptions ();
  bool return_by_direct_ref_p = false;
  bool return_by_invisi_ref_p = false;
  bool return_unconstrained_p = false;
  bool incomplete_profile_p = false;
  int num;

  /* Look into the return type and get its associated GCC tree if it is not
     void, and then compute various flags for the subprogram type.  But make
     sure not to do this processing multiple times.  */
  if (Ekind (gnat_return_type) == E_Void)
    gnu_return_type = void_type_node;

  else if (gnu_type
	   && FUNC_OR_METHOD_TYPE_P (gnu_type)
	   && !TYPE_IS_DUMMY_P (TREE_TYPE (gnu_type)))
    {
      gnu_return_type = TREE_TYPE (gnu_type);
      return_unconstrained_p = TYPE_RETURN_UNCONSTRAINED_P (gnu_type);
      return_by_direct_ref_p = TYPE_RETURN_BY_DIRECT_REF_P (gnu_type);
      return_by_invisi_ref_p = TREE_ADDRESSABLE (gnu_type);
    }

  else
    {
      /* For foreign convention/intrinsic subprograms, return System.Address
	 as void * or equivalent; this comprises GCC builtins.  */
      if ((Has_Foreign_Convention (gnat_subprog)
	   || Is_Intrinsic_Subprogram (gnat_subprog))
	  && Is_Descendant_Of_Address (Underlying_Type (gnat_return_type)))
	gnu_return_type = ptr_type_node;
      else
	gnu_return_type = gnat_to_gnu_profile_type (gnat_return_type);

      /* If this function returns by reference, make the actual return type
	 the reference type and make a note of that.  */
      if (Returns_By_Ref (gnat_subprog))
	{
	  gnu_return_type = build_reference_type (gnu_return_type);
	  return_by_direct_ref_p = true;
	}

      /* If the return type is an unconstrained array type, the return value
	 will be allocated on the secondary stack so the actual return type
	 is the fat pointer type.  */
      else if (TREE_CODE (gnu_return_type) == UNCONSTRAINED_ARRAY_TYPE)
	{
	  gnu_return_type = TYPE_REFERENCE_TO (gnu_return_type);
	  return_unconstrained_p = true;
	}

      /* This is the same unconstrained array case, but for a dummy type.  */
      else if (TYPE_REFERENCE_TO (gnu_return_type)
	       && TYPE_IS_FAT_POINTER_P (TYPE_REFERENCE_TO (gnu_return_type)))
	{
	  gnu_return_type = TYPE_REFERENCE_TO (gnu_return_type);
	  return_unconstrained_p = true;
	}

      /* Likewise, if the return type requires a transient scope, the return
	 value will also be allocated on the secondary stack so the actual
	 return type is the reference type.  */
      else if (Requires_Transient_Scope (gnat_return_type))
	{
	  gnu_return_type = build_reference_type (gnu_return_type);
	  return_unconstrained_p = true;
	}

      /* If the Mechanism is By_Reference, ensure this function uses the
	 target's by-invisible-reference mechanism, which may not be the
	 same as above (e.g. it might be passing an extra parameter).  */
      else if (kind == E_Function && Mechanism (gnat_subprog) == By_Reference)
	return_by_invisi_ref_p = true;

      /* Likewise, if the return type is itself By_Reference.  */
      else if (TYPE_IS_BY_REFERENCE_P (gnu_return_type))
	return_by_invisi_ref_p = true;

      /* If the type is a padded type and the underlying type would not be
	 passed by reference or the function has a foreign convention, return
	 the underlying type.  */
      else if (TYPE_IS_PADDING_P (gnu_return_type)
	       && (!default_pass_by_ref
		      (TREE_TYPE (TYPE_FIELDS (gnu_return_type)))
		   || Has_Foreign_Convention (gnat_subprog)))
	gnu_return_type = TREE_TYPE (TYPE_FIELDS (gnu_return_type));

      /* If the return type is unconstrained, it must have a maximum size.
	 Use the padded type as the effective return type.  And ensure the
	 function uses the target's by-invisible-reference mechanism to
	 avoid copying too much data when it returns.  */
      if (CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_return_type)))
	{
	  tree orig_type = gnu_return_type;
	  tree max_return_size = max_size (TYPE_SIZE (gnu_return_type), true);

	  /* If the size overflows to 0, set it to an arbitrary positive
	     value so that assignments in the type are preserved.  Their
	     actual size is independent of this positive value.  */
	  if (TREE_CODE (max_return_size) == INTEGER_CST
	      && TREE_OVERFLOW (max_return_size)
	      && integer_zerop (max_return_size))
	    {
	      max_return_size = copy_node (bitsize_unit_node);
	      TREE_OVERFLOW (max_return_size) = 1;
	    }

	  gnu_return_type = maybe_pad_type (gnu_return_type, max_return_size,
					    0, gnat_subprog, false, definition,
					    true);

	  /* Declare it now since it will never be declared otherwise.  This
	     is necessary to ensure that its subtrees are properly marked.  */
	  if (gnu_return_type != orig_type
	      && !DECL_P (TYPE_NAME (gnu_return_type)))
	    create_type_decl (TYPE_NAME (gnu_return_type), gnu_return_type,
			      true, debug_info_p, gnat_subprog);

	  return_by_invisi_ref_p = true;
	}

      /* If the return type has a size that overflows, we usually cannot have
	 a function that returns that type.  This usage doesn't really make
	 sense anyway, so issue an error here.  */
      if (!return_by_invisi_ref_p
	  && TYPE_SIZE_UNIT (gnu_return_type)
	  && TREE_CODE (TYPE_SIZE_UNIT (gnu_return_type)) == INTEGER_CST
	  && !valid_constant_size_p (TYPE_SIZE_UNIT (gnu_return_type)))
	{
	  post_error ("cannot return type whose size overflows", gnat_subprog);
	  gnu_return_type = copy_type (gnu_return_type);
	  TYPE_SIZE (gnu_return_type) = bitsize_zero_node;
	  TYPE_SIZE_UNIT (gnu_return_type) = size_zero_node;
	}

      /* If the return type is incomplete, there are 2 cases: if the function
	 returns by reference, then the return type is only linked indirectly
	 in the profile, so the profile can be seen as complete since it need
	 not be further modified, only the reference types need be adjusted;
	 otherwise the profile is incomplete and need be adjusted too.  */
      if (TYPE_IS_DUMMY_P (gnu_return_type))
	{
	  associate_subprog_with_dummy_type (gnat_subprog, gnu_return_type);
	  incomplete_profile_p = true;
	}

      if (kind == E_Function)
	Set_Mechanism (gnat_subprog, return_unconstrained_p
				     || return_by_direct_ref_p
				     || return_by_invisi_ref_p
				     ? By_Reference : By_Copy);
    }

  /* A procedure (something that doesn't return anything) shouldn't be
     considered pure since there would be no reason for calling such a
     subprogram.  Note that procedures with Out (or In Out) parameters
     have already been converted into a function with a return type.
     Similarly, if the function returns an unconstrained type, then the
     function will allocate the return value on the secondary stack and
     thus calls to it cannot be CSE'ed, lest the stack be reclaimed.  */
  if (VOID_TYPE_P (gnu_return_type) || return_unconstrained_p)
    pure_flag = false;

  /* Loop over the parameters and get their associated GCC tree.  While doing
     this, build a copy-in copy-out structure if we need one.  */
  for (gnat_param = First_Formal_With_Extras (gnat_subprog), num = 0;
       Present (gnat_param);
       gnat_param = Next_Formal_With_Extras (gnat_param), num++)
    {
      const bool mech_is_by_ref
	= Mechanism (gnat_param) == By_Reference
	  && !(num == 0 && Is_Valued_Procedure (gnat_subprog));
      tree gnu_param_name = get_entity_name (gnat_param);
      tree gnu_param, gnu_param_type;
      bool cico = false;

      /* For a variadic C function, do not build unnamed parameters.  */
      if (variadic
	  && num == (Convention (gnat_subprog) - Convention_C_Variadic_0))
	break;

      /* Fetch an existing parameter with complete type and reuse it.  But we
	 didn't save the CICO property so we can only do it for In parameters
	 or parameters passed by reference.  */
      if ((Ekind (gnat_param) == E_In_Parameter || mech_is_by_ref)
	  && present_gnu_tree (gnat_param)
	  && (gnu_param = get_gnu_tree (gnat_param))
	  && !TYPE_IS_DUMMY_P (TREE_TYPE (gnu_param)))
	{
	  DECL_CHAIN (gnu_param) = NULL_TREE;
	  gnu_param_type = TREE_TYPE (gnu_param);
	}

      /* Otherwise translate the parameter type and act accordingly.  */
      else
	{
	  Entity_Id gnat_param_type = Etype (gnat_param);

	  /* For foreign convention/intrinsic subprograms, pass System.Address
	     as void * or equivalent; this comprises GCC builtins.  */
	  if ((Has_Foreign_Convention (gnat_subprog)
	       || Is_Intrinsic_Subprogram (gnat_subprog))
	      && Is_Descendant_Of_Address (Underlying_Type (gnat_param_type)))
	    gnu_param_type = ptr_type_node;
	  else
	    gnu_param_type = gnat_to_gnu_profile_type (gnat_param_type);

	  /* If the parameter type is incomplete, there are 2 cases: if it is
	     passed by reference, then the type is only linked indirectly in
	     the profile, so the profile can be seen as complete since it need
	     not be further modified, only the reference type need be adjusted;
	     otherwise the profile is incomplete and need be adjusted too.  */
	  if (TYPE_IS_DUMMY_P (gnu_param_type))
	    {
	      Node_Id gnat_decl;

	      if (mech_is_by_ref
		  || (TYPE_REFERENCE_TO (gnu_param_type)
		      && TYPE_IS_FAT_POINTER_P
			 (TYPE_REFERENCE_TO (gnu_param_type)))
		  || TYPE_IS_BY_REFERENCE_P (gnu_param_type))
		{
		  gnu_param_type = build_reference_type (gnu_param_type);
		  gnu_param
		    = create_param_decl (gnu_param_name, gnu_param_type);
		  TREE_READONLY (gnu_param) = 1;
		  DECL_BY_REF_P (gnu_param) = 1;
		  DECL_POINTS_TO_READONLY_P (gnu_param)
		    = (Ekind (gnat_param) == E_In_Parameter
		       && !Address_Taken (gnat_param));
		  Set_Mechanism (gnat_param, By_Reference);
		  Sloc_to_locus (Sloc (gnat_param),
				 &DECL_SOURCE_LOCATION (gnu_param));
		}

	      /* ??? This is a kludge to support null procedures in spec taking
		 a parameter with an untagged incomplete type coming from a
		 limited context.  The front-end creates a body without knowing
		 anything about the non-limited view, which is illegal Ada and
		 cannot be supported.  Create a parameter with a fake type.  */
	      else if (kind == E_Procedure
		       && (gnat_decl = Parent (gnat_subprog))
		       && Nkind (gnat_decl) == N_Procedure_Specification
		       && Null_Present (gnat_decl)
		       && Is_Incomplete_Type (gnat_param_type))
		gnu_param = create_param_decl (gnu_param_name, ptr_type_node);

	      else
		{
		  /* Build a minimal PARM_DECL without DECL_ARG_TYPE so that
		     Call_to_gnu will stop if it encounters the PARM_DECL.  */
		  gnu_param
		    = build_decl (input_location, PARM_DECL, gnu_param_name,
				  gnu_param_type);
		  associate_subprog_with_dummy_type (gnat_subprog,
						     gnu_param_type);
		  incomplete_profile_p = true;
		}
	    }

	  /* Otherwise build the parameter declaration normally.  */
	  else
	    {
	      gnu_param
		= gnat_to_gnu_param (gnat_param, gnu_param_type, num == 0,
				     gnat_subprog, &cico);

	      /* We are returned either a PARM_DECL or a type if no parameter
		 needs to be passed; in either case, adjust the type.  */
	      if (DECL_P (gnu_param))
		gnu_param_type = TREE_TYPE (gnu_param);
	      else
		{
		  gnu_param_type = gnu_param;
		  gnu_param = NULL_TREE;
		}
	    }
	}

      /* If we have a GCC tree for the parameter, register it.  */
      save_gnu_tree (gnat_param, NULL_TREE, false);
      if (gnu_param)
	{
	  gnu_param_type_list
	    = tree_cons (NULL_TREE, gnu_param_type, gnu_param_type_list);
	  DECL_CHAIN (gnu_param) = gnu_param_list;
	  gnu_param_list = gnu_param;
	  save_gnu_tree (gnat_param, gnu_param, false);

	  /* A pure function in the Ada sense which takes an access parameter
	     may modify memory through it and thus cannot be considered pure
	     in the GCC sense, unless it's access-to-function.  Likewise it if
	     takes a by-ref In Out or Out parameter.  But if it takes a by-ref
	     In parameter, then it may only read memory through it and can be
	     considered pure in the GCC sense.  */
	  if (pure_flag
	      && ((POINTER_TYPE_P (gnu_param_type)
		   && TREE_CODE (TREE_TYPE (gnu_param_type)) != FUNCTION_TYPE)
		  || TYPE_IS_FAT_POINTER_P (gnu_param_type)))
	    pure_flag = DECL_POINTS_TO_READONLY_P (gnu_param);
	}

      /* If the parameter uses the copy-in copy-out mechanism, allocate a field
	 for it in the return type and register the association.  */
      if (cico && !incomplete_profile_p)
	{
	  if (!gnu_cico_list)
	    {
	      gnu_cico_return_type = make_node (RECORD_TYPE);

	      /* If this is a function, we also need a field for the
		 return value to be placed.  */
	      if (!VOID_TYPE_P (gnu_return_type))
		{
		  tree gnu_field
		    = create_field_decl (get_identifier ("RETVAL"),
				         gnu_return_type,
				         gnu_cico_return_type, NULL_TREE,
				         NULL_TREE, 0, 0);
		  Sloc_to_locus (Sloc (gnat_subprog),
			         &DECL_SOURCE_LOCATION (gnu_field));
		  gnu_cico_field_list = gnu_field;
		  gnu_cico_list
		    = tree_cons (gnu_field, void_type_node, NULL_TREE);
		  if (!type_contains_only_integral_data (gnu_return_type))
		    gnu_cico_only_integral_type = false;
		}

	      TYPE_NAME (gnu_cico_return_type) = get_identifier ("RETURN");
	      /* Set a default alignment to speed up accesses.  But we should
		 not increase the size of the structure too much, lest it does
		 not fit in return registers anymore.  */
	      SET_TYPE_ALIGN (gnu_cico_return_type,
			      get_mode_alignment (ptr_mode));
	    }

	  tree gnu_field
	    = create_field_decl (gnu_param_name, gnu_param_type,
				 gnu_cico_return_type, NULL_TREE, NULL_TREE,
				 0, 0);
	  Sloc_to_locus (Sloc (gnat_param),
			 &DECL_SOURCE_LOCATION (gnu_field));
	  DECL_CHAIN (gnu_field) = gnu_cico_field_list;
	  gnu_cico_field_list = gnu_field;
	  gnu_cico_list = tree_cons (gnu_field, gnu_param, gnu_cico_list);
	  if (!type_contains_only_integral_data (gnu_param_type))
	    gnu_cico_only_integral_type = false;
	}
    }

  /* If the subprogram uses the copy-in copy-out mechanism, possibly adjust
     and finish up the return type.  */
  if (gnu_cico_list && !incomplete_profile_p)
    {
      /* If we have a CICO list but it has only one entry, we convert
	 this function into a function that returns this object.  */
      if (list_length (gnu_cico_list) == 1)
	gnu_cico_return_type = TREE_TYPE (TREE_PURPOSE (gnu_cico_list));

      /* Do not finalize the return type if the subprogram is stubbed
	 since structures are incomplete for the back-end.  */
      else if (Convention (gnat_subprog) != Convention_Stubbed)
	{
	  finish_record_type (gnu_cico_return_type,
			      nreverse (gnu_cico_field_list),
			      0, false);

	  /* Try to promote the mode if the return type is fully returned
	     in integer registers, again to speed up accesses.  */
	  if (TYPE_MODE (gnu_cico_return_type) == BLKmode
	      && gnu_cico_only_integral_type
	      && !targetm.calls.return_in_memory (gnu_cico_return_type,
						  NULL_TREE))
	    {
	      unsigned int size
		= TREE_INT_CST_LOW (TYPE_SIZE (gnu_cico_return_type));
	      unsigned int i = BITS_PER_UNIT;
	      scalar_int_mode mode;

	      while (i < size)
		i <<= 1;
	      if (int_mode_for_size (i, 0).exists (&mode))
		{
		  SET_TYPE_MODE (gnu_cico_return_type, mode);
		  SET_TYPE_ALIGN (gnu_cico_return_type,
				  GET_MODE_ALIGNMENT (mode));
		  TYPE_SIZE (gnu_cico_return_type)
		    = bitsize_int (GET_MODE_BITSIZE (mode));
		  TYPE_SIZE_UNIT (gnu_cico_return_type)
		    = size_int (GET_MODE_SIZE (mode));
		}
	    }

	  /* But demote the mode if the return type is partly returned in FP
	     registers to avoid creating problematic paradoxical subregs.
	     Note that we need to cater to historical 32-bit architectures
	     that incorrectly use the mode to select the return mechanism.  */
	  else if (INTEGRAL_MODE_P (TYPE_MODE (gnu_cico_return_type))
		   && !gnu_cico_only_integral_type
		   && BITS_PER_WORD >= 64
		   && !targetm.calls.return_in_memory (gnu_cico_return_type,
						       NULL_TREE))
	    SET_TYPE_MODE (gnu_cico_return_type, BLKmode);

	  if (debug_info_p)
	    rest_of_record_type_compilation (gnu_cico_return_type);
	}

      gnu_return_type = gnu_cico_return_type;
    }

  /* The lists have been built in reverse.  */
  gnu_param_type_list = nreverse (gnu_param_type_list);
  if (!variadic)
    gnu_param_type_list = chainon (gnu_param_type_list, void_list_node);
  gnu_param_list = nreverse (gnu_param_list);
  gnu_cico_list = nreverse (gnu_cico_list);

  /* Turn imported C++ constructors into their callable form as done in the
     front-end, i.e. add the "this" pointer and void the return type.  */
  if (method_p
      && Is_Constructor (gnat_subprog)
      && !VOID_TYPE_P (gnu_return_type))
    {
      tree gnu_param_type
	= build_pointer_type (gnat_to_gnu_profile_type (gnat_return_type));
      tree gnu_param_name = get_identifier (Get_Name_String (Name_uInit));
      tree gnu_param
	= build_decl (input_location, PARM_DECL, gnu_param_name,
		      gnu_param_type);
      gnu_param_type_list
	= tree_cons (NULL_TREE, gnu_param_type, gnu_param_type_list);
      DECL_CHAIN (gnu_param) = gnu_param_list;
      gnu_param_list = gnu_param;
      gnu_return_type = void_type_node;
    }

  /* If the profile is incomplete, we only set the (temporary) return and
     parameter types; otherwise, we build the full type.  In either case,
     we reuse an already existing GCC tree that we built previously here.  */
  if (incomplete_profile_p)
    {
      if (gnu_type && FUNC_OR_METHOD_TYPE_P (gnu_type))
	;
      else
	gnu_type = make_node (method_p ? METHOD_TYPE : FUNCTION_TYPE);
      TREE_TYPE (gnu_type) = gnu_return_type;
      TYPE_ARG_TYPES (gnu_type) = gnu_param_type_list;
      TYPE_RETURN_UNCONSTRAINED_P (gnu_type) = return_unconstrained_p;
      TYPE_RETURN_BY_DIRECT_REF_P (gnu_type) = return_by_direct_ref_p;
      TREE_ADDRESSABLE (gnu_type) = return_by_invisi_ref_p;
    }
  else
    {
      if (gnu_type && FUNC_OR_METHOD_TYPE_P (gnu_type))
	{
	  TREE_TYPE (gnu_type) = gnu_return_type;
	  TYPE_ARG_TYPES (gnu_type) = gnu_param_type_list;
	  if (method_p)
	    {
	      tree gnu_basetype = TREE_TYPE (TREE_VALUE (gnu_param_type_list));
	      TYPE_METHOD_BASETYPE (gnu_type)
		= TYPE_MAIN_VARIANT (gnu_basetype);
	    }
	  TYPE_CI_CO_LIST (gnu_type) = gnu_cico_list;
	  TYPE_RETURN_UNCONSTRAINED_P (gnu_type) = return_unconstrained_p;
	  TYPE_RETURN_BY_DIRECT_REF_P (gnu_type) = return_by_direct_ref_p;
	  TREE_ADDRESSABLE (gnu_type) = return_by_invisi_ref_p;
	  TYPE_CANONICAL (gnu_type) = gnu_type;
	  layout_type (gnu_type);
	}
      else
	{
	  if (method_p)
	    {
	      tree gnu_basetype = TREE_TYPE (TREE_VALUE (gnu_param_type_list));
	      gnu_type
		= build_method_type_directly (gnu_basetype, gnu_return_type,
					      TREE_CHAIN (gnu_param_type_list));
	    }
	  else
	    gnu_type
	      = build_function_type (gnu_return_type, gnu_param_type_list);

	  /* GNU_TYPE may be shared since GCC hashes types.  Unshare it if it
	     has a different TYPE_CI_CO_LIST or flags.  */
	  if (!fntype_same_flags_p (gnu_type, gnu_cico_list,
				    return_unconstrained_p,
				    return_by_direct_ref_p,
				    return_by_invisi_ref_p))
	    {
	      gnu_type = copy_type (gnu_type);
	      TYPE_CI_CO_LIST (gnu_type) = gnu_cico_list;
	      TYPE_RETURN_UNCONSTRAINED_P (gnu_type) = return_unconstrained_p;
	      TYPE_RETURN_BY_DIRECT_REF_P (gnu_type) = return_by_direct_ref_p;
	      TREE_ADDRESSABLE (gnu_type) = return_by_invisi_ref_p;
	    }
	}

      if (pure_flag)
	gnu_type = change_qualified_type (gnu_type, TYPE_QUAL_RESTRICT);

      if (No_Return (gnat_subprog))
	gnu_type = change_qualified_type (gnu_type, TYPE_QUAL_VOLATILE);

      /* If this subprogram is expectedly bound to a GCC builtin, fetch the
	 corresponding DECL node and check the parameter association.  */
      if (Is_Intrinsic_Subprogram (gnat_subprog)
	  && Present (Interface_Name (gnat_subprog)))
	{
	  tree gnu_ext_name = create_concat_name (gnat_subprog, NULL);
	  tree gnu_builtin_decl = builtin_decl_for (gnu_ext_name);

	  /* If we have a builtin DECL for that function, use it.  Check if
	     the profiles are compatible and warn if they are not.  Note that
	     the checker is expected to post diagnostics in this case.  */
	  if (gnu_builtin_decl)
	    {
	      if (fndecl_built_in_p (gnu_builtin_decl, BUILT_IN_NORMAL))
		{
		  const enum built_in_function fncode
		    = DECL_FUNCTION_CODE (gnu_builtin_decl);

		  switch (fncode)
		  {
		    case BUILT_IN_SYNC_FETCH_AND_ADD_N:
		    case BUILT_IN_SYNC_FETCH_AND_SUB_N:
		    case BUILT_IN_SYNC_FETCH_AND_OR_N:
		    case BUILT_IN_SYNC_FETCH_AND_AND_N:
		    case BUILT_IN_SYNC_FETCH_AND_XOR_N:
		    case BUILT_IN_SYNC_FETCH_AND_NAND_N:
		    case BUILT_IN_SYNC_ADD_AND_FETCH_N:
		    case BUILT_IN_SYNC_SUB_AND_FETCH_N:
		    case BUILT_IN_SYNC_OR_AND_FETCH_N:
		    case BUILT_IN_SYNC_AND_AND_FETCH_N:
		    case BUILT_IN_SYNC_XOR_AND_FETCH_N:
		    case BUILT_IN_SYNC_NAND_AND_FETCH_N:
		    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_N:
		    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_N:
		    case BUILT_IN_ATOMIC_EXCHANGE_N:
		    case BUILT_IN_ATOMIC_LOAD_N:
		    case BUILT_IN_ATOMIC_ADD_FETCH_N:
		    case BUILT_IN_ATOMIC_SUB_FETCH_N:
		    case BUILT_IN_ATOMIC_AND_FETCH_N:
		    case BUILT_IN_ATOMIC_NAND_FETCH_N:
		    case BUILT_IN_ATOMIC_XOR_FETCH_N:
		    case BUILT_IN_ATOMIC_OR_FETCH_N:
		    case BUILT_IN_ATOMIC_FETCH_ADD_N:
		    case BUILT_IN_ATOMIC_FETCH_SUB_N:
		    case BUILT_IN_ATOMIC_FETCH_AND_N:
		    case BUILT_IN_ATOMIC_FETCH_NAND_N:
		    case BUILT_IN_ATOMIC_FETCH_XOR_N:
		    case BUILT_IN_ATOMIC_FETCH_OR_N:
		      /* This is a generic builtin overloaded on its return
			 type, so do type resolution based on it.  */
		      if (!VOID_TYPE_P (gnu_return_type)
			  && type_for_atomic_builtin_p (gnu_return_type))
			gnu_builtin_decl
			  = resolve_atomic_builtin (fncode, gnu_return_type);
		      else
			{
			  post_error
			    ("??cannot import type-generic 'G'C'C builtin!",
			     gnat_subprog);
			  post_error
			    ("\\?use a supported result type",
			     gnat_subprog);
			  gnu_builtin_decl = NULL_TREE;
			}
		      break;

		    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_N:
		      /* This is a generic builtin overloaded on its third
			 parameter type, so do type resolution based on it.  */
		      if (list_length (gnu_param_type_list) >= 4
			  && type_for_atomic_builtin_p
			       (list_third (gnu_param_type_list)))
			gnu_builtin_decl
			  = resolve_atomic_builtin
			      (fncode, list_third (gnu_param_type_list));
		      else
			{
			  post_error
			    ("??cannot import type-generic 'G'C'C builtin!",
			     gnat_subprog);
			  post_error
			    ("\\?use a supported third parameter type",
			     gnat_subprog);
			  gnu_builtin_decl = NULL_TREE;
			}
		      break;

		    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_N:
		    case BUILT_IN_SYNC_LOCK_RELEASE_N:
		    case BUILT_IN_ATOMIC_STORE_N:
		      post_error
			("??unsupported type-generic 'G'C'C builtin!",
			 gnat_subprog);
		      gnu_builtin_decl = NULL_TREE;
		      break;

		    default:
		      break;
		  }
		}

	      if (gnu_builtin_decl)
		{
		  const intrin_binding_t inb
		    = { gnat_subprog, gnu_type, TREE_TYPE (gnu_builtin_decl) };

		  if (!intrin_profiles_compatible_p (&inb))
		    post_error
		      ("??profile of& doesn''t match the builtin it binds!",
		       gnat_subprog);

		  return gnu_builtin_decl;
		}
	    }

	  /* Inability to find the builtin DECL most often indicates a genuine
	     mistake, but imports of unregistered intrinsics are sometimes used
	     on purpose to allow hooking in alternate bodies; we post a warning
	     conditioned on Wshadow in this case, to let developers be notified
	     on demand without risking false positives with common default sets
	     of options.  */
	  if (warn_shadow)
	    post_error ("'G'C'C builtin not found for&!??", gnat_subprog);
	}
    }

  *param_list = gnu_param_list;

  return gnu_type;
}

/* Return the external name for GNAT_SUBPROG given its entity name.  */

static tree
gnu_ext_name_for_subprog (Entity_Id gnat_subprog, tree gnu_entity_name)
{
  tree gnu_ext_name = create_concat_name (gnat_subprog, NULL);

  /* If there was no specified Interface_Name and the external and
     internal names of the subprogram are the same, only use the
     internal name to allow disambiguation of nested subprograms.  */
  if (No (Interface_Name (gnat_subprog)) && gnu_ext_name == gnu_entity_name)
    gnu_ext_name = NULL_TREE;

  return gnu_ext_name;
}

/* Set TYPE_NONALIASED_COMPONENT on an array type built by means of
   build_nonshared_array_type.  */

static void
set_nonaliased_component_on_array_type (tree type)
{
  TYPE_NONALIASED_COMPONENT (type) = 1;
  if (TYPE_CANONICAL (type))
    TYPE_NONALIASED_COMPONENT (TYPE_CANONICAL (type)) = 1;
}

/* Set TYPE_REVERSE_STORAGE_ORDER on an array type built by means of
   build_nonshared_array_type.  */

static void
set_reverse_storage_order_on_array_type (tree type)
{
  TYPE_REVERSE_STORAGE_ORDER (type) = 1;
  if (TYPE_CANONICAL (type))
    TYPE_REVERSE_STORAGE_ORDER (TYPE_CANONICAL (type)) = 1;
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
  /* If the array type has an aliased component in the front-end sense,
     then it also has an aliased component in the back-end sense.  */
  if (Has_Aliased_Components (gnat_type))
    return false;

  /* If this is a derived type, then it has a non-aliased component if
     and only if its parent type also has one.  */
  if (Is_Derived_Type (gnat_type))
    {
      tree gnu_parent_type = gnat_to_gnu_type (Etype (gnat_type));
      if (TREE_CODE (gnu_parent_type) == UNCONSTRAINED_ARRAY_TYPE)
	gnu_parent_type
	  = TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_parent_type))));
      return TYPE_NONALIASED_COMPONENT (gnu_parent_type);
    }

  /* For a multi-dimensional array type, find the component type.  */
  while (TREE_CODE (TREE_TYPE (gnu_type)) == ARRAY_TYPE
	 && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_type)))
    gnu_type = TREE_TYPE (gnu_type);

  /* Consider that an array of pointers has an aliased component, which is
     sort of logical and helps with Taft Amendment types in LTO mode.  */
  if (POINTER_TYPE_P (TREE_TYPE (gnu_type)))
    return false;

  /* Otherwise, rely exclusively on properties of the element type.  */
  return type_for_nonaliased_component_p (TREE_TYPE (gnu_type));
}

/* Return true if GNAT_ADDRESS is a value known at compile-time.  */

static bool
compile_time_known_address_p (Node_Id gnat_address)
{
  /* Handle reference to a constant.  */
  if (Is_Entity_Name (gnat_address)
      && Ekind (Entity (gnat_address)) == E_Constant)
    {
      gnat_address = Constant_Value (Entity (gnat_address));
      if (No (gnat_address))
	return false;
    }

  /* Catch System'To_Address.  */
  if (Nkind (gnat_address) == N_Unchecked_Type_Conversion)
    gnat_address = Expression (gnat_address);

  return Compile_Time_Known_Value (gnat_address);
}

/* Return true if GNAT_INDIC, a N_Subtype_Indication node for the index of a
   FLB, cannot yield superflat objects, i.e. if the inequality HB >= LB - 1
   is true for these objects.  LB and HB are the low and high bounds.  */

static bool
flb_cannot_be_superflat (Node_Id gnat_indic)
{
  const Entity_Id gnat_type = Entity (Subtype_Mark (gnat_indic));
  const Entity_Id gnat_subtype = Etype (gnat_indic);
  Node_Id gnat_scalar_range, gnat_lb, gnat_hb;
  tree gnu_lb, gnu_hb, gnu_lb_minus_one;

  /* This is a FLB so LB is fixed.  */
  if ((Ekind (gnat_subtype) == E_Signed_Integer_Subtype
       || Ekind (gnat_subtype) == E_Modular_Integer_Subtype)
      && (gnat_scalar_range = Scalar_Range (gnat_subtype)))
    {
      gnat_lb = Low_Bound (gnat_scalar_range);
      gcc_assert (Nkind (gnat_lb) == N_Integer_Literal);
    }
  else
    return false;

  /* The low bound of the type is a lower bound for HB.  */
  if ((Ekind (gnat_type) == E_Signed_Integer_Subtype
       || Ekind (gnat_type) == E_Modular_Integer_Subtype)
      && (gnat_scalar_range = Scalar_Range (gnat_type)))
    {
      gnat_hb = Low_Bound (gnat_scalar_range);
      gcc_assert (Nkind (gnat_hb) == N_Integer_Literal);
    }
  else
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

/* Return true if GNAT_RANGE, a N_Range node, cannot be superflat, i.e. if the
   inequality HB >= LB - 1 is true.  LB and HB are the low and high bounds.  */

static bool
range_cannot_be_superflat (Node_Id gnat_range)
{
  Node_Id gnat_lb = Low_Bound (gnat_range), gnat_hb = High_Bound (gnat_range);
  Node_Id gnat_scalar_range;
  tree gnu_lb, gnu_hb, gnu_lb_minus_one;

  /* If the low bound is not constant, take the worst case by finding an upper
     bound for its type, repeatedly if need be.  */
  while (Nkind (gnat_lb) != N_Integer_Literal
	 && (Ekind (Etype (gnat_lb)) == E_Signed_Integer_Subtype
	     || Ekind (Etype (gnat_lb)) == E_Modular_Integer_Subtype)
	 && (gnat_scalar_range = Scalar_Range (Etype (gnat_lb)))
	 && (Nkind (gnat_scalar_range) == N_Signed_Integer_Type_Definition
	     || Nkind (gnat_scalar_range) == N_Range))
    gnat_lb = High_Bound (gnat_scalar_range);

  /* If the high bound is not constant, take the worst case by finding a lower
     bound for its type, repeatedly if need be.  */
  while (Nkind (gnat_hb) != N_Integer_Literal
	 && (Ekind (Etype (gnat_hb)) == E_Signed_Integer_Subtype
	     || Ekind (Etype (gnat_hb)) == E_Modular_Integer_Subtype)
	 && (gnat_scalar_range = Scalar_Range (Etype (gnat_hb)))
	 && (Nkind (gnat_scalar_range) == N_Signed_Integer_Type_Definition
	     || Nkind (gnat_scalar_range) == N_Range))
    gnat_hb = Low_Bound (gnat_scalar_range);

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

/* Return true if the size in units represented by GNU_SIZE can be handled by
   an allocation.  If STATIC_P is true, consider only what can be done with a
   static allocation.  */

static bool
allocatable_size_p (tree gnu_size, bool static_p)
{
  /* We can allocate a fixed size if it is a valid for the middle-end.  */
  if (TREE_CODE (gnu_size) == INTEGER_CST)
    return valid_constant_size_p (gnu_size);

  /* We can allocate a variable size if this isn't a static allocation.  */
  else
    return !static_p;
}

/* Return true if GNU_EXPR needs a conversion to GNU_TYPE when used as the
   initial value of an object of GNU_TYPE.  */

static bool
initial_value_needs_conversion (tree gnu_type, tree gnu_expr)
{
  /* Do not convert if the object's type is unconstrained because this would
     generate useless evaluations of the CONSTRUCTOR to compute the size.  */
  if (TREE_CODE (gnu_type) == UNCONSTRAINED_ARRAY_TYPE
      || CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_type)))
    return false;

  /* Do not convert if the object's type is a padding record whose field is of
     self-referential size because we want to copy only the actual data.  */
  if (type_is_padding_self_referential (gnu_type))
    return false;

  /* Do not convert a call to a function that returns with variable size since
     we want to use the return slot optimization in this case.  */
  if (TREE_CODE (gnu_expr) == CALL_EXPR
      && return_type_with_variable_size_p (TREE_TYPE (gnu_expr)))
    return false;

  /* Do not convert to a record type with a variant part from a record type
     without one, to keep the object simpler.  */
  if (TREE_CODE (gnu_type) == RECORD_TYPE
      && TREE_CODE (TREE_TYPE (gnu_expr)) == RECORD_TYPE
      && get_variant_part (gnu_type)
      && !get_variant_part (TREE_TYPE (gnu_expr)))
    return false;

  /* In all the other cases, convert the expression to the object's type.  */
  return true;
}

/* Add the contribution of [MIN, MAX] to the current number of elements N_ELEM
   of an array type and return the result, or NULL_TREE if it overflowed.  */

static tree
update_n_elem (tree n_elem, tree min, tree max)
{
  /* First deal with the empty case.  */
  if (TREE_CODE (min) == INTEGER_CST
      && TREE_CODE (max) == INTEGER_CST
      && tree_int_cst_lt (max, min))
    return size_zero_node;

  min = convert (sizetype, min);
  max = convert (sizetype, max);

  /* Compute the number of elements in this dimension.  */
  tree this_n_elem
    = size_binop (PLUS_EXPR, size_one_node, size_binop (MINUS_EXPR, max, min));

  if (TREE_CODE (this_n_elem) == INTEGER_CST && TREE_OVERFLOW (this_n_elem))
    return NULL_TREE;

  /* Multiply the current number of elements by the result.  */
  n_elem = size_binop (MULT_EXPR, n_elem, this_n_elem);

  if (TREE_CODE (n_elem) == INTEGER_CST && TREE_OVERFLOW (n_elem))
    return NULL_TREE;

  return n_elem;
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
	  elaborate_expression (gnat_lb, gnat_entity, "L", true, false,
				Needs_Debug_Info (gnat_entity));
	if (!Raises_Constraint_Error (gnat_hb))
	  elaborate_expression (gnat_hb, gnat_entity, "U", true, false,
				Needs_Debug_Info (gnat_entity));
      break;
      }

    case E_Record_Subtype:
    case E_Private_Subtype:
    case E_Limited_Private_Subtype:
    case E_Record_Subtype_With_Private:
      if (Has_Discriminants (gnat_entity) && Is_Constrained (gnat_entity))
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
	    /* Ignore access discriminants.  */
	    if (!Is_Access_Type (Etype (Node (gnat_discriminant_expr))))
	      elaborate_expression (Node (gnat_discriminant_expr),
				    gnat_entity, get_entity_char (gnat_field),
				    true, false, false);
	}
      break;

    }
}

/* Prepend to ATTR_LIST an entry for an attribute with provided TYPE,
   NAME, ARGS and ERROR_POINT.  */

static void
prepend_one_attribute (struct attrib **attr_list,
		       enum attrib_type attrib_type,
		       tree attr_name,
		       tree attr_args,
		       Node_Id attr_error_point)
{
  struct attrib * attr = (struct attrib *) xmalloc (sizeof (struct attrib));

  attr->type = attrib_type;
  attr->name = attr_name;
  attr->args = attr_args;
  attr->error_point = attr_error_point;

  attr->next = *attr_list;
  *attr_list = attr;
}

/* Prepend to ATTR_LIST an entry for an attribute provided by GNAT_PRAGMA.  */

static void
prepend_one_attribute_pragma (struct attrib **attr_list, Node_Id gnat_pragma)
{
  const Node_Id gnat_arg = First (Pragma_Argument_Associations (gnat_pragma));
  Node_Id gnat_next_arg = Next (gnat_arg);
  tree gnu_arg1 = NULL_TREE, gnu_arg_list = NULL_TREE;
  enum attrib_type etype;

  /* Map the pragma at hand.  Skip if this isn't one we know how to handle.  */
  switch (Get_Pragma_Id (Chars (Pragma_Identifier (gnat_pragma))))
    {
    case Pragma_Linker_Alias:
      etype = ATTR_LINK_ALIAS;
      break;

    case Pragma_Linker_Constructor:
      etype = ATTR_LINK_CONSTRUCTOR;
      break;

    case Pragma_Linker_Destructor:
      etype = ATTR_LINK_DESTRUCTOR;
      break;

    case Pragma_Linker_Section:
      etype = ATTR_LINK_SECTION;
      break;

    case Pragma_Machine_Attribute:
      etype = ATTR_MACHINE_ATTRIBUTE;
      break;

    case Pragma_Thread_Local_Storage:
      etype = ATTR_THREAD_LOCAL_STORAGE;
      break;

    case Pragma_Weak_External:
      etype = ATTR_WEAK_EXTERNAL;
      break;

    default:
      return;
    }

  /* See what arguments we have and turn them into GCC trees for attribute
     handlers.  The first one is always expected to be a string meant to be
     turned into an identifier.  The next ones are all static expressions,
     among which strings meant to be turned into an identifier, except for
     a couple of specific attributes that require raw strings.  */
  if (Present (gnat_next_arg))
    {
      gnu_arg1 = gnat_to_gnu (Expression (gnat_next_arg));
      gcc_assert (TREE_CODE (gnu_arg1) == STRING_CST);

      const char *const p = TREE_STRING_POINTER (gnu_arg1);
      const bool string_args
	= strcmp (p, "target") == 0 || strcmp (p, "target_clones") == 0;
      gnu_arg1 = get_identifier (p);
      if (IDENTIFIER_LENGTH (gnu_arg1) == 0)
	return;
      gnat_next_arg = Next (gnat_next_arg);

      while (Present (gnat_next_arg))
	{
	  tree gnu_arg = gnat_to_gnu (Expression (gnat_next_arg));
	  if (TREE_CODE (gnu_arg) == STRING_CST && !string_args)
	    gnu_arg = get_identifier (TREE_STRING_POINTER (gnu_arg));
	  gnu_arg_list
	    = chainon (gnu_arg_list, build_tree_list (NULL_TREE, gnu_arg));
	  gnat_next_arg = Next (gnat_next_arg);
	}
    }

  prepend_one_attribute (attr_list, etype, gnu_arg1, gnu_arg_list,
			 Present (Next (gnat_arg))
			 ? Expression (Next (gnat_arg)) : gnat_pragma);
}

/* Prepend to ATTR_LIST the list of attributes for GNAT_ENTITY, if any.  */

static void
prepend_attributes (struct attrib **attr_list, Entity_Id gnat_entity)
{
  Node_Id gnat_temp;

  /* Attributes are stored as Representation Item pragmas.  */
  for (gnat_temp = First_Rep_Item (gnat_entity);
       Present (gnat_temp);
       gnat_temp = Next_Rep_Item (gnat_temp))
    if (Nkind (gnat_temp) == N_Pragma)
      prepend_one_attribute_pragma (attr_list, gnat_temp);
}

/* Given a GNAT tree GNAT_EXPR, for an expression which is a value within a
   type definition (either a bound or a discriminant value) for GNAT_ENTITY,
   return the GCC tree to use for that expression.  S is the suffix to use
   if a variable needs to be created and DEFINITION is true if this is done
   for a definition of GNAT_ENTITY.  If NEED_VALUE is true, we need a result;
   otherwise, we are just elaborating the expression for side-effects.  If
   NEED_FOR_DEBUG is true, we need a variable for debugging purposes even
   if it isn't needed for code generation.  */

static tree
elaborate_expression (Node_Id gnat_expr, Entity_Id gnat_entity, const char *s,
		      bool definition, bool need_value, bool need_for_debug)
{
  tree gnu_expr;

  /* If we already elaborated this expression (e.g. it was involved
     in the definition of a private type), use the old value.  */
  if (present_gnu_tree (gnat_expr))
    return get_gnu_tree (gnat_expr);

  /* If we don't need a value and this is static or a discriminant,
     we don't need to do anything.  */
  if (!need_value
      && (Compile_Time_Known_Value (gnat_expr)
	  || (Nkind (gnat_expr) == N_Identifier
	      && Ekind (Entity (gnat_expr)) == E_Discriminant)))
    return NULL_TREE;

  /* If it's a static expression, we don't need a variable for debugging.  */
  if (need_for_debug && Compile_Time_Known_Value (gnat_expr))
    need_for_debug = false;

  /* Otherwise, convert this tree to its GCC equivalent and elaborate it.  */
  gnu_expr = elaborate_expression_1 (gnat_to_gnu (gnat_expr), gnat_entity, s,
				     definition, need_for_debug);

  /* Save the expression in case we try to elaborate this entity again.  Since
     it's not a DECL, don't check it.  Don't save if it's a discriminant.  */
  if (!CONTAINS_PLACEHOLDER_P (gnu_expr))
    save_gnu_tree (gnat_expr, gnu_expr, true);

  return need_value ? gnu_expr : error_mark_node;
}

/* Similar, but take a GNU expression and always return a result.  */

static tree
elaborate_expression_1 (tree gnu_expr, Entity_Id gnat_entity, const char *s,
			bool definition, bool need_for_debug)
{
  const bool expr_public_p = Is_Public (gnat_entity);
  const bool expr_global_p = expr_public_p || global_bindings_p ();
  bool expr_variable_p, use_variable;

  /* If GNU_EXPR contains a placeholder, just return it.  We rely on the fact
     that an expression cannot contain both a discriminant and a variable.  */
  if (CONTAINS_PLACEHOLDER_P (gnu_expr))
    return gnu_expr;

  /* If GNU_EXPR is neither a constant nor based on a read-only variable, make
     a variable that is initialized to contain the expression when the package
     containing the definition is elaborated.  If this entity is defined at top
     level, replace the expression by the variable; otherwise use a SAVE_EXPR
     if this is necessary.  */
  if (TREE_CONSTANT (gnu_expr))
    expr_variable_p = false;
  else
    {
      /* Skip any conversions and simple constant arithmetics to see if the
	 expression is based on a read-only variable.  */
      tree inner = remove_conversions (gnu_expr, true);

      inner = skip_simple_constant_arithmetic (inner);

      if (handled_component_p (inner))
	inner = get_inner_constant_reference (inner);

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
		         && definition
			 && Is_Itype (gnat_entity)
			 && Nkind (Associated_Node_For_Itype (gnat_entity))
			    == N_Loop_Parameter_Specification));

  /* If the GNAT encodings are not used, we don't need a variable for debug
     info purposes if the expression is a constant or another variable, but
     we must be careful because we do not generate debug info for external
     variables so DECL_IGNORED_P is not stable across units.  */
  if (need_for_debug
      && gnat_encodings != DWARF_GNAT_ENCODINGS_ALL
      && (TREE_CONSTANT (gnu_expr)
	  || (!expr_public_p
	      && DECL_P (gnu_expr)
	      && !DECL_IGNORED_P (gnu_expr))))
    need_for_debug = false;

  /* Now create it, possibly only for debugging purposes.  */
  if (use_variable || need_for_debug)
    {
      /* The following variable creation can happen when processing the body
	 of subprograms that are defined outside of the extended main unit and
	 inlined.  In this case, we are not at the global scope, and thus the
	 new variable must not be tagged "external", as we used to do here as
	 soon as DEFINITION was false.  And note that we test Needs_Debug_Info
	 here instead of NEED_FOR_DEBUG because, once the variable is created,
	 whether or not debug information is generated for it is orthogonal to
	 the reason why it was created in the first place.  */
      tree gnu_decl
	= create_var_decl (create_concat_name (gnat_entity, s), NULL_TREE,
			   TREE_TYPE (gnu_expr), gnu_expr, true,
			   expr_public_p, !definition && expr_global_p,
			   expr_global_p, false, true,
			   Needs_Debug_Info (gnat_entity),
			   NULL, gnat_entity, false);

      /* Using this variable for debug (if need_for_debug is true) requires
	 a proper location.  The back-end will compute a location for this
	 variable only if the variable is used by the generated code.
	 Returning the variable ensures the caller will use it in generated
	 code.  Note that there is no need for a location if the debug info
	 contains an integer constant.  */
      if (use_variable || (need_for_debug && !TREE_CONSTANT (gnu_expr)))
	return gnu_decl;
    }

  return expr_variable_p ? gnat_save_expr (gnu_expr) : gnu_expr;
}

/* Similar, but take an alignment factor and make it explicit in the tree.  */

static tree
elaborate_expression_2 (tree gnu_expr, Entity_Id gnat_entity, const char *s,
			bool definition, bool need_for_debug, unsigned int align)
{
  tree unit_align = size_int (align / BITS_PER_UNIT);
  return
    size_binop (MULT_EXPR,
		elaborate_expression_1 (size_binop (EXACT_DIV_EXPR,
						    gnu_expr,
						    unit_align),
					gnat_entity, s, definition,
					need_for_debug),
		unit_align);
}

/* Structure to hold internal data for elaborate_reference.  */

struct er_data
{
  Entity_Id entity;
  bool definition;
  unsigned int n;
};

/* Wrapper function around elaborate_expression_1 for elaborate_reference.  */

static tree
elaborate_reference_1 (tree ref, void *data)
{
  struct er_data *er = (struct er_data *)data;
  char suffix[16];

  /* This is what elaborate_expression_1 does if NEED_DEBUG is false.  */
  if (TREE_CONSTANT (ref))
    return ref;

  /* If this is a COMPONENT_REF of a fat pointer, elaborate the entire fat
     pointer.  This may be more efficient, but will also allow us to more
     easily find the match for the PLACEHOLDER_EXPR.  */
  if (TREE_CODE (ref) == COMPONENT_REF
      && TYPE_IS_FAT_POINTER_P (TREE_TYPE (TREE_OPERAND (ref, 0))))
    return build3 (COMPONENT_REF, TREE_TYPE (ref),
		   elaborate_reference_1 (TREE_OPERAND (ref, 0), data),
		   TREE_OPERAND (ref, 1), NULL_TREE);

  /* If this is the displacement of a pointer, elaborate the pointer and then
     displace the result.  The actual purpose here is to drop the location on
     the expression, which may be problematic if replicated on references.  */
  if (TREE_CODE (ref) == POINTER_PLUS_EXPR
      && TREE_CODE (TREE_OPERAND (ref, 1)) == INTEGER_CST)
    return build2 (POINTER_PLUS_EXPR, TREE_TYPE (ref),
		   elaborate_reference_1 (TREE_OPERAND (ref, 0), data),
		   TREE_OPERAND (ref, 1));

  sprintf (suffix, "EXP%d", ++er->n);
  return
    elaborate_expression_1 (ref, er->entity, suffix, er->definition, false);
}

/* Elaborate the reference REF to be used as renamed object for GNAT_ENTITY.
   DEFINITION is true if this is done for a definition of GNAT_ENTITY and
   INIT is set to the first arm of a COMPOUND_EXPR present in REF, if any.  */

static tree
elaborate_reference (tree ref, Entity_Id gnat_entity, bool definition,
		     tree *init)
{
  struct er_data er = { gnat_entity, definition, 0 };
  return gnat_rewrite_reference (ref, elaborate_reference_1, &er, init);
}

/* Given a GNU tree and a GNAT list of choices, generate an expression to test
   the value passed against the list of choices.  */

static tree
choices_to_gnu (tree gnu_operand, Node_Id gnat_choices)
{
  tree gnu_result = boolean_false_node, gnu_type;

  gnu_operand = maybe_character_value (gnu_operand);
  gnu_type = TREE_TYPE (gnu_operand);

  for (Node_Id gnat_choice = First (gnat_choices);
       Present (gnat_choice);
       gnat_choice = Next (gnat_choice))
    {
      tree gnu_low = NULL_TREE, gnu_high = NULL_TREE;
      tree gnu_test;

      switch (Nkind (gnat_choice))
	{
	case N_Range:
	  gnu_low = gnat_to_gnu (Low_Bound (gnat_choice));
	  gnu_high = gnat_to_gnu (High_Bound (gnat_choice));
	  break;

	case N_Subtype_Indication:
	  gnu_low = gnat_to_gnu (Low_Bound (Range_Expression
					    (Constraint (gnat_choice))));
	  gnu_high = gnat_to_gnu (High_Bound (Range_Expression
					      (Constraint (gnat_choice))));
	  break;

	case N_Identifier:
	case N_Expanded_Name:
	  /* This represents either a subtype range or a static value of
	     some kind; Ekind says which.  */
	  if (Is_Type (Entity (gnat_choice)))
	    {
	      tree gnu_type = get_unpadded_type (Entity (gnat_choice));

	      gnu_low = TYPE_MIN_VALUE (gnu_type);
	      gnu_high = TYPE_MAX_VALUE (gnu_type);
	      break;
	    }

	  /* ... fall through ... */

	case N_Character_Literal:
	case N_Integer_Literal:
	  gnu_low = gnat_to_gnu (gnat_choice);
	  break;

	case N_Others_Choice:
	  break;

	default:
	  gcc_unreachable ();
	}

      /* Everything should be folded into constants at this point.  */
      gcc_assert (!gnu_low  || TREE_CODE (gnu_low)  == INTEGER_CST);
      gcc_assert (!gnu_high || TREE_CODE (gnu_high) == INTEGER_CST);

      if (gnu_low && TREE_TYPE (gnu_low) != gnu_type)
	gnu_low = convert (gnu_type, gnu_low);
      if (gnu_high && TREE_TYPE (gnu_high) != gnu_type)
	gnu_high = convert (gnu_type, gnu_high);

      if (gnu_low && gnu_high)
	gnu_test
	  = build_binary_op (TRUTH_ANDIF_EXPR, boolean_type_node,
			     build_binary_op (GE_EXPR, boolean_type_node,
					      gnu_operand, gnu_low, true),
			     build_binary_op (LE_EXPR, boolean_type_node,
					      gnu_operand, gnu_high, true),
			     true);
      else if (gnu_low == boolean_true_node
	       && TREE_TYPE (gnu_operand) == boolean_type_node)
	gnu_test = gnu_operand;
      else if (gnu_low)
	gnu_test
	  = build_binary_op (EQ_EXPR, boolean_type_node, gnu_operand, gnu_low,
			     true);
      else
	gnu_test = boolean_true_node;

      if (gnu_result == boolean_false_node)
	gnu_result = gnu_test;
      else
	gnu_result
	  = build_binary_op (TRUTH_ORIF_EXPR, boolean_type_node, gnu_result,
			     gnu_test, true);
    }

  return gnu_result;
}

/* Adjust PACKED setting as passed to gnat_to_gnu_field for a field of
   type FIELD_TYPE to be placed in RECORD_TYPE.  Return the result.  */

static int
adjust_packed (tree field_type, tree record_type, int packed)
{
  /* If the field contains an array with self-referential size, we'd better
     not pack it because this would misalign it and, therefore, cause large
     temporaries to be created in case we need to take the address of the
     field.  See addressable_p and the notes on the addressability issues
     for further details.  */
  if (AGGREGATE_TYPE_P (field_type)
      && aggregate_type_contains_array_p (field_type, true))
    return 0;

  /* In the other cases, we can honor the packing.  */
  if (packed)
    return packed;

  /* If the alignment of the record is specified and the field type
     is over-aligned, request Storage_Unit alignment for the field.  */
  if (TYPE_ALIGN (record_type)
      && TYPE_ALIGN (field_type) > TYPE_ALIGN (record_type))
    return -1;

  /* Likewise if the maximum alignment of the record is specified.  */
  if (TYPE_MAX_ALIGN (record_type)
      && TYPE_ALIGN (field_type) > TYPE_MAX_ALIGN (record_type))
    return -1;

  return 0;
}

/* Return a GCC tree for a field corresponding to GNAT_FIELD to be
   placed in GNU_RECORD_TYPE.

   PACKED is 1 if the enclosing record is packed or -1 if the enclosing
   record has Component_Alignment of Storage_Unit.

   DEFINITION is true if this field is for a record being defined.

   DEBUG_INFO_P is true if we need to write debug information for types
   that we may create in the process.  */

static tree
gnat_to_gnu_field (Entity_Id gnat_field, tree gnu_record_type, int packed,
		   bool definition, bool debug_info_p)
{
  const Node_Id gnat_clause = Component_Clause (gnat_field);
  const Entity_Id gnat_record_type = Underlying_Type (Scope (gnat_field));
  const Entity_Id gnat_field_type = Etype (gnat_field);
  tree gnu_field_type = gnat_to_gnu_type (gnat_field_type);
  tree gnu_field_id = get_entity_name (gnat_field);
  const bool is_aliased = Is_Aliased (gnat_field);
  const bool is_full_access
    = (Is_Full_Access (gnat_field) || Is_Full_Access (gnat_field_type));
  const bool is_independent
    = (Is_Independent (gnat_field) || Is_Independent (gnat_field_type));
  const bool is_volatile
    = (Treat_As_Volatile (gnat_field) || Treat_As_Volatile (gnat_field_type));
  const bool is_by_ref = TYPE_IS_BY_REFERENCE_P (gnu_field_type);
  const bool is_strict_alignment = Strict_Alignment (gnat_field_type);
  /* We used to consider that volatile fields also require strict alignment,
     but that was an interpolation and would cause us to reject a pragma
     volatile on a packed record type containing boolean components, while
     there is no basis to do so in the RM.  In such cases, the writes will
     involve load-modify-store sequences, but that's OK for volatile.  The
     only constraint is the implementation advice whereby only the bits of
     the components should be accessed if they both start and end on byte
     boundaries, but that should be guaranteed by the GCC memory model.
     Note that we have some redundancies (is_full_access => is_independent,
     is_aliased => is_independent and is_by_ref => is_strict_alignment)
     so the following formula is sufficient.  */
  const bool needs_strict_alignment = (is_independent || is_strict_alignment);
  const char *field_s, *size_s;
  tree gnu_field, gnu_size, gnu_pos;
  bool is_bitfield;

  /* Force the type of the Not_Handled_By_Others field to be that of the
     field in struct Exception_Data declared in raise.h instead of using
     the declared boolean type.  We need to do that because there is no
     easy way to make use of a C compatible boolean type for the latter.  */
  if (gnu_field_id == not_handled_by_others_name_id
      && gnu_field_type == boolean_type_node)
    gnu_field_type = char_type_node;

  /* The qualifier to be used in messages.  */
  if (is_aliased)
    field_s = "aliased&";
  else if (is_full_access)
    {
      if (Is_Volatile_Full_Access (gnat_field)
	  || Is_Volatile_Full_Access (gnat_field_type))
	field_s = "volatile full access&";
      else
	field_s = "atomic&";
    }
  else if (is_independent)
    field_s = "independent&";
  else if (is_by_ref)
    field_s = "& with by-reference type";
  else if (is_strict_alignment)
    field_s = "& with aliased part";
  else
    field_s = "&";

  /* The message to be used for incompatible size.  */
  if (is_aliased || is_full_access)
    size_s = "size for %s must be ^";
  else if (field_s)
    size_s = "size for %s too small{, minimum allowed is ^}";

  /* If a field requires strict alignment, we cannot pack it (RM 13.2(7)).  */
  if (needs_strict_alignment)
    packed = 0;
  else
    packed = adjust_packed (gnu_field_type, gnu_record_type, packed);

  /* If a size is specified, use it.  Otherwise, if the record type is packed,
     use the official RM size.  See "Handling of Type'Size Values" in Einfo
     for further details.  */
  if (Present (gnat_clause) || Known_Esize (gnat_field))
    gnu_size = validate_size (Esize (gnat_field), gnu_field_type, gnat_field,
			      FIELD_DECL, false, true, size_s, field_s);
  else if (packed == 1)
    {
      gnu_size = rm_size (gnu_field_type);
      if (TREE_CODE (gnu_size) != INTEGER_CST)
	gnu_size = NULL_TREE;
    }
  else
    gnu_size = NULL_TREE;

  /* Likewise for the position.  */
  if (Present (gnat_clause))
    {
      gnu_pos = UI_To_gnu (Component_Bit_Offset (gnat_field), bitsizetype);
      is_bitfield = !value_factor_p (gnu_pos, BITS_PER_UNIT);
    }

  /* If the record has rep clauses and this is the tag field, make a rep
     clause for it as well.  */
  else if (Has_Specified_Layout (gnat_record_type)
	   && Chars (gnat_field) == Name_uTag)
    {
      gnu_pos = bitsize_zero_node;
      gnu_size = TYPE_SIZE (gnu_field_type);
      is_bitfield = false;
    }

  else
    {
      gnu_pos = NULL_TREE;
      is_bitfield = false;
    }

  /* If the field's type is a fixed-size record that does not require strict
     alignment, and the record is packed or we have a position specified for
     the field that makes it a bitfield or we have a specified size that is
     smaller than that of the field's type, then see if we can get either an
     integral mode form of the field's type or a smaller form.  If we can,
     consider that a size was specified for the field if there wasn't one
     already, so we know to make it a bitfield and avoid making things wider.

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
      && tree_fits_uhwi_p (TYPE_SIZE (gnu_field_type))
      && (packed == 1
	  || is_bitfield
	  || (gnu_size
	      && tree_int_cst_lt (gnu_size, TYPE_SIZE (gnu_field_type)))))
    {
      tree gnu_packable_type
	= make_packable_type (gnu_field_type, true, is_bitfield ? 1 : 0);
      if (gnu_packable_type != gnu_field_type)
	{
	  gnu_field_type = gnu_packable_type;
	  if (!gnu_size)
	    gnu_size = rm_size (gnu_field_type);
	}
    }

  /* Now check if the type of the field allows atomic access.  */
  if (Is_Full_Access (gnat_field))
    {
      const unsigned int align
	= promote_object_alignment (gnu_field_type, NULL_TREE, gnat_field);
      if (align > 0)
	gnu_field_type
	  = maybe_pad_type (gnu_field_type, NULL_TREE, align, gnat_field,
			    false, definition, true);
      check_ok_for_atomic_type (gnu_field_type, gnat_field, false);
    }

  /* If a position is specified, check that it is valid.  */
  if (gnu_pos)
    {
      Entity_Id gnat_parent = Parent_Subtype (gnat_record_type);

      /* Ensure the position doesn't overlap with the parent subtype if there
	 is one.  It would be impossible to build CONSTRUCTORs and accessing
	 the parent could clobber the component in the extension if directly
	 done.  We accept it with -gnatd.K for the sake of compatibility.  */
      if (Present (gnat_parent)
	  && !(Debug_Flag_Dot_KK && Is_Fully_Repped_Tagged_Type (gnat_parent)))
	{
	  tree gnu_parent = gnat_to_gnu_type (gnat_parent);

	  if (TREE_CODE (TYPE_SIZE (gnu_parent)) == INTEGER_CST
	      && tree_int_cst_lt (gnu_pos, TYPE_SIZE (gnu_parent)))
	    post_error_ne_tree
	      ("position for& must be beyond parent{, minimum allowed is ^}",
	       Position (gnat_clause), gnat_field, TYPE_SIZE_UNIT (gnu_parent));
	}

      /* If this field needs strict alignment, make sure that the record is
	 sufficiently aligned and that the position and size are consistent
	 with the type.  But don't do it if we are just annotating types and
	 the field's type is tagged, since tagged types aren't fully laid out
	 in this mode.  Also, note that atomic implies volatile so the inner
	 test sequences ordering is significant here.  */
      if (needs_strict_alignment
	  && !(type_annotate_only && Is_Tagged_Type (gnat_field_type)))
	{
	  const unsigned int type_align = TYPE_ALIGN (gnu_field_type);

	  if (TYPE_ALIGN (gnu_record_type)
	      && TYPE_ALIGN (gnu_record_type) < type_align)
	    SET_TYPE_ALIGN (gnu_record_type, type_align);

	  /* If the position is not a multiple of the storage unit, then error
	     out and reset the position.  */
	  if (!integer_zerop (size_binop (TRUNC_MOD_EXPR, gnu_pos,
					  bitsize_unit_node)))
	    {
	      char s[128];
	      snprintf (s, sizeof (s), "position for %s must be "
			"multiple of Storage_Unit", field_s);
	      post_error_ne (s, First_Bit (gnat_clause), gnat_field);
	      gnu_pos = NULL_TREE;
	    }

	  /* If the position is not a multiple of the alignment of the type,
	     then error out and reset the position.  */
	  else if (type_align > BITS_PER_UNIT
		   && !integer_zerop (size_binop (TRUNC_MOD_EXPR, gnu_pos,
						  bitsize_int (type_align))))
	    {
	      char s[128];
              snprintf (s, sizeof (s), "position for %s must be multiple of ^",
			field_s);
	      post_error_ne_num (s, First_Bit (gnat_clause), gnat_field,
				 type_align / BITS_PER_UNIT);
	      post_error_ne_num ("\\because alignment of its type& is ^",
				 First_Bit (gnat_clause), Etype (gnat_field),
				 type_align / BITS_PER_UNIT);
	      gnu_pos = NULL_TREE;
	    }

	  if (gnu_size)
	    {
	      tree type_size = TYPE_SIZE (gnu_field_type);
	      int cmp;

	      /* If the size is not a multiple of the storage unit, then error
		 out and reset the size.  */
	      if (!integer_zerop (size_binop (TRUNC_MOD_EXPR, gnu_size,
					      bitsize_unit_node)))
		{
		  char s[128];
		  snprintf (s, sizeof (s), "size for %s must be "
			    "multiple of Storage_Unit", field_s);
		  post_error_ne (s, Last_Bit (gnat_clause), gnat_field);
		  gnu_size = NULL_TREE;
		}

	      /* If the size is lower than that of the type, or greater for
		 atomic and aliased, then error out and reset the size.  */
	      else if ((cmp = tree_int_cst_compare (gnu_size, type_size)) < 0
		       || (cmp > 0 && (is_aliased || is_full_access)))
		{
		  char s[128];
		  snprintf (s, sizeof (s), size_s, field_s);
		  post_error_ne_tree (s, Last_Bit (gnat_clause), gnat_field,
				      type_size);
		  gnu_size = NULL_TREE;
		}
	    }
	}
    }

  else
    {
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
	 layout, if it has the same storage order as the enclosing record
	 and if the prescribed size is not greater than that of the packed
	 array to preserve the justification.  */
      if (!needs_strict_alignment
	  && TREE_CODE (gnu_field_type) == RECORD_TYPE
	  && TYPE_JUSTIFIED_MODULAR_P (gnu_field_type)
	  && TYPE_REVERSE_STORAGE_ORDER (gnu_field_type)
	     == Reverse_Storage_Order (gnat_record_type)
	  && tree_int_cst_compare (gnu_size, TYPE_ADA_SIZE (gnu_field_type))
	       <= 0)
	gnu_field_type = TREE_TYPE (TYPE_FIELDS (gnu_field_type));

      /* Similarly if the field's type is a misaligned integral type, but
	 there is no restriction on the size as there is no justification.  */
      if (!needs_strict_alignment
	  && TYPE_IS_PADDING_P (gnu_field_type)
	  && INTEGRAL_TYPE_P (TREE_TYPE (TYPE_FIELDS (gnu_field_type))))
	gnu_field_type = TREE_TYPE (TYPE_FIELDS (gnu_field_type));

      orig_field_type = gnu_field_type;
      gnu_field_type
	= make_type_from_size (gnu_field_type, gnu_size,
			       Has_Biased_Representation (gnat_field));

      /* If the type has been extended, we may need to cap the alignment.  */
      if (!needs_strict_alignment
	  && gnu_field_type != orig_field_type
	  && tree_int_cst_lt (TYPE_SIZE (orig_field_type), gnu_size))
	packed = adjust_packed (gnu_field_type, gnu_record_type, packed);

      orig_field_type = gnu_field_type;
      gnu_field_type = maybe_pad_type (gnu_field_type, gnu_size, 0, gnat_field,
				       false, definition, true);

      /* If a padding record was made, declare it now since it will never be
	 declared otherwise.  This is necessary to ensure that its subtrees
	 are properly marked.  */
      if (gnu_field_type != orig_field_type
	  && !DECL_P (TYPE_NAME (gnu_field_type)))
	create_type_decl (TYPE_NAME (gnu_field_type), gnu_field_type, true,
			  debug_info_p, gnat_field);
    }

  /* Otherwise (or if there was an error), don't specify a position.  */
  else
    gnu_pos = NULL_TREE;

  /* If the field's type is a padded type made for a scalar field of a record
     type with reverse storage order, we need to propagate the reverse storage
     order to the padding type since it is the innermost enclosing aggregate
     type around the scalar.  */
  if (TYPE_IS_PADDING_P (gnu_field_type)
      && TYPE_REVERSE_STORAGE_ORDER (gnu_record_type)
      && Is_Scalar_Type (gnat_field_type))
    gnu_field_type = set_reverse_storage_order_on_pad_type (gnu_field_type);

  gcc_assert (TREE_CODE (gnu_field_type) != RECORD_TYPE
	      || !TYPE_CONTAINS_TEMPLATE_P (gnu_field_type));

  /* Now create the decl for the field.  */
  gnu_field
    = create_field_decl (gnu_field_id, gnu_field_type, gnu_record_type,
			 gnu_size, gnu_pos, packed, is_aliased);
  Sloc_to_locus (Sloc (gnat_field), &DECL_SOURCE_LOCATION (gnu_field));
  DECL_ALIASED_P (gnu_field) = is_aliased;
  TREE_SIDE_EFFECTS (gnu_field) = TREE_THIS_VOLATILE (gnu_field) = is_volatile;

  /* If this is a discriminant, then we treat it specially: first, we set its
     index number for the back-annotation; second, we record whether it cannot
     be changed once it has been set for the computation of loop invariants;
     third, we make it addressable in order for the optimizer to more easily
     see that it cannot be modified by assignments to the other fields of the
     record (see create_field_decl for a more detailed explanation), which is
     crucial to hoist the offset and size computations of dynamic fields.  */
  if (Ekind (gnat_field) == E_Discriminant)
    {
      DECL_DISCRIMINANT_NUMBER (gnu_field)
	= UI_To_gnu (Discriminant_Number (gnat_field), sizetype);
      DECL_INVARIANT_P (gnu_field)
	= No (Discriminant_Default_Value (gnat_field));
      DECL_NONADDRESSABLE_P (gnu_field) = 0;
    }

  return gnu_field;
}

/* Return true if at least one member of COMPONENT_LIST needs strict
   alignment.  */

static bool
components_need_strict_alignment (Node_Id component_list)
{
  Node_Id component_decl;

  for (component_decl = First_Non_Pragma (Component_Items (component_list));
       Present (component_decl);
       component_decl = Next_Non_Pragma (component_decl))
    {
      Entity_Id gnat_field = Defining_Entity (component_decl);

      if (Is_Independent (gnat_field) || Is_Independent (Etype (gnat_field)))
	return true;

      if (Strict_Alignment (Etype (gnat_field)))
	return true;
    }

  return false;
}

/* Return true if FIELD is an artificial field.  */

static bool
field_is_artificial (tree field)
{
  /* These fields are generated by the front-end proper.  */
  if (IDENTIFIER_POINTER (DECL_NAME (field)) [0] == '_')
    return true;

  /* These fields are generated by gigi.  */
  if (DECL_INTERNAL_P (field))
    return true;

  return false;
}

/* Return true if FIELD is a non-artificial field with self-referential
   size.  */

static bool
field_has_self_size (tree field)
{
  if (field_is_artificial (field))
    return false;

  if (DECL_SIZE (field) && TREE_CODE (DECL_SIZE (field)) == INTEGER_CST)
    return false;

  return CONTAINS_PLACEHOLDER_P (TYPE_SIZE (TREE_TYPE (field)));
}

/* Return true if FIELD is a non-artificial field with variable size.  */

static bool
field_has_variable_size (tree field)
{
  if (field_is_artificial (field))
    return false;

  if (DECL_SIZE (field) && TREE_CODE (DECL_SIZE (field)) == INTEGER_CST)
    return false;

  return TREE_CODE (TYPE_SIZE (TREE_TYPE (field))) != INTEGER_CST;
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

/* Sort the LIST of fields in reverse order of increasing position.  */

static tree
reverse_sort_field_list (tree list)
{
  const int len = list_length (list);
  tree *field_arr = XALLOCAVEC (tree, len);

  for (int i = 0; list; list = DECL_CHAIN (list), i++)
    field_arr[i] = list;

  qsort (field_arr, len, sizeof (tree), compare_field_bitpos);

  for (int i = 0; i < len; i++)
    {
      DECL_CHAIN (field_arr[i]) = list;
      list = field_arr[i];
    }

  return list;
}

/* Reverse function from gnat_to_gnu_field: return the GNAT field present in
   either GNAT_COMPONENT_LIST or the discriminants of GNAT_RECORD_TYPE, and
   corresponding to the GNU tree GNU_FIELD.  */

static Entity_Id
gnu_field_to_gnat (tree gnu_field, Node_Id gnat_component_list,
		   Entity_Id gnat_record_type)
{
  Entity_Id gnat_component_decl, gnat_field;

  if (Present (Component_Items (gnat_component_list)))
    for (gnat_component_decl
	   = First_Non_Pragma (Component_Items (gnat_component_list));
	 Present (gnat_component_decl);
	 gnat_component_decl = Next_Non_Pragma (gnat_component_decl))
      {
	gnat_field = Defining_Entity (gnat_component_decl);
	if (gnat_to_gnu_field_decl (gnat_field) == gnu_field)
	  return gnat_field;
      }

  if (Has_Discriminants (gnat_record_type))
    for (gnat_field = First_Stored_Discriminant (gnat_record_type);
	 Present (gnat_field);
	 gnat_field = Next_Stored_Discriminant (gnat_field))
      if (gnat_to_gnu_field_decl (gnat_field) == gnu_field)
	return gnat_field;

  return Empty;
}

/* Issue a warning for the problematic placement of GNU_FIELD present in
   either GNAT_COMPONENT_LIST or the discriminants of GNAT_RECORD_TYPE.
   IN_VARIANT is true if GNAT_COMPONENT_LIST is the list of a variant.
   DO_REORDER is true if fields of GNAT_RECORD_TYPE are being reordered.  */

static void
warn_on_field_placement (tree gnu_field, Node_Id gnat_component_list,
			 Entity_Id gnat_record_type, bool in_variant,
			 bool do_reorder)
{
  if (!Comes_From_Source (gnat_record_type))
    return;

  Entity_Id gnat_field
    = gnu_field_to_gnat (gnu_field, gnat_component_list, gnat_record_type);
  gcc_assert (Present (gnat_field));

  const char *msg1
    = in_variant
      ? "??variant layout may cause performance issues"
      : "??record layout may cause performance issues";
  const char *msg2
    = Ekind (gnat_field) == E_Discriminant
      ? "??discriminant & whose length is not multiple of a byte"
      : field_has_self_size (gnu_field)
	? "??component & whose length depends on a discriminant"
	: field_has_variable_size (gnu_field)
	  ? "??component & whose length is not fixed"
	  : "??component & whose length is not multiple of a byte";
  const char *msg3
    = do_reorder
      ? "??comes too early and was moved down"
      : "??comes too early and ought to be moved down";

  post_error (msg1, gnat_field);
  post_error_ne (msg2, gnat_field, gnat_field);
  post_error (msg3, gnat_field);
}

/* Likewise but for every field present on GNU_FIELD_LIST.  */

static void
warn_on_list_placement (tree gnu_field_list, Node_Id gnat_component_list,
		        Entity_Id gnat_record_type, bool in_variant,
		        bool do_reorder)
{
  for (tree gnu_tmp = gnu_field_list; gnu_tmp; gnu_tmp = DECL_CHAIN (gnu_tmp))
    warn_on_field_placement (gnu_tmp, gnat_component_list, gnat_record_type,
			     in_variant, do_reorder);
}

/* Structure holding information for a given variant.  */
typedef struct vinfo
{
  /* The record type of the variant.  */
  tree type;

  /* The name of the variant.  */
  tree name;

  /* The qualifier of the variant.  */
  tree qual;

  /* Whether the variant has a rep clause.  */
  bool has_rep;

  /* Whether the variant is packed.  */
  bool packed;

} vinfo_t;

/* Translate and chain GNAT_COMPONENT_LIST present in GNAT_RECORD_TYPE to
   GNU_FIELD_LIST, set the result as the field list of GNU_RECORD_TYPE and
   finish it up.  Return true if GNU_RECORD_TYPE has a rep clause that affects
   the layout (see below).  When called from gnat_to_gnu_entity during the
   processing of a record definition, the GCC node for the parent, if any,
   will be the single field of GNU_RECORD_TYPE and the GCC nodes for the
   discriminants will be on GNU_FIELD_LIST.  The other call to this function
   is a recursive call for the component list of a variant and, in this case,
   GNU_FIELD_LIST is empty.  Note that GNAT_COMPONENT_LIST may be Empty.

   PACKED is 1 if this is for a packed record or -1 if this is for a record
   with Component_Alignment of Storage_Unit.

   DEFINITION is true if we are defining this record type.

   CANCEL_ALIGNMENT is true if the alignment should be zeroed before laying
   out the record.  This means the alignment only serves to force fields to
   be bitfields, but not to require the record to be that aligned.  This is
   used for variants.

   ALL_REP is true if a rep clause is present for all the fields.

   UNCHECKED_UNION is true if we are building this type for a record with a
   Pragma Unchecked_Union.

   ARTIFICIAL is true if this is a type that was generated by the compiler.

   DEBUG_INFO is true if we need to write debug information about the type.

   MAYBE_UNUSED is true if this type may be unused in the end; this doesn't
   mean that its contents may be unused as well, only the container itself.

   FIRST_FREE_POS, if nonzero, is the first (lowest) free field position in
   the outer record type down to this variant level.  It is nonzero only if
   all the fields down to this level have a rep clause and ALL_REP is false.

   P_GNU_REP_LIST, if nonzero, is a pointer to a list to which each field
   with a rep clause is to be added; in this case, that is all that should
   be done with such fields and the return value will be false.  */

static bool
components_to_record (Node_Id gnat_component_list, Entity_Id gnat_record_type,
		      tree gnu_field_list, tree gnu_record_type, int packed,
		      bool definition, bool cancel_alignment, bool all_rep,
		      bool unchecked_union, bool artificial, bool debug_info,
		      bool maybe_unused, tree first_free_pos,
		      tree *p_gnu_rep_list)
{
  const bool needs_xv_encodings
    = debug_info && gnat_encodings == DWARF_GNAT_ENCODINGS_ALL;
  bool all_rep_and_size = all_rep && TYPE_SIZE (gnu_record_type);
  bool variants_have_rep = all_rep;
  bool layout_with_rep = false;
  bool has_non_packed_fixed_size_field = false;
  bool has_self_field = false;
  bool has_aliased_after_self_field = false;
  Entity_Id gnat_component_decl, gnat_variant_part;
  tree gnu_field, gnu_next, gnu_last;
  tree gnu_variant_part = NULL_TREE;
  tree gnu_rep_list = NULL_TREE;

  /* For each component referenced in a component declaration create a GCC
     field and add it to the list, skipping pragmas in the GNAT list.  */
  gnu_last = tree_last (gnu_field_list);
  if (Present (gnat_component_list)
      && (Present (Component_Items (gnat_component_list))))
    for (gnat_component_decl
	   = First_Non_Pragma (Component_Items (gnat_component_list));
	 Present (gnat_component_decl);
	 gnat_component_decl = Next_Non_Pragma (gnat_component_decl))
      {
	Entity_Id gnat_field = Defining_Entity (gnat_component_decl);
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

		/* And record information for the final layout.  */
		if (field_has_self_size (gnu_field))
		  has_self_field = true;
		else if (has_self_field && DECL_ALIASED_P (gnu_field))
		  has_aliased_after_self_field = true;
		else if (!DECL_FIELD_OFFSET (gnu_field)
			 && !DECL_PACKED (gnu_field)
			 && !field_has_variable_size (gnu_field))
		  has_non_packed_fixed_size_field = true;
	      }
	  }

	save_gnu_tree (gnat_field, gnu_field, false);
      }

  /* At the end of the component list there may be a variant part.  */
  if (Present (gnat_component_list))
    gnat_variant_part = Variant_Part (gnat_component_list);
  else
    gnat_variant_part = Empty;

  /* We create a QUAL_UNION_TYPE for the variant part since the variants are
     mutually exclusive and should go in the same memory.  To do this we need
     to treat each variant as a record whose elements are created from the
     component list for the variant.  So here we create the records from the
     lists for the variants and put them all into the QUAL_UNION_TYPE.
     If this is an Unchecked_Union, we make a UNION_TYPE instead or
     use GNU_RECORD_TYPE if there are no fields so far.  */
  if (Present (gnat_variant_part))
    {
      Node_Id gnat_discr = Name (gnat_variant_part), variant;
      tree gnu_discr = gnat_to_gnu (gnat_discr);
      tree gnu_name = TYPE_IDENTIFIER (gnu_record_type);
      tree gnu_var_name
	= concat_name (get_identifier (Get_Name_String (Chars (gnat_discr))),
		       "XVN");
      tree gnu_union_name
	= concat_name (gnu_name, IDENTIFIER_POINTER (gnu_var_name));
      tree gnu_union_type;
      tree this_first_free_pos, gnu_variant_list = NULL_TREE;
      bool union_field_needs_strict_alignment = false;
      auto_vec <vinfo_t, 16> variant_types;
      vinfo_t *gnu_variant;
      unsigned int variants_align = 0;
      unsigned int i;

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
	  SET_TYPE_ALIGN (gnu_union_type, 0);
	  TYPE_PACKED (gnu_union_type) = TYPE_PACKED (gnu_record_type);
	  TYPE_REVERSE_STORAGE_ORDER (gnu_union_type)
	    = TYPE_REVERSE_STORAGE_ORDER (gnu_record_type);
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

      /* We build the variants in two passes.  The bulk of the work is done in
	 the first pass, that is to say translating the GNAT nodes, building
	 the container types and computing the associated properties.  However
	 we cannot finish up the container types during this pass because we
	 don't know where the variant part will be placed until the end.  */
      for (variant = First_Non_Pragma (Variants (gnat_variant_part));
	   Present (variant);
	   variant = Next_Non_Pragma (variant))
	{
	  tree gnu_variant_type = make_node (RECORD_TYPE);
	  tree gnu_inner_name, gnu_qual;
	  bool has_rep;
	  int field_packed;
	  vinfo_t vinfo;

	  Get_Variant_Encoding (variant);
	  gnu_inner_name = get_identifier_with_length (Name_Buffer, Name_Len);
	  TYPE_NAME (gnu_variant_type)
	    = concat_name (gnu_union_name,
			   IDENTIFIER_POINTER (gnu_inner_name));

	  /* Set the alignment of the inner type in case we need to make
	     inner objects into bitfields, but then clear it out so the
	     record actually gets only the alignment required.  */
	  SET_TYPE_ALIGN (gnu_variant_type, TYPE_ALIGN (gnu_record_type));
	  TYPE_PACKED (gnu_variant_type) = TYPE_PACKED (gnu_record_type);
	  TYPE_REVERSE_STORAGE_ORDER (gnu_variant_type)
	    = TYPE_REVERSE_STORAGE_ORDER (gnu_record_type);

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
	  has_rep
	    = components_to_record (Component_List (variant), gnat_record_type,
				    NULL_TREE, gnu_variant_type, packed,
				    definition, !all_rep_and_size, all_rep,
				    unchecked_union, true, needs_xv_encodings,
				    true, this_first_free_pos,
				    all_rep || this_first_free_pos
				    ? NULL : &gnu_rep_list);

	  /* Translate the qualifier and annotate the GNAT node.  */
	  gnu_qual = choices_to_gnu (gnu_discr, Discrete_Choices (variant));
	  Set_Present_Expr (variant, annotate_value (gnu_qual));

	  /* Deal with packedness like in gnat_to_gnu_field.  */
	  if (components_need_strict_alignment (Component_List (variant)))
	    {
	      field_packed = 0;
	      union_field_needs_strict_alignment = true;
	    }
	  else
	    field_packed
	      = adjust_packed (gnu_variant_type, gnu_record_type, packed);

	  /* Push this variant onto the stack for the second pass.  */
	  vinfo.type = gnu_variant_type;
	  vinfo.name = gnu_inner_name;
	  vinfo.qual = gnu_qual;
	  vinfo.has_rep = has_rep;
	  vinfo.packed = field_packed;
	  variant_types.safe_push (vinfo);

	  /* Compute the global properties that will determine the placement of
	     the variant part.  */
	  variants_have_rep |= has_rep;
	  if (!field_packed && TYPE_ALIGN (gnu_variant_type) > variants_align)
	    variants_align = TYPE_ALIGN (gnu_variant_type);
	}

      /* Round up the first free position to the alignment of the variant part
	 for the variants without rep clause.  This will guarantee a consistent
	 layout independently of the placement of the variant part.  */
      if (variants_have_rep && variants_align > 0 && this_first_free_pos)
	this_first_free_pos = round_up (this_first_free_pos, variants_align);

      /* In the second pass, the container types are adjusted if necessary and
	 finished up, then the corresponding fields of the variant part are
	 built with their qualifier, unless this is an unchecked union.  */
      FOR_EACH_VEC_ELT (variant_types, i, gnu_variant)
	{
	  tree gnu_variant_type = gnu_variant->type;
	  tree gnu_field_list = TYPE_FIELDS (gnu_variant_type);

	  /* If this is an Unchecked_Union whose fields are all in the variant
	     part and we have a single field with no representation clause or
	     placed at offset zero, use the field directly to match the layout
	     of C unions.  */
	  if (TREE_CODE (gnu_record_type) == UNION_TYPE
	      && gnu_field_list
	      && !DECL_CHAIN (gnu_field_list)
	      && (!DECL_FIELD_OFFSET (gnu_field_list)
		  || integer_zerop (bit_position (gnu_field_list))))
	    {
	      gnu_field = gnu_field_list;
	      DECL_CONTEXT (gnu_field) = gnu_record_type;
	    }
	  else
	    {
	      /* Finalize the variant type now.  We used to throw away empty
		 record types but we no longer do that because we need them to
		 generate complete debug info for the variant; otherwise, the
		 union type definition will be lacking the fields associated
		 with these empty variants.  */
	      if (gnu_field_list && variants_have_rep && !gnu_variant->has_rep)
		{
		  /* The variant part will be at offset 0 so we need to ensure
		     that the fields are laid out starting from the first free
		     position at this level.  */
		  tree gnu_rep_type = make_node (RECORD_TYPE);
		  tree gnu_rep_part;
		  TYPE_REVERSE_STORAGE_ORDER (gnu_rep_type)
		    = TYPE_REVERSE_STORAGE_ORDER (gnu_variant_type);
		  finish_record_type (gnu_rep_type, NULL_TREE, 0, debug_info);
		  gnu_rep_part
		    = create_rep_part (gnu_rep_type, gnu_variant_type,
				       this_first_free_pos);
		  DECL_CHAIN (gnu_rep_part) = gnu_field_list;
		  gnu_field_list = gnu_rep_part;
		  finish_record_type (gnu_variant_type, gnu_field_list, 0,
				      false);
		}

	      if (debug_info)
		rest_of_record_type_compilation (gnu_variant_type);
	      create_type_decl (TYPE_NAME (gnu_variant_type), gnu_variant_type,
				true, needs_xv_encodings, gnat_component_list);

	      gnu_field
		= create_field_decl (gnu_variant->name, gnu_variant_type,
				     gnu_union_type,
				     all_rep_and_size
				     ? TYPE_SIZE (gnu_variant_type) : 0,
				     variants_have_rep ? bitsize_zero_node : 0,
				     gnu_variant->packed, 0);

	      DECL_INTERNAL_P (gnu_field) = 1;

	      if (!unchecked_union)
		DECL_QUALIFIER (gnu_field) = gnu_variant->qual;
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
			      all_rep_and_size ? 1 : 0, needs_xv_encodings);

	  /* If GNU_UNION_TYPE is our record type, it means we must have an
	     Unchecked_Union with no fields.  Verify that and, if so, just
	     return.  */
	  if (gnu_union_type == gnu_record_type)
	    {
	      gcc_assert (unchecked_union
			  && !gnu_field_list
			  && !gnu_rep_list);
	      return variants_have_rep;
	    }

	  create_type_decl (TYPE_NAME (gnu_union_type), gnu_union_type, true,
			    needs_xv_encodings, gnat_component_list);

	  /* Deal with packedness like in gnat_to_gnu_field.  */
	  if (union_field_needs_strict_alignment)
	    union_field_packed = 0;
	  else
	    union_field_packed
	      = adjust_packed (gnu_union_type, gnu_record_type, packed);

	  gnu_variant_part
	    = create_field_decl (gnu_var_name, gnu_union_type, gnu_record_type,
				 all_rep_and_size
				 ? TYPE_SIZE (gnu_union_type) : 0,
				 variants_have_rep ? bitsize_zero_node : 0,
				 union_field_packed, 0);

	  DECL_INTERNAL_P (gnu_variant_part) = 1;
	}
    }

  /* Scan GNU_FIELD_LIST and see if any fields have rep clauses.  If they do,
     pull them out and put them onto the appropriate list.

     Similarly, pull out the fields with zero size and no rep clause, as they
     would otherwise modify the layout and thus very likely run afoul of the
     Ada semantics, which are different from those of C here.

     Finally, if there is an aliased field placed in the list after fields
     with self-referential size, pull out the latter in the same way.

     Optionally, if the reordering mechanism is enabled, pull out the fields
     with self-referential size, variable size and fixed size not a multiple
     of a byte, so that they don't cause the regular fields to be either at
     self-referential/variable offset or misaligned.  Note, in the latter
     case, that this can only happen in packed record types so the alignment
     is effectively capped to the byte for the whole record.  But we don't
     do it for packed record types if not all fixed-size fiels can be packed
     and for non-packed record types if pragma Optimize_Alignment (Space) is
     specified, because this can prevent alignment gaps from being filled.

     Optionally, if the layout warning is enabled, keep track of the above 4
     different kinds of fields and issue a warning if some of them would be
     (or are being) reordered by the reordering mechanism.

     ??? If we reorder fields, the debugging information will be affected and
     the debugger print fields in a different order from the source code.  */
  const bool do_reorder
    = (Convention (gnat_record_type) == Convention_Ada
       && !No_Reordering (gnat_record_type)
       && !(Is_Packed (gnat_record_type)
	    ? has_non_packed_fixed_size_field
	    : Optimize_Alignment_Space (gnat_record_type))
       && !Debug_Flag_Dot_R);
  const bool w_reorder
    = (Convention (gnat_record_type) == Convention_Ada
       && Warn_On_Questionable_Layout
       && !(No_Reordering (gnat_record_type) && GNAT_Mode));
  const bool in_variant = (p_gnu_rep_list != NULL);
  tree gnu_zero_list = NULL_TREE;
  tree gnu_self_list = NULL_TREE;
  tree gnu_var_list = NULL_TREE;
  tree gnu_bitp_list = NULL_TREE;
  tree gnu_tmp_bitp_list = NULL_TREE;
  unsigned int tmp_bitp_size = 0;
  unsigned int last_reorder_field_type = -1;
  unsigned int tmp_last_reorder_field_type = -1;

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

  gnu_last = NULL_TREE;
  for (gnu_field = gnu_field_list; gnu_field; gnu_field = gnu_next)
    {
      gnu_next = DECL_CHAIN (gnu_field);

      if (DECL_FIELD_OFFSET (gnu_field))
	{
	  MOVE_FROM_FIELD_LIST_TO (gnu_rep_list);
	  continue;
	}

      if (DECL_SIZE (gnu_field) && integer_zerop (DECL_SIZE (gnu_field)))
	{
	  DECL_SIZE_UNIT (gnu_field) = size_zero_node;
	  DECL_FIELD_OFFSET (gnu_field) = size_zero_node;
	  SET_DECL_OFFSET_ALIGN (gnu_field, BIGGEST_ALIGNMENT);
	  DECL_FIELD_BIT_OFFSET (gnu_field) = bitsize_zero_node;
	  if (DECL_ALIASED_P (gnu_field))
	    SET_TYPE_ALIGN (gnu_record_type,
			    MAX (TYPE_ALIGN (gnu_record_type),
				 TYPE_ALIGN (TREE_TYPE (gnu_field))));
	  MOVE_FROM_FIELD_LIST_TO (gnu_zero_list);
	  continue;
	}

      if (has_aliased_after_self_field && field_has_self_size (gnu_field))
	{
	  MOVE_FROM_FIELD_LIST_TO (gnu_self_list);
	  continue;
	}

      /* We don't need further processing in default mode.  */
      if (!w_reorder && !do_reorder)
	{
	  gnu_last = gnu_field;
	  continue;
	}

      if (field_has_self_size (gnu_field))
	{
	  if (w_reorder)
	    {
	      if (last_reorder_field_type < 4)
		warn_on_field_placement (gnu_field, gnat_component_list,
					 gnat_record_type, in_variant,
					 do_reorder);
	      else
		last_reorder_field_type = 4;
	    }

	  if (do_reorder)
	    {
	      MOVE_FROM_FIELD_LIST_TO (gnu_self_list);
	      continue;
	    }
	}

      else if (field_has_variable_size (gnu_field))
	{
	  if (w_reorder)
	    {
	      if (last_reorder_field_type < 3)
		warn_on_field_placement (gnu_field, gnat_component_list,
					 gnat_record_type, in_variant,
					 do_reorder);
	      else
		last_reorder_field_type = 3;
	    }

	  if (do_reorder)
	    {
	      MOVE_FROM_FIELD_LIST_TO (gnu_var_list);
	      continue;
	    }
	}

      else
	{
	  /* If the field has no size, then it cannot be bit-packed.  */
	  const unsigned int bitp_size
	    = DECL_SIZE (gnu_field)
	      ? TREE_INT_CST_LOW (DECL_SIZE (gnu_field)) % BITS_PER_UNIT
	      : 0;

	  /* If the field is bit-packed, we move it to a temporary list that
	     contains the contiguously preceding bit-packed fields, because
	     we want to be able to put them back if the misalignment happens
	     to cancel itself after several bit-packed fields.  */
	  if (bitp_size != 0)
	    {
	      tmp_bitp_size = (tmp_bitp_size + bitp_size) % BITS_PER_UNIT;

	      if (last_reorder_field_type != 2)
		{
		  tmp_last_reorder_field_type = last_reorder_field_type;
		  last_reorder_field_type = 2;
		}

	      if (do_reorder)
		{
		  MOVE_FROM_FIELD_LIST_TO (gnu_tmp_bitp_list);
		  continue;
		}
	    }

	  /* No more bit-packed fields, move the existing ones to the end or
	     put them back at their original location.  */
	  else if (last_reorder_field_type == 2 || gnu_tmp_bitp_list)
	    {
	      last_reorder_field_type = 1;

	      if (tmp_bitp_size != 0)
		{
		  if (w_reorder && tmp_last_reorder_field_type < 2)
		    {
		      if (gnu_tmp_bitp_list)
			warn_on_list_placement (gnu_tmp_bitp_list,
						gnat_component_list,
						gnat_record_type, in_variant,
						do_reorder);
		      else
			warn_on_field_placement (gnu_last,
						 gnat_component_list,
						 gnat_record_type, in_variant,
						 do_reorder);
		    }

		  if (do_reorder)
		    gnu_bitp_list = chainon (gnu_tmp_bitp_list, gnu_bitp_list);

		  gnu_tmp_bitp_list = NULL_TREE;
		  tmp_bitp_size = 0;
		}
	      else
		{
		  /* Rechain the temporary list in front of GNU_FIELD.  */
		  tree gnu_bitp_field = gnu_field;
		  while (gnu_tmp_bitp_list)
		    {
		      tree gnu_bitp_next = DECL_CHAIN (gnu_tmp_bitp_list);
		      DECL_CHAIN (gnu_tmp_bitp_list) = gnu_bitp_field;
		      if (gnu_last)
			DECL_CHAIN (gnu_last) = gnu_tmp_bitp_list;
		      else
			gnu_field_list = gnu_tmp_bitp_list;
		      gnu_bitp_field = gnu_tmp_bitp_list;
		      gnu_tmp_bitp_list = gnu_bitp_next;
		    }
		}
	    }

	  else
	    last_reorder_field_type = 1;
	}

      gnu_last = gnu_field;
    }

#undef MOVE_FROM_FIELD_LIST_TO

  gnu_field_list = nreverse (gnu_field_list);

  /* If permitted, we reorder the fields as follows:

      1) all (groups of) fields whose length is fixed and multiple of a byte,
      2) the remaining fields whose length is fixed and not multiple of a byte,
      3) the remaining fields whose length doesn't depend on discriminants,
      4) all fields whose length depends on discriminants,
      5) the variant part,

     within the record and within each variant recursively.  */

  if (w_reorder)
    {
      /* If we have pending bit-packed fields, warn if they would be moved
	 to after regular fields.  */
      if (last_reorder_field_type == 2
	  && tmp_bitp_size != 0
	  && tmp_last_reorder_field_type < 2)
	{
	  if (gnu_tmp_bitp_list)
	    warn_on_list_placement (gnu_tmp_bitp_list,
				    gnat_component_list, gnat_record_type,
				    in_variant, do_reorder);
	  else
	    warn_on_field_placement (gnu_field_list,
				     gnat_component_list, gnat_record_type,
				     in_variant, do_reorder);
	}
    }

  if (do_reorder)
    {
      /* If we have pending bit-packed fields on the temporary list, we put
	 them either on the bit-packed list or back on the regular list.  */
      if (gnu_tmp_bitp_list)
	{
	  if (tmp_bitp_size != 0)
	    gnu_bitp_list = chainon (gnu_tmp_bitp_list, gnu_bitp_list);
	  else
	    gnu_field_list = chainon (gnu_tmp_bitp_list, gnu_field_list);
	}

      gnu_field_list
	= chainon (gnu_field_list,
		   chainon (gnu_bitp_list,
			    chainon (gnu_var_list, gnu_self_list)));
    }

  /* Otherwise, if there is an aliased field placed after a field whose length
     depends on discriminants, we put all the fields of the latter sort, last.
     We need to do this in case an object of this record type is mutable.  */
  else if (has_aliased_after_self_field)
    gnu_field_list = chainon (gnu_field_list, gnu_self_list);

  /* If P_REP_LIST is nonzero, this means that we are asked to move the fields
     in our REP list to the previous level because this level needs them in
     order to do a correct layout, i.e. avoid having overlapping fields.  */
  if (p_gnu_rep_list && gnu_rep_list)
    *p_gnu_rep_list = chainon (*p_gnu_rep_list, gnu_rep_list);

  /* Deal with the case of an extension of a record type with variable size and
     partial rep clause, for which the _Parent field is forced at offset 0 and
     has variable size.  Note that we cannot do it if the field has fixed size
     because we rely on the presence of the REP part built below to trigger the
     reordering of the fields in a derived record type when all the fields have
     a fixed position.  */
  else if (gnu_rep_list
	   && !DECL_CHAIN (gnu_rep_list)
	   && TREE_CODE (DECL_SIZE (gnu_rep_list)) != INTEGER_CST
	   && !variants_have_rep
	   && first_free_pos
	   && integer_zerop (first_free_pos)
	   && integer_zerop (bit_position (gnu_rep_list)))
    {
      DECL_CHAIN (gnu_rep_list) = gnu_field_list;
      gnu_field_list = gnu_rep_list;
      gnu_rep_list = NULL_TREE;
    }

  /* Otherwise, sort the fields by bit position and put them into their own
     record, before the others, if we also have fields without rep clause.  */
  else if (gnu_rep_list)
    {
      tree gnu_parent, gnu_rep_type;

      /* If all the fields have a rep clause, we can do a flat layout.  */
      layout_with_rep = !gnu_field_list
			&& (!gnu_variant_part || variants_have_rep);

      /* Same as above but the extension itself has a rep clause, in which case
	 we need to set aside the _Parent field to lay out the REP part.  */
      if (TREE_CODE (DECL_SIZE (gnu_rep_list)) != INTEGER_CST
	  && !layout_with_rep
	  && !variants_have_rep
	  && first_free_pos
	  && integer_zerop (first_free_pos)
	  && integer_zerop (bit_position (gnu_rep_list)))
	{
	  gnu_parent = gnu_rep_list;
	  gnu_rep_list = DECL_CHAIN (gnu_rep_list);
	}
      else
	gnu_parent = NULL_TREE;

      gnu_rep_type
	= layout_with_rep ? gnu_record_type : make_node (RECORD_TYPE);

      /* Sort the fields in order of increasing bit position.  */
      const int len = list_length (gnu_rep_list);
      tree *gnu_arr = XALLOCAVEC (tree, len);

      gnu_field = gnu_rep_list;
      for (int i = 0; i < len; i++)
	{
	  gnu_arr[i] = gnu_field;
	  gnu_field = DECL_CHAIN (gnu_field);
	}

      qsort (gnu_arr, len, sizeof (tree), compare_field_bitpos);

      gnu_rep_list = NULL_TREE;
      for (int i = len - 1; i >= 0; i--)
	{
	  DECL_CHAIN (gnu_arr[i]) = gnu_rep_list;
	  gnu_rep_list = gnu_arr[i];
	  DECL_CONTEXT (gnu_arr[i]) = gnu_rep_type;
	}

      /* Do the layout of the REP part, if any.  */
      if (layout_with_rep)
	gnu_field_list = gnu_rep_list;
      else
	{
	  TYPE_NAME (gnu_rep_type)
	    = create_concat_name (gnat_record_type, "REP");
	  TYPE_REVERSE_STORAGE_ORDER (gnu_rep_type)
	    = TYPE_REVERSE_STORAGE_ORDER (gnu_record_type);
	  finish_record_type (gnu_rep_type, gnu_rep_list, 1, false);

	  /* If FIRST_FREE_POS is nonzero, we need to ensure that the fields
	     without rep clause are laid out starting from this position.
	     Therefore, we force it as a minimal size on the REP part.  */
	  tree gnu_rep_part
	    = create_rep_part (gnu_rep_type, gnu_record_type, first_free_pos);

	  /* If this is an extension, put back the _Parent field as the first
	     field of the REP part at offset 0 and update its layout.  */
	  if (gnu_parent)
	    {
	      const unsigned int align = DECL_ALIGN (gnu_parent);
	      DECL_CHAIN (gnu_parent) = TYPE_FIELDS (gnu_rep_type);
	      TYPE_FIELDS (gnu_rep_type) = gnu_parent;
	      DECL_CONTEXT (gnu_parent) = gnu_rep_type;
	      if (align > TYPE_ALIGN (gnu_rep_type))
		{
		  SET_TYPE_ALIGN (gnu_rep_type, align);
		  TYPE_SIZE (gnu_rep_type)
		    = round_up (TYPE_SIZE (gnu_rep_type), align);
		  TYPE_SIZE_UNIT (gnu_rep_type)
		    = round_up (TYPE_SIZE_UNIT (gnu_rep_type), align);
		  SET_DECL_ALIGN (gnu_rep_part, align);
		}
	    }

	  if (debug_info)
	    rest_of_record_type_compilation (gnu_rep_type);

	  /* Chain the REP part at the beginning of the field list.  */
	  DECL_CHAIN (gnu_rep_part) = gnu_field_list;
	  gnu_field_list = gnu_rep_part;
	}
    }

  /* Chain the variant part at the end of the field list.  */
  if (gnu_variant_part)
    gnu_field_list = chainon (gnu_field_list, gnu_variant_part);

  if (cancel_alignment)
    SET_TYPE_ALIGN (gnu_record_type, 0);

  TYPE_ARTIFICIAL (gnu_record_type) = artificial;

  finish_record_type (gnu_record_type, gnu_field_list, layout_with_rep ? 1 : 0,
		      debug_info && !maybe_unused);

  /* Chain the fields with zero size at the beginning of the field list.  */
  if (gnu_zero_list)
    TYPE_FIELDS (gnu_record_type)
      = chainon (gnu_zero_list, TYPE_FIELDS (gnu_record_type));

  return (gnu_rep_list && !p_gnu_rep_list) || variants_have_rep;
}

/* Given GNU_SIZE, a GCC tree representing a size, return a Uint to be
   placed into an Esize, Component_Bit_Offset, or Component_Size value
   in the GNAT tree.  */

static Uint
annotate_value (tree gnu_size)
{
  static int var_count = 0;
  TCode tcode;
  Node_Ref_Or_Val ops[3] = { No_Uint, No_Uint, No_Uint };
  struct tree_int_map in;

  /* See if we've already saved the value for this node.  */
  if (EXPR_P (gnu_size) || DECL_P (gnu_size))
    {
      struct tree_int_map *e;

      in.base.from = gnu_size;
      e = annotate_value_cache->find (&in);

      if (e)
	return (Node_Ref_Or_Val) e->to;
    }
  else
    in.base.from = NULL_TREE;

  /* If we do not return inside this switch, TCODE will be set to the
     code to be used in a call to Create_Node.  */
  switch (TREE_CODE (gnu_size))
    {
    case INTEGER_CST:
      /* For negative values, build NEGATE_EXPR of the opposite.  Such values
	 can appear for discriminants in expressions for variants.  */
      if (tree_int_cst_sgn (gnu_size) < 0)
	{
	  tree t = wide_int_to_tree (sizetype, -wi::to_wide (gnu_size));
	  tcode = Negate_Expr;
	  ops[0] = UI_From_gnu (t);
	}
      else
	return TREE_OVERFLOW (gnu_size) ? No_Uint : UI_From_gnu (gnu_size);
      break;

    case COMPONENT_REF:
      /* The only case we handle here is a simple discriminant reference.  */
      if (DECL_DISCRIMINANT_NUMBER (TREE_OPERAND (gnu_size, 1)))
	{
	  tree ref = gnu_size;
	  gnu_size = TREE_OPERAND (ref, 1);

	  /* Climb up the chain of successive extensions, if any.  */
	  while (TREE_CODE (TREE_OPERAND (ref, 0)) == COMPONENT_REF
		 && DECL_NAME (TREE_OPERAND (TREE_OPERAND (ref, 0), 1))
		    == parent_name_id)
	    ref = TREE_OPERAND (ref, 0);

	  if (TREE_CODE (TREE_OPERAND (ref, 0)) == PLACEHOLDER_EXPR)
	    {
	      /* Fall through to common processing as a FIELD_DECL.  */
	      tcode = Discrim_Val;
	      ops[0] = UI_From_gnu (DECL_DISCRIMINANT_NUMBER (gnu_size));
	    }
	  else
	    return No_Uint;
	}
      else
	return No_Uint;
      break;

    case VAR_DECL:
      tcode = Dynamic_Val;
      ops[0] = UI_From_Int (++var_count);
      break;

    CASE_CONVERT:
    case NON_LVALUE_EXPR:
      return annotate_value (TREE_OPERAND (gnu_size, 0));

      /* Now just list the operations we handle.  */
    case COND_EXPR:		tcode = Cond_Expr; break;
    case MINUS_EXPR:		tcode = Minus_Expr; break;
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
    case TRUTH_ANDIF_EXPR:
    case TRUTH_AND_EXPR:	tcode = Truth_And_Expr; break;
    case TRUTH_ORIF_EXPR:
    case TRUTH_OR_EXPR:		tcode = Truth_Or_Expr; break;
    case TRUTH_XOR_EXPR:	tcode = Truth_Xor_Expr; break;
    case TRUTH_NOT_EXPR:	tcode = Truth_Not_Expr; break;
    case LT_EXPR:		tcode = Lt_Expr; break;
    case LE_EXPR:		tcode = Le_Expr; break;
    case GT_EXPR:		tcode = Gt_Expr; break;
    case GE_EXPR:		tcode = Ge_Expr; break;
    case EQ_EXPR:		tcode = Eq_Expr; break;
    case NE_EXPR:		tcode = Ne_Expr; break;

    case PLUS_EXPR:
      /* Turn addition of negative constant into subtraction.  */
      if (TREE_CODE (TREE_OPERAND (gnu_size, 1)) == INTEGER_CST
	  && tree_int_cst_sign_bit (TREE_OPERAND (gnu_size, 1)))
	{
	  tcode = Minus_Expr;
	  wide_int wop1 = -wi::to_wide (TREE_OPERAND (gnu_size, 1));
	  ops[1] = annotate_value (wide_int_to_tree (sizetype, wop1));
	  break;
	}

      /* ... fall through ... */

    case MULT_EXPR:
      tcode = (TREE_CODE (gnu_size) == MULT_EXPR ? Mult_Expr : Plus_Expr);
      /* Fold conversions from bytes to bits into inner operations.  */
      if (TREE_CODE (TREE_OPERAND (gnu_size, 1)) == INTEGER_CST
	  && CONVERT_EXPR_P (TREE_OPERAND (gnu_size, 0)))
	{
	  tree inner_op = TREE_OPERAND (TREE_OPERAND (gnu_size, 0), 0);
	  if (TREE_CODE (inner_op) == TREE_CODE (gnu_size)
	      && TREE_CODE (TREE_OPERAND (inner_op, 1)) == INTEGER_CST)
	    {
	      ops[0] = annotate_value (TREE_OPERAND (inner_op, 0));
	      tree inner_op_op1 = TREE_OPERAND (inner_op, 1);
	      tree gnu_size_op1 = TREE_OPERAND (gnu_size, 1);
	      widest_int op1;
	      if (TREE_CODE (gnu_size) == MULT_EXPR)
		op1 = (wi::to_widest (inner_op_op1)
		       * wi::to_widest (gnu_size_op1));
	      else
		{
		  op1 = (wi::to_widest (inner_op_op1)
			 + wi::to_widest (gnu_size_op1));
		  if (wi::zext (op1, TYPE_PRECISION (sizetype)) == 0)
		    return ops[0];
		}
	      ops[1] = annotate_value (wide_int_to_tree (sizetype, op1));
	    }
	}
      break;

    case BIT_AND_EXPR:
      tcode = Bit_And_Expr;
      /* For negative values in sizetype, build NEGATE_EXPR of the opposite.
	 Such values can appear in expressions with aligning patterns.  */
      if (TREE_CODE (TREE_OPERAND (gnu_size, 1)) == INTEGER_CST)
	{
	  wide_int wop1 = -wi::to_wide (TREE_OPERAND (gnu_size, 1));
	  tree op1 = wide_int_to_tree (sizetype, wop1);
	  ops[1] = annotate_value (build1 (NEGATE_EXPR, sizetype, op1));
	}
      break;

    case CALL_EXPR:
      /* In regular mode, inline back only if symbolic annotation is requested
	 in order to avoid memory explosion on big discriminated record types.
	 But not in ASIS mode, as symbolic annotation is required for DDA.  */
      if (List_Representation_Info >= 3 || type_annotate_only)
	{
	  tree t = maybe_inline_call_in_expr (gnu_size);
	  return t ? annotate_value (t) : No_Uint;
	}
      else
	return Uint_Minus_1;

    default:
      return No_Uint;
    }

  /* Now get each of the operands that's relevant for this code.  If any
     cannot be expressed as a repinfo node, say we can't.  */
  for (int i = 0; i < TREE_CODE_LENGTH (TREE_CODE (gnu_size)); i++)
    if (ops[i] == No_Uint)
      {
	ops[i] = annotate_value (TREE_OPERAND (gnu_size, i));
	if (ops[i] == No_Uint)
	  return No_Uint;
      }

  Node_Ref_Or_Val ret = Create_Node (tcode, ops[0], ops[1], ops[2]);

  /* Save the result in the cache.  */
  if (in.base.from)
    {
      struct tree_int_map **h;
      /* We can't assume the hash table data hasn't moved since the initial
	 look up, so we have to search again.  Allocating and inserting an
	 entry at that point would be an alternative, but then we'd better
	 discard the entry if we decided not to cache it.  */
      h = annotate_value_cache->find_slot (&in, INSERT);
      gcc_assert (!*h);
      *h = ggc_alloc<tree_int_map> ();
      (*h)->base.from = in.base.from;
      (*h)->to = ret;
    }

  return ret;
}

/* Given GNAT_ENTITY, an object (constant, variable, parameter, exception)
   and GNU_TYPE, its corresponding GCC type, set Esize and Alignment to the
   size and alignment used by Gigi.  Prefer SIZE over TYPE_SIZE if non-null.
   BY_REF is true if the object is used by reference.  */

void
annotate_object (Entity_Id gnat_entity, tree gnu_type, tree size, bool by_ref)
{
  if (by_ref)
    {
      if (TYPE_IS_FAT_POINTER_P (gnu_type))
	gnu_type = TYPE_UNCONSTRAINED_ARRAY (gnu_type);
      else
	gnu_type = TREE_TYPE (gnu_type);
    }

  if (!Known_Esize (gnat_entity))
    {
      if (TREE_CODE (gnu_type) == RECORD_TYPE
	  && TYPE_CONTAINS_TEMPLATE_P (gnu_type))
	size = TYPE_SIZE (TREE_TYPE (DECL_CHAIN (TYPE_FIELDS (gnu_type))));
      else if (!size)
	size = TYPE_SIZE (gnu_type);

      if (size)
	Set_Esize (gnat_entity, No_Uint_To_0 (annotate_value (size)));
    }

  if (!Known_Alignment (gnat_entity))
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
  /* For an extension, the inherited components have not been translated because
     they are fetched from the _Parent component on the fly.  */
  const bool is_extension
    = Is_Tagged_Type (gnat_entity) && Is_Derived_Type (gnat_entity);

  /* We operate by first making a list of all fields and their position (we
     can get the size easily) and then update all the sizes in the tree.  */
  tree gnu_list
    = build_position_list (gnu_type, false, size_zero_node, bitsize_zero_node,
			   BIGGEST_ALIGNMENT, NULL_TREE);

  for (Entity_Id gnat_field = First_Entity (gnat_entity);
       Present (gnat_field);
       gnat_field = Next_Entity (gnat_field))
    if ((Ekind (gnat_field) == E_Component
	 && (is_extension || present_gnu_tree (gnat_field)))
	|| (Ekind (gnat_field) == E_Discriminant
	    && !Is_Unchecked_Union (Scope (gnat_field))))
      {
	tree t = purpose_member_field (gnat_to_gnu_field_decl (gnat_field),
				       gnu_list);
	if (t)
	  {
	    tree offset = TREE_VEC_ELT (TREE_VALUE (t), 0);
	    tree bit_offset = TREE_VEC_ELT (TREE_VALUE (t), 2);

	    /* If we are just annotating types and the type is tagged, the tag
	       and the parent components are not generated by the front-end so
	       we need to add the appropriate offset to each component without
	       representation clause.  */
	    if (type_annotate_only
		&& Is_Tagged_Type (gnat_entity)
		&& No (Component_Clause (gnat_field)))
	      {
		tree parent_bit_offset;

		/* For a component appearing in the current extension, the
		   offset is the size of the parent.  */
		if (Is_Derived_Type (gnat_entity)
		    && Original_Record_Component (gnat_field) == gnat_field)
		  parent_bit_offset
		    = UI_To_gnu (Esize (Etype (Base_Type (gnat_entity))),
				 bitsizetype);
		else
		  parent_bit_offset = bitsize_int (POINTER_SIZE);

		if (TYPE_FIELDS (gnu_type))
		  parent_bit_offset
		    = round_up (parent_bit_offset,
				DECL_ALIGN (TYPE_FIELDS (gnu_type)));

		offset
		  = size_binop (PLUS_EXPR, offset,
				fold_convert (sizetype,
					      size_binop (TRUNC_DIV_EXPR,
							  parent_bit_offset,
							  bitsize_unit_node)));
	      }

	    /* If the field has a variable offset, also compute the normalized
	       position since it's easier to do on trees here than to deduce
	       it from the annotated expression of Component_Bit_Offset.  */
	    if (TREE_CODE (offset) != INTEGER_CST)
	      {
		normalize_offset (&offset, &bit_offset, BITS_PER_UNIT);
		Set_Normalized_Position (gnat_field,
					 annotate_value (offset));
		Set_Normalized_First_Bit (gnat_field,
					  annotate_value (bit_offset));
	      }

	    Set_Component_Bit_Offset
	      (gnat_field,
	       annotate_value (bit_from_pos (offset, bit_offset)));

	    Set_Esize
	      (gnat_field,
	       No_Uint_To_0 (annotate_value (DECL_SIZE (TREE_PURPOSE (t)))));
	  }
	else if (is_extension)
	  {
	    /* If there is no entry, this is an inherited component whose
	       position is the same as in the parent type.  */
	    Entity_Id gnat_orig = Original_Record_Component (gnat_field);

	    /* If we are just annotating types, discriminants renaming those of
	       the parent have no entry so deal with them specifically.  */
	    if (type_annotate_only
		&& gnat_orig == gnat_field
		&& Ekind (gnat_field) == E_Discriminant)
	      gnat_orig = Corresponding_Discriminant (gnat_field);

	    if (Known_Normalized_Position (gnat_orig))
	      {
		Set_Normalized_Position (gnat_field,
					 Normalized_Position (gnat_orig));
		Set_Normalized_First_Bit (gnat_field,
					  Normalized_First_Bit (gnat_orig));
	      }

	    Set_Component_Bit_Offset (gnat_field,
				      Component_Bit_Offset (gnat_orig));

	    Set_Esize (gnat_field, Esize (gnat_orig));
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

/* Return a list describing the substitutions needed to reflect the
   discriminant substitutions from GNAT_TYPE to GNAT_SUBTYPE.  They can
   be in any order.  The values in an element of the list are in the form
   of operands to SUBSTITUTE_IN_EXPR.  DEFINITION is true if this is for
   a definition of GNAT_SUBTYPE.  */

static vec<subst_pair>
build_subst_list (Entity_Id gnat_subtype, Entity_Id gnat_type, bool definition)
{
  vec<subst_pair> gnu_list = vNULL;
  Entity_Id gnat_discrim;
  Node_Id gnat_constr;

  for (gnat_discrim = First_Stored_Discriminant (gnat_type),
       gnat_constr = First_Elmt (Stored_Constraint (gnat_subtype));
       Present (gnat_discrim);
       gnat_discrim = Next_Stored_Discriminant (gnat_discrim),
       gnat_constr = Next_Elmt (gnat_constr))
    /* Ignore access discriminants.  */
    if (!Is_Access_Type (Etype (Node (gnat_constr))))
      {
	tree gnu_field = gnat_to_gnu_field_decl (gnat_discrim);
	tree replacement
	  = elaborate_expression (Node (gnat_constr), gnat_subtype,
				  get_entity_char (gnat_discrim),
				  definition, true, false);
	/* If this is a definition, we need to make sure that the SAVE_EXPRs
	   are instantiated on every possibly path in size computations.  */
	if (definition && TREE_CODE (replacement) == SAVE_EXPR)
	  add_stmt (replacement);
	replacement = convert (TREE_TYPE (gnu_field), replacement);
	subst_pair s = { gnu_field, replacement };
	gnu_list.safe_push (s);
      }

  return gnu_list;
}

/* Scan all fields in {GNU_QUAL_UNION_TYPE,GNAT_VARIANT_PART} and return a list
   describing the variants of GNU_QUAL_UNION_TYPE that are still relevant after
   applying the substitutions described in SUBST_LIST.  GNU_LIST is an existing
   list to be prepended to the newly created entries.  */

static vec<variant_desc>
build_variant_list (tree gnu_qual_union_type, Node_Id gnat_variant_part,
		    vec<subst_pair> subst_list, vec<variant_desc> gnu_list)
{
  Node_Id gnat_variant;
  tree gnu_field;

  for (gnu_field = TYPE_FIELDS (gnu_qual_union_type),
       gnat_variant
	= Present (gnat_variant_part)
	  ? First_Non_Pragma (Variants (gnat_variant_part))
	  : Empty;
       gnu_field;
       gnu_field = DECL_CHAIN (gnu_field),
       gnat_variant
	= Present (gnat_variant_part)
	  ? Next_Non_Pragma (gnat_variant)
	  : Empty)
    {
      tree qual = DECL_QUALIFIER (gnu_field);
      unsigned int i;
      subst_pair *s;

      FOR_EACH_VEC_ELT (subst_list, i, s)
	qual = SUBSTITUTE_IN_EXPR (qual, s->discriminant, s->replacement);

      /* If the new qualifier is not unconditionally false, its variant may
	 still be accessed.  */
      if (!integer_zerop (qual))
	{
	  tree variant_type = TREE_TYPE (gnu_field), variant_subpart;
	  variant_desc v
	    = { variant_type, gnu_field, qual, NULL_TREE, NULL_TREE };

	  gnu_list.safe_push (v);

	  /* Annotate the GNAT node if present.  */
	  if (Present (gnat_variant))
	    Set_Present_Expr (gnat_variant, annotate_value (qual));

	  /* Recurse on the variant subpart of the variant, if any.  */
	  variant_subpart = get_variant_part (variant_type);
	  if (variant_subpart)
	    gnu_list
	      = build_variant_list (TREE_TYPE (variant_subpart),
				    Present (gnat_variant)
				    ? Variant_Part
				      (Component_List (gnat_variant))
				    : Empty,
				    subst_list,
				    gnu_list);

	  /* If the new qualifier is unconditionally true, the subsequent
	     variants cannot be accessed.  */
	  if (integer_onep (qual))
	    break;
	}
    }

  return gnu_list;
}

/* If SIZE has overflowed, return the maximum valid size, which is the upper
   bound of the signed sizetype in bits, rounded down to ALIGN.  Otherwise
   return SIZE unmodified.  */

static tree
maybe_saturate_size (tree size, unsigned int align)
{
  if (TREE_CODE (size) == INTEGER_CST && TREE_OVERFLOW (size))
    {
      size
	= size_binop (MULT_EXPR,
		      fold_convert (bitsizetype, TYPE_MAX_VALUE (ssizetype)),
		      build_int_cst (bitsizetype, BITS_PER_UNIT));
      size = round_down (size, align);
    }

  return size;
}

/* UINT_SIZE is a Uint giving the specified size for an object of GNU_TYPE
   corresponding to GNAT_OBJECT.  If the size is valid, return an INTEGER_CST
   corresponding to its value.  Otherwise, return NULL_TREE.  KIND is set to
   VAR_DECL if we are specifying the size of an object, TYPE_DECL for the
   size of a type, and FIELD_DECL for the size of a field.  COMPONENT_P is
   true if we are being called to process the Component_Size of GNAT_OBJECT;
   this is used only for error messages.  ZERO_OK is true if a size of zero
   is permitted; if ZERO_OK is false, it means that a size of zero should be
   treated as an unspecified size.  S1 and S2 are used for error messages.  */

static tree
validate_size (Uint uint_size, tree gnu_type, Entity_Id gnat_object,
	       enum tree_code kind, bool component_p, bool zero_ok,
	       const char *s1, const char *s2)
{
  Node_Id gnat_error_node;
  tree old_size, size;

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
  else if (Has_Object_Size_Clause (gnat_object))
    gnat_error_node = Expression (Object_Size_Clause (gnat_object));
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
	post_error_ne ("component size for& must be multiple of Storage_Unit",
		       gnat_error_node, gnat_object);
      else
	post_error_ne ("size for& must be multiple of Storage_Unit",
		       gnat_error_node, gnat_object);
      return NULL_TREE;
    }

  /* If this is an integral type or a bit-packed array type, the front-end has
     already verified the size, so we need not do it again (which would mean
     checking against the bounds).  However, if this is an aliased object, it
     may not be smaller than the type of the object.  */
  if ((INTEGRAL_TYPE_P (gnu_type) || BIT_PACKED_ARRAY_TYPE_P (gnu_type))
      && !(kind == VAR_DECL && Is_Aliased (gnat_object)))
    return size;

  /* If the object is a record that contains a template, add the size of the
     template to the specified size.  */
  if (TREE_CODE (gnu_type) == RECORD_TYPE
      && TYPE_CONTAINS_TEMPLATE_P (gnu_type))
    size = size_binop (PLUS_EXPR, DECL_SIZE (TYPE_FIELDS (gnu_type)), size);

  old_size = (kind == VAR_DECL ? TYPE_SIZE (gnu_type) : rm_size (gnu_type));

  /* If the old size is self-referential, get the maximum size.  */
  if (CONTAINS_PLACEHOLDER_P (old_size))
    old_size = max_size (old_size, true);

  /* If this is an access type or a fat pointer, the minimum size is that given
     by the smallest integral mode that's valid for pointers.  */
  if (TREE_CODE (gnu_type) == POINTER_TYPE || TYPE_IS_FAT_POINTER_P (gnu_type))
    {
      scalar_int_mode p_mode = NARROWEST_INT_MODE;
      while (!targetm.valid_pointer_mode (p_mode))
	p_mode = GET_MODE_WIDER_MODE (p_mode).require ();
      old_size = bitsize_int (GET_MODE_BITSIZE (p_mode));
    }

  /* Issue an error either if the default size of the object isn't a constant
     or if the new size is smaller than it.  */
  if (TREE_CODE (old_size) != INTEGER_CST
      || (!TREE_OVERFLOW (old_size) && tree_int_cst_lt (size, old_size)))
    {
      char buf[128];
      const char *s;

      if (s1 && s2)
	{
	  snprintf (buf, sizeof (buf), s1, s2);
	  s = buf;
	}
      else if (component_p)
	s = "component size for& too small{, minimum allowed is ^}";
      else
	s = "size for& too small{, minimum allowed is ^}";

      post_error_ne_tree (s, gnat_error_node, gnat_object, old_size);

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

  /* Only issue an error if a Value_Size clause was explicitly given for the
     entity; otherwise, we'd be duplicating an error on the Size clause.  */
  gnat_attr_node
    = Get_Attribute_Definition_Clause (gnat_entity, Attr_Value_Size);
  if (Present (gnat_attr_node) && Entity (gnat_attr_node) != gnat_entity)
    gnat_attr_node = Empty;

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
     this for scalar and bit-packed array types.  */
  if (TREE_CODE (old_size) != INTEGER_CST
      || TREE_OVERFLOW (old_size)
      || (AGGREGATE_TYPE_P (gnu_type)
	  && !BIT_PACKED_ARRAY_TYPE_P (gnu_type)
	  && !(TYPE_IS_PADDING_P (gnu_type)
	       && BIT_PACKED_ARRAY_TYPE_P (TREE_TYPE (TYPE_FIELDS (gnu_type))))
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

/* Promote the alignment of GNU_TYPE for an object with GNU_SIZE corresponding
   to GNAT_ENTITY.  Return a positive value on success or zero on failure.  */

static unsigned int
promote_object_alignment (tree gnu_type, tree gnu_size, Entity_Id gnat_entity)
{
  unsigned int align, size_cap, align_cap;

  /* No point in promoting the alignment if this doesn't prevent BLKmode access
     to the object, in particular block copy, as this will for example disable
     the NRV optimization for it.  No point in jumping through all the hoops
     needed in order to support BIGGEST_ALIGNMENT if we don't really have to.
     So we cap to the smallest alignment that corresponds to a known efficient
     memory access pattern, except for a full access entity.  */
  if (Is_Full_Access (gnat_entity))
    {
      size_cap = UINT_MAX;
      align_cap = BIGGEST_ALIGNMENT;
    }
  else
    {
      size_cap = MAX_FIXED_MODE_SIZE;
      align_cap = get_mode_alignment (ptr_mode);
    }

  if (!gnu_size)
    gnu_size = TYPE_SIZE (gnu_type);

  /* Do the promotion within the above limits.  */
  if (!tree_fits_uhwi_p (gnu_size)
      || compare_tree_int (gnu_size, size_cap) > 0)
    align = 0;
  else if (compare_tree_int (gnu_size, align_cap) > 0)
    align = align_cap;
  else
    align = ceil_pow2 (tree_to_uhwi (gnu_size));

  /* But make sure not to under-align the object.  */
  if (align <= TYPE_ALIGN (gnu_type))
    align = 0;

   /* And honor the minimum valid atomic alignment, if any.  */
#ifdef MINIMUM_ATOMIC_ALIGNMENT
  else if (align < MINIMUM_ATOMIC_ALIGNMENT)
    align = MINIMUM_ATOMIC_ALIGNMENT;
#endif

  return align;
}

/* Verify that TYPE is something we can implement atomically.  If not, issue
   an error for GNAT_ENTITY.  COMPONENT_P is true if we are being called to
   process a component type.  */

static void
check_ok_for_atomic_type (tree type, Entity_Id gnat_entity, bool component_p)
{
  Node_Id gnat_error_point = gnat_entity;
  Node_Id gnat_node;
  machine_mode mode;
  enum mode_class mclass;
  unsigned int align;
  tree size;

  /* If this is an anonymous base type, nothing to check, the error will be
     reported on the source type if need be.  */
  if (!Comes_From_Source (gnat_entity))
    return;

  mode = TYPE_MODE (type);
  mclass = GET_MODE_CLASS (mode);
  align = TYPE_ALIGN (type);
  size = TYPE_SIZE (type);

  /* Consider all aligned floating-point types atomic and any aligned types
     that are represented by integers no wider than a machine word.  */
  scalar_int_mode int_mode;
  if ((mclass == MODE_FLOAT
       || (is_a <scalar_int_mode> (mode, &int_mode)
	   && GET_MODE_BITSIZE (int_mode) <= BITS_PER_WORD))
      && align >= GET_MODE_ALIGNMENT (mode))
    return;

  /* For the moment, also allow anything that has an alignment equal to its
     size and which is smaller than a word.  */
  if (size
      && TREE_CODE (size) == INTEGER_CST
      && compare_tree_int (size, align) == 0
      && align <= BITS_PER_WORD)
    return;

  for (gnat_node = First_Rep_Item (gnat_entity);
       Present (gnat_node);
       gnat_node = Next_Rep_Item (gnat_node))
    if (Nkind (gnat_node) == N_Pragma)
      {
	unsigned char pragma_id
	  = Get_Pragma_Id (Chars (Pragma_Identifier (gnat_node)));

	if ((pragma_id == Pragma_Atomic && !component_p)
	    || (pragma_id == Pragma_Atomic_Components && component_p))
	  {
	    gnat_error_point = First (Pragma_Argument_Associations (gnat_node));
	    break;
	  }
      }

  if (component_p)
    post_error_ne ("atomic access to component of & cannot be guaranteed",
		   gnat_error_point, gnat_entity);
  else if (Is_Volatile_Full_Access (gnat_entity))
    post_error_ne ("volatile full access to & cannot be guaranteed",
		   gnat_error_point, gnat_entity);
  else
    post_error_ne ("atomic access to & cannot be guaranteed",
		   gnat_error_point, gnat_entity);
}

/* Return true if TYPE is suitable for a type-generic atomic builtin.  */

static bool
type_for_atomic_builtin_p (tree type)
{
  const enum machine_mode mode = TYPE_MODE (type);
  if (GET_MODE_CLASS (mode) == MODE_FLOAT)
    return true;

  scalar_int_mode imode;
  if (is_a <scalar_int_mode> (mode, &imode) && GET_MODE_SIZE (imode) <= 16)
    return true;

  return false;
}

/* Return the GCC atomic builtin based on CODE and sized for TYPE.  */

static tree
resolve_atomic_builtin (enum built_in_function code, tree type)
{
  const unsigned int size = resolve_atomic_size (type);
  code = (enum built_in_function) ((int) code + exact_log2 (size) + 1);

  return builtin_decl_implicit (code);
}

/* Helper for intrin_profiles_compatible_p, to perform compatibility checks
   on the Ada/builtin argument lists for the INB binding.  */

static bool
intrin_arglists_compatible_p (const intrin_binding_t *inb)
{
  function_args_iterator ada_iter, btin_iter;

  function_args_iter_init (&ada_iter, inb->ada_fntype);
  function_args_iter_init (&btin_iter, inb->btin_fntype);

  /* Sequence position of the last argument we checked.  */
  int argpos = 0;

  while (true)
    {
      tree ada_type = function_args_iter_cond (&ada_iter);
      tree btin_type = function_args_iter_cond (&btin_iter);

      /* If we've exhausted both lists simultaneously, we're done.  */
      if (!ada_type && !btin_type)
	break;

      /* If the internal builtin uses a variable list, accept anything.  */
      if (!btin_type)
	break;

      /* If we're done with the Ada args and not with the internal builtin
	 args, or the other way around, complain.  */
      if (ada_type == void_type_node && btin_type != void_type_node)
	{
	  post_error ("??Ada parameter list too short!", inb->gnat_entity);
	  return false;
	}

      if (btin_type == void_type_node && ada_type != void_type_node)
	{
	  post_error_ne_num ("??Ada parameter list too long ('> ^)!",
			     inb->gnat_entity, inb->gnat_entity, argpos);
	  return false;
	}

      /* Otherwise, check that types match for the current argument.  */
      argpos++;
      if (!types_compatible_p (ada_type, btin_type))
	{
	  /* For vector builtins, issue an error to avoid an ICE.  */
	  if (VECTOR_TYPE_P (btin_type))
	    post_error_ne_num
	      ("intrinsic binding type mismatch on parameter ^",
	       inb->gnat_entity, inb->gnat_entity, argpos);
	  else
	    post_error_ne_num
	      ("??intrinsic binding type mismatch on parameter ^!",
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
intrin_return_compatible_p (const intrin_binding_t *inb)
{
  tree ada_return_type = TREE_TYPE (inb->ada_fntype);
  tree btin_return_type = TREE_TYPE (inb->btin_fntype);

  /* Accept function imported as procedure, common and convenient.  */
  if (VOID_TYPE_P (ada_return_type) && !VOID_TYPE_P (btin_return_type))
    return true;

  /* Check return types compatibility otherwise.  Note that this
     handles void/void as well.  */
  if (!types_compatible_p (btin_return_type, ada_return_type))
    {
      /* For vector builtins, issue an error to avoid an ICE.  */
      if (VECTOR_TYPE_P (btin_return_type))
	post_error ("intrinsic binding type mismatch on result",
		    inb->gnat_entity);
      else
	post_error ("??intrinsic binding type mismatch on result",
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
intrin_profiles_compatible_p (const intrin_binding_t *inb)
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
			vec<subst_pair> subst_list)
{
  tree t = TREE_VALUE (purpose_member (old_field, pos_list));
  tree pos = TREE_VEC_ELT (t, 0), bitpos = TREE_VEC_ELT (t, 2);
  unsigned int offset_align = tree_to_uhwi (TREE_VEC_ELT (t, 1));
  tree new_pos, new_field;
  unsigned int i;
  subst_pair *s;

  if (CONTAINS_PLACEHOLDER_P (pos))
    FOR_EACH_VEC_ELT (subst_list, i, s)
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
      /* Finalize the position.  */
      DECL_FIELD_OFFSET (new_field) = variable_size (pos);
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
			     min_size, NULL_TREE, 0, 1);
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
  if (field
      && DECL_INTERNAL_P (field)
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
   layout.  DEBUG_INFO_P is true if we need to write debug information.  */

static tree
create_variant_part_from (tree old_variant_part,
			  vec<variant_desc> variant_list,
			  tree record_type, tree pos_list,
			  vec<subst_pair> subst_list,
			  bool debug_info_p)
{
  tree offset = DECL_FIELD_OFFSET (old_variant_part);
  tree old_union_type = TREE_TYPE (old_variant_part);
  tree new_union_type, new_variant_part;
  tree union_field_list = NULL_TREE;
  variant_desc *v;
  unsigned int i;

  /* First create the type of the variant part from that of the old one.  */
  new_union_type = make_node (QUAL_UNION_TYPE);
  TYPE_NAME (new_union_type)
    = concat_name (TYPE_NAME (record_type),
		   IDENTIFIER_POINTER (DECL_NAME (old_variant_part)));

  /* If the position of the variant part is constant, subtract it from the
     size of the type of the parent to get the new size.  This manual CSE
     reduces the code size when not optimizing.  */
  if (TREE_CODE (offset) == INTEGER_CST
      && TYPE_SIZE (record_type)
      && TYPE_SIZE_UNIT (record_type))
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
      SET_TYPE_ALIGN (new_union_type, TYPE_ALIGN (old_union_type));
      relate_alias_sets (new_union_type, old_union_type, ALIAS_SET_COPY);
    }
  else
    copy_and_substitute_in_size (new_union_type, old_union_type, subst_list);

  /* Now finish up the new variants and populate the union type.  */
  FOR_EACH_VEC_ELT_REVERSE (variant_list, i, v)
    {
      tree old_field = v->field, new_field;
      tree old_variant, old_variant_subpart, new_variant, field_list;

      /* Skip variants that don't belong to this nesting level.  */
      if (DECL_CONTEXT (old_field) != old_union_type)
	continue;

      /* Retrieve the list of fields already added to the new variant.  */
      new_variant = v->new_type;
      field_list = TYPE_FIELDS (new_variant);

      /* If the old variant had a variant subpart, we need to create a new
	 variant subpart and add it to the field list.  */
      old_variant = v->type;
      old_variant_subpart = get_variant_part (old_variant);
      if (old_variant_subpart)
	{
	  tree new_variant_subpart
	    = create_variant_part_from (old_variant_subpart, variant_list,
					new_variant, pos_list, subst_list,
					debug_info_p);
	  DECL_CHAIN (new_variant_subpart) = field_list;
	  field_list = new_variant_subpart;
	}

      /* Finish up the new variant and create the field.  */
      finish_record_type (new_variant, nreverse (field_list), 2, debug_info_p);
      create_type_decl (TYPE_NAME (new_variant), new_variant, true,
			debug_info_p, Empty);

      new_field
	= create_field_decl_from (old_field, new_variant, new_union_type,
				  TYPE_SIZE (new_variant),
				  pos_list, subst_list);
      DECL_QUALIFIER (new_field) = v->qual;
      DECL_INTERNAL_P (new_field) = 1;
      DECL_CHAIN (new_field) = union_field_list;
      union_field_list = new_field;
    }

  /* Finish up the union type and create the variant part.  Note that we don't
     reverse the field list because VARIANT_LIST has been traversed in reverse
     order.  */
  finish_record_type (new_union_type, union_field_list, 2, debug_info_p);
  create_type_decl (TYPE_NAME (new_union_type), new_union_type, true,
		    debug_info_p, Empty);

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
			     vec<subst_pair> subst_list)
{
  unsigned int i;
  subst_pair *s;

  TYPE_SIZE (new_type) = TYPE_SIZE (old_type);
  TYPE_SIZE_UNIT (new_type) = TYPE_SIZE_UNIT (old_type);
  SET_TYPE_ADA_SIZE (new_type, TYPE_ADA_SIZE (old_type));
  SET_TYPE_ALIGN (new_type, TYPE_ALIGN (old_type));
  relate_alias_sets (new_type, old_type, ALIAS_SET_COPY);

  if (CONTAINS_PLACEHOLDER_P (TYPE_SIZE (new_type)))
    FOR_EACH_VEC_ELT (subst_list, i, s)
      TYPE_SIZE (new_type)
	= SUBSTITUTE_IN_EXPR (TYPE_SIZE (new_type),
			      s->discriminant, s->replacement);

  if (CONTAINS_PLACEHOLDER_P (TYPE_SIZE_UNIT (new_type)))
    FOR_EACH_VEC_ELT (subst_list, i, s)
      TYPE_SIZE_UNIT (new_type)
	= SUBSTITUTE_IN_EXPR (TYPE_SIZE_UNIT (new_type),
			      s->discriminant, s->replacement);

  if (CONTAINS_PLACEHOLDER_P (TYPE_ADA_SIZE (new_type)))
    FOR_EACH_VEC_ELT (subst_list, i, s)
      SET_TYPE_ADA_SIZE
	(new_type, SUBSTITUTE_IN_EXPR (TYPE_ADA_SIZE (new_type),
				       s->discriminant, s->replacement));

  /* Finalize the size.  */
  TYPE_SIZE (new_type) = variable_size (TYPE_SIZE (new_type));
  TYPE_SIZE_UNIT (new_type) = variable_size (TYPE_SIZE_UNIT (new_type));
}

/* Return true if DISC is a stored discriminant of RECORD_TYPE.  */

static inline bool
is_stored_discriminant (Entity_Id discr, Entity_Id record_type)
{
  if (Is_Unchecked_Union (record_type))
    return false;
  else if (Is_Tagged_Type (record_type))
    return No (Corresponding_Discriminant (discr));
  else if (Ekind (record_type) == E_Record_Type)
    return Original_Record_Component (discr) == discr;
  else
    return true;
}

/* Copy the layout from {GNAT,GNU}_OLD_TYPE to {GNAT,GNU}_NEW_TYPE, which are
   both record types, after applying the substitutions described in SUBST_LIST.
   DEBUG_INFO_P is true if we need to write debug information for NEW_TYPE.  */

static void
copy_and_substitute_in_layout (Entity_Id gnat_new_type,
			       Entity_Id gnat_old_type,
			       tree gnu_new_type,
			       tree gnu_old_type,
			       vec<subst_pair> subst_list,
			       bool debug_info_p)
{
  const bool is_subtype = (Ekind (gnat_new_type) == E_Record_Subtype);
  tree gnu_field_list = NULL_TREE;
  tree gnu_variable_field_list = NULL_TREE;
  bool selected_variant;
  vec<variant_desc> gnu_variant_list;

  /* Look for REP and variant parts in the old type.  */
  tree gnu_rep_part = get_rep_part (gnu_old_type);
  tree gnu_variant_part = get_variant_part (gnu_old_type);

  /* If there is a variant part, we must compute whether the constraints
     statically select a particular variant.  If so, we simply drop the
     qualified union and flatten the list of fields.  Otherwise we will
     build a new qualified union for the variants that are still relevant.  */
  if (gnu_variant_part)
    {
      const Node_Id gnat_decl = Declaration_Node (gnat_new_type);
      variant_desc *v;
      unsigned int i;

      gnu_variant_list
	= build_variant_list (TREE_TYPE (gnu_variant_part),
			      is_subtype
			      ? Empty
			      : Variant_Part
				(Component_List (Type_Definition (gnat_decl))),
			      subst_list,
			      vNULL);

      /* If all the qualifiers are unconditionally true, the innermost variant
	 is statically selected.  */
      selected_variant = true;
      FOR_EACH_VEC_ELT (gnu_variant_list, i, v)
	if (!integer_onep (v->qual))
	  {
	    selected_variant = false;
	    break;
	  }

      /* Otherwise, create the new variants.  */
      if (!selected_variant)
	FOR_EACH_VEC_ELT (gnu_variant_list, i, v)
	  {
	    tree old_variant = v->type;
	    tree new_variant = make_node (RECORD_TYPE);
	    tree suffix
	      = concat_name (DECL_NAME (gnu_variant_part),
			     IDENTIFIER_POINTER (DECL_NAME (v->field)));
	    TYPE_NAME (new_variant)
	      = concat_name (TYPE_NAME (gnu_new_type),
			     IDENTIFIER_POINTER (suffix));
	    TYPE_REVERSE_STORAGE_ORDER (new_variant)
	      = TYPE_REVERSE_STORAGE_ORDER (gnu_new_type);
	    copy_and_substitute_in_size (new_variant, old_variant, subst_list);
	    v->new_type = new_variant;
	  }
    }
  else
    {
      gnu_variant_list.create (0);
      selected_variant = false;
    }

  /* Make a list of fields and their position in the old type.  */
  tree gnu_pos_list
    = build_position_list (gnu_old_type,
			   gnu_variant_list.exists () && !selected_variant,
			   size_zero_node, bitsize_zero_node,
			   BIGGEST_ALIGNMENT, NULL_TREE);

  /* Now go down every component in the new type and compute its size and
     position from those of the component in the old type and the stored
     constraints of the new type.  */
  Entity_Id gnat_field, gnat_old_field;
  for (gnat_field = First_Entity (gnat_new_type);
       Present (gnat_field);
       gnat_field = Next_Entity (gnat_field))
    if ((Ekind (gnat_field) == E_Component
	 || (Ekind (gnat_field) == E_Discriminant
	     && is_stored_discriminant (gnat_field, gnat_new_type)))
        && (gnat_old_field = is_subtype
			     ? Original_Record_Component (gnat_field)
			     : Corresponding_Record_Component (gnat_field))
	&& Underlying_Type (Scope (gnat_old_field)) == gnat_old_type
	&& present_gnu_tree (gnat_old_field))
      {
	Name_Id gnat_name = Chars (gnat_field);
	tree gnu_old_field = get_gnu_tree (gnat_old_field);
	if (TREE_CODE (gnu_old_field) == COMPONENT_REF)
	  gnu_old_field = TREE_OPERAND (gnu_old_field, 1);
        tree gnu_context = DECL_CONTEXT (gnu_old_field);
	tree gnu_field, gnu_field_type, gnu_size, gnu_pos;
	tree gnu_cont_type, gnu_last = NULL_TREE;
	variant_desc *v = NULL;

	/* If the type is the same, retrieve the GCC type from the
	   old field to take into account possible adjustments.  */
	if (Etype (gnat_field) == Etype (gnat_old_field))
	  gnu_field_type = TREE_TYPE (gnu_old_field);
	else
	  gnu_field_type = gnat_to_gnu_type (Etype (gnat_field));

	/* If there was a component clause, the field types must be the same
	   for the old and new types, so copy the data from the old field to
	   avoid recomputation here.  Also if the field is justified modular
	   and the optimization in gnat_to_gnu_field was applied.  */
	if (Present (Component_Clause (gnat_old_field))
	    || (TREE_CODE (gnu_field_type) == RECORD_TYPE
		&& TYPE_JUSTIFIED_MODULAR_P (gnu_field_type)
		&& TREE_TYPE (TYPE_FIELDS (gnu_field_type))
		   == TREE_TYPE (gnu_old_field)))
	  {
	    gnu_size = DECL_SIZE (gnu_old_field);
	    gnu_field_type = TREE_TYPE (gnu_old_field);
	  }

	/* If the old field was packed and of constant size, we have to get the
	   old size here as it might differ from what the Etype conveys and the
	   latter might overlap with the following field.  Try to arrange the
	   type for possible better packing along the way.  */
	else if (DECL_PACKED (gnu_old_field)
		 && TREE_CODE (DECL_SIZE (gnu_old_field)) == INTEGER_CST)
	  {
	    gnu_size = DECL_SIZE (gnu_old_field);
	    if (RECORD_OR_UNION_TYPE_P (gnu_field_type)
		&& !TYPE_FAT_POINTER_P (gnu_field_type)
		&& tree_fits_uhwi_p (TYPE_SIZE (gnu_field_type)))
	      gnu_field_type = make_packable_type (gnu_field_type, true, 0);
	  }

	else
	  gnu_size = TYPE_SIZE (gnu_field_type);

	/* If the context of the old field is the old type or its REP part,
	   put the field directly in the new type; otherwise look up the
	   context in the variant list and put the field either in the new
	   type if there is a selected variant or in one new variant.  */
	if (gnu_context == gnu_old_type
	    || (gnu_rep_part && gnu_context == TREE_TYPE (gnu_rep_part)))
	  gnu_cont_type = gnu_new_type;
	else
	  {
	    unsigned int i;
	    tree rep_part;

	    FOR_EACH_VEC_ELT (gnu_variant_list, i, v)
	      if (gnu_context == v->type
		  || ((rep_part = get_rep_part (v->type))
		      && gnu_context == TREE_TYPE (rep_part)))
		break;

	    if (v)
	      gnu_cont_type = selected_variant ? gnu_new_type : v->new_type;
	    else
	      /* The front-end may pass us zombie components if it fails to
		 recognize that a constrain statically selects a particular
		 variant.  Discard them.  */
	      continue;
	  }

	/* Now create the new field modeled on the old one.  */
	gnu_field
	  = create_field_decl_from (gnu_old_field, gnu_field_type,
				    gnu_cont_type, gnu_size,
				    gnu_pos_list, subst_list);
	gnu_pos = DECL_FIELD_OFFSET (gnu_field);

	/* If the context is a variant, put it in the new variant directly.  */
	if (gnu_cont_type != gnu_new_type)
	  {
	    if (TREE_CODE (gnu_pos) == INTEGER_CST)
	      {
		DECL_CHAIN (gnu_field) = TYPE_FIELDS (gnu_cont_type);
		TYPE_FIELDS (gnu_cont_type) = gnu_field;
	      }
	    else
	      {
		DECL_CHAIN (gnu_field) = v->aux;
		v->aux = gnu_field;
	      }
	  }

	/* To match the layout crafted in components_to_record, if this is
	   the _Tag or _Parent field, put it before any other fields.  */
	else if (gnat_name == Name_uTag || gnat_name == Name_uParent)
	  gnu_field_list = chainon (gnu_field_list, gnu_field);

	/* Similarly, if this is the _Controller field, put it before the
	   other fields except for the _Tag or _Parent field.  */
	else if (gnat_name == Name_uController && gnu_last)
	  {
	    DECL_CHAIN (gnu_field) = DECL_CHAIN (gnu_last);
	    DECL_CHAIN (gnu_last) = gnu_field;
	  }

	/* Otherwise, put it after the other fields.  */
	else
	  {
	    if (TREE_CODE (gnu_pos) == INTEGER_CST)
	      {
		DECL_CHAIN (gnu_field) = gnu_field_list;
		gnu_field_list = gnu_field;
		if (!gnu_last)
		  gnu_last = gnu_field;
	      }
	    else
	      {
		DECL_CHAIN (gnu_field) = gnu_variable_field_list;
		gnu_variable_field_list = gnu_field;
	      }
	  }

	/* For a stored discriminant in a derived type, replace the field.  */
	if (!is_subtype && Ekind (gnat_field) == E_Discriminant)
	  {
	    tree gnu_ref = get_gnu_tree (gnat_field);
	    TREE_OPERAND (gnu_ref, 1) = gnu_field;
	  }
	else
	  save_gnu_tree (gnat_field, gnu_field, false);
      }

  /* Put the fields with fixed position in order of increasing position.  */
  if (gnu_field_list)
    gnu_field_list = reverse_sort_field_list (gnu_field_list);

  /* Put the fields with variable position at the end.  */
  if (gnu_variable_field_list)
    gnu_field_list = chainon (gnu_variable_field_list, gnu_field_list);

  /* If there is a variant list and no selected variant, we need to create the
     nest of variant parts from the old nest.  */
  if (gnu_variant_list.exists () && !selected_variant)
    {
      variant_desc *v;
      unsigned int i;

      /* Same processing as above for the fields of each variant.  */
      FOR_EACH_VEC_ELT (gnu_variant_list, i, v)
	{
	  if (TYPE_FIELDS (v->new_type))
	    TYPE_FIELDS (v->new_type)
	      = reverse_sort_field_list (TYPE_FIELDS (v->new_type));
	  if (v->aux)
	    TYPE_FIELDS (v->new_type)
	      = chainon (v->aux, TYPE_FIELDS (v->new_type));
	}

      tree new_variant_part
	= create_variant_part_from (gnu_variant_part, gnu_variant_list,
				    gnu_new_type, gnu_pos_list,
				    subst_list, debug_info_p);
      DECL_CHAIN (new_variant_part) = gnu_field_list;
      gnu_field_list = new_variant_part;
    }

  gnu_variant_list.release ();
  subst_list.release ();

  /* If NEW_TYPE is a subtype, it inherits all the attributes from OLD_TYPE.
     Otherwise sizes and alignment must be computed independently.  */
  finish_record_type (gnu_new_type, nreverse (gnu_field_list),
		      is_subtype ? 2 : 1, debug_info_p);

  /* Now go through the entities again looking for itypes that we have not yet
     elaborated (e.g. Etypes of fields that have Original_Components).  */
  for (Entity_Id gnat_field = First_Entity (gnat_new_type);
       Present (gnat_field);
       gnat_field = Next_Entity (gnat_field))
    if ((Ekind (gnat_field) == E_Component
	 || Ekind (gnat_field) == E_Discriminant)
	&& Is_Itype (Etype (gnat_field))
	&& !present_gnu_tree (Etype (gnat_field)))
      gnat_to_gnu_entity (Etype (gnat_field), NULL_TREE, false);
}

/* Associate to the implementation type of a packed array type specified by
   GNU_TYPE, which is the translation of GNAT_ENTITY, the original array type
   if it has been translated.  This association is a parallel type for GNAT
   encodings or a debug type for standard DWARF.  Note that for standard DWARF,
   we also want to get the original type name and therefore we return it.  */

static tree
associate_original_type_to_packed_array (tree gnu_type, Entity_Id gnat_entity)
{
  const Entity_Id gnat_original_array_type
    = Underlying_Type (Original_Array_Type (gnat_entity));
  tree gnu_original_array_type;

  if (!present_gnu_tree (gnat_original_array_type))
    return NULL_TREE;

  gnu_original_array_type = gnat_to_gnu_type (gnat_original_array_type);

  if (TYPE_IS_DUMMY_P (gnu_original_array_type))
    return NULL_TREE;

  gcc_assert (TYPE_IMPL_PACKED_ARRAY_P (gnu_type));

  if (gnat_encodings == DWARF_GNAT_ENCODINGS_ALL)
    {
      add_parallel_type (gnu_type, gnu_original_array_type);
      return NULL_TREE;
    }
  else
    {
      SET_TYPE_ORIGINAL_PACKED_ARRAY (gnu_type, gnu_original_array_type);

      tree original_name = TYPE_NAME (gnu_original_array_type);
      if (TREE_CODE (original_name) == TYPE_DECL)
	original_name = DECL_NAME (original_name);
      return original_name;
    }
}

/* Given a type T, a FIELD_DECL F, and a replacement value R, return an
   equivalent type with adjusted size expressions where all occurrences
   of references to F in a PLACEHOLDER_EXPR have been replaced by R.

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
    case METHOD_TYPE:
      /* These should never show up here.  */
      gcc_unreachable ();

    case ARRAY_TYPE:
      {
	tree component = substitute_in_type (TREE_TYPE (t), f, r);
	tree domain = substitute_in_type (TYPE_DOMAIN (t), f, r);

	if (component == TREE_TYPE (t) && domain == TYPE_DOMAIN (t))
	  return t;

	nt = build_nonshared_array_type (component, domain);
	SET_TYPE_ALIGN (nt, TYPE_ALIGN (t));
	TYPE_USER_ALIGN (nt) = TYPE_USER_ALIGN (t);
	SET_TYPE_MODE (nt, TYPE_MODE (t));
	TYPE_SIZE (nt) = SUBSTITUTE_IN_EXPR (TYPE_SIZE (t), f, r);
	TYPE_SIZE_UNIT (nt) = SUBSTITUTE_IN_EXPR (TYPE_SIZE_UNIT (t), f, r);
	TYPE_MULTI_ARRAY_P (nt) = TYPE_MULTI_ARRAY_P (t);
	TYPE_CONVENTION_FORTRAN_P (nt) = TYPE_CONVENTION_FORTRAN_P (t);
	if (TYPE_REVERSE_STORAGE_ORDER (t))
	  set_reverse_storage_order_on_array_type (nt);
	if (TYPE_NONALIASED_COMPONENT (t))
	  set_nonaliased_component_on_array_type (nt);
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

  /* If the type contains a template, return the padded size of the template
     plus the RM size of the actual data.  */
  if (TREE_CODE (gnu_type) == RECORD_TYPE
      && TYPE_CONTAINS_TEMPLATE_P (gnu_type))
    return
      size_binop (PLUS_EXPR,
		  bit_position (DECL_CHAIN (TYPE_FIELDS (gnu_type))),
		  rm_size (TREE_TYPE (DECL_CHAIN (TYPE_FIELDS (gnu_type)))));

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

static const char *
get_entity_char (Entity_Id gnat_entity)
{
  Get_Encoded_Name (gnat_entity);
  return ggc_strdup (Name_Buffer);
}

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
  const Entity_Kind kind = Ekind (gnat_entity);
  const bool has_suffix = (suffix != NULL);
  String_Template temp = {1, has_suffix ? (int) strlen (suffix) : 0};
  String_Pointer sp = {suffix, &temp};

  Get_External_Name (gnat_entity, has_suffix, sp);

  /* A variable using the Stdcall convention lives in a DLL.  We adjust
     its name to use the jump table, the _imp__NAME contains the address
     for the NAME variable.  */
  if ((kind == E_Variable || kind == E_Constant)
      && Has_Stdcall_Convention (gnat_entity))
    {
      const int len = strlen (STDCALL_PREFIX) + Name_Len;
      char *new_name = (char *) alloca (len + 1);
      strcpy (new_name, STDCALL_PREFIX);
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

/* Initialize the data structures of the decl.c module.  */

void
init_gnat_decl (void)
{
  /* Initialize the cache of annotated values.  */
  annotate_value_cache = hash_table<value_annotation_hasher>::create_ggc (512);

  /* Initialize the association of dummy types with subprograms.  */
  dummy_to_subprog_map = hash_table<dummy_type_hasher>::create_ggc (512);
}

/* Destroy the data structures of the decl.c module.  */

void
destroy_gnat_decl (void)
{
  /* Destroy the cache of annotated values.  */
  annotate_value_cache->empty ();
  annotate_value_cache = NULL;

  /* Destroy the association of dummy types with subprograms.  */
  dummy_to_subprog_map->empty ();
  dummy_to_subprog_map = NULL;
}

#include "gt-ada-decl.h"
