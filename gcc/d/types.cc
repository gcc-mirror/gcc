/* types.cc -- Lower D frontend types to GCC trees.
   Copyright (C) 2006-2025 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "dmd/attrib.h"
#include "dmd/aggregate.h"
#include "dmd/enum.h"
#include "dmd/expression.h"
#include "dmd/identifier.h"
#include "dmd/mtype.h"
#include "dmd/target.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "tm.h"
#include "function.h"
#include "toplev.h"
#include "target.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "attribs.h"

#include "d-tree.h"
#include "d-target.h"


/* Return the signed or unsigned version of TYPE, an integral type, the
   signedness being specified by UNSIGNEDP.  */

static tree
d_signed_or_unsigned_type (int unsignedp, tree type)
{
  if (VECTOR_TYPE_P (type) || !ANY_INTEGRAL_TYPE_P (type))
    return signed_or_unsigned_type_for (unsignedp, type);

  if (TYPE_PRECISION (type) == TYPE_PRECISION (d_cent_type))
    return unsignedp ? d_ucent_type : d_cent_type;

  if (TYPE_PRECISION (type) == TYPE_PRECISION (d_long_type))
    return unsignedp ? d_ulong_type : d_long_type;

  if (TYPE_PRECISION (type) == TYPE_PRECISION (d_int_type))
    return unsignedp ? d_uint_type : d_int_type;

  if (TYPE_PRECISION (type) == TYPE_PRECISION (d_short_type))
    return unsignedp ? d_ushort_type : d_short_type;

  if (TYPE_PRECISION (type) == TYPE_PRECISION (d_byte_type))
    return unsignedp ? d_ubyte_type : d_byte_type;

  return signed_or_unsigned_type_for (unsignedp, type);
}

/* Return the unsigned version of TYPE, an integral type.  */

tree
d_unsigned_type (tree type)
{
  return d_signed_or_unsigned_type (1, type);
}

/* Return the signed version of TYPE, an integral type.  */

tree
d_signed_type (tree type)
{
  return d_signed_or_unsigned_type (0, type);
}

/* Return TRUE if TYPE is a static array va_list.  This is for compatibility
   with the C ABI, where va_list static arrays are passed by reference.
   However for every other case in D, static arrays are passed by value.  */

bool
valist_array_p (Type *type)
{
  Type *tvalist = target.va_listType (Loc (), NULL);
  if (tvalist->ty == TY::Tsarray)
    {
      Type *tb = type->toBasetype ();
      if (same_type_p (tb, tvalist))
	return true;
    }

  return false;
}

/* Returns true if TYPE contains no actual data, just various
   possible combinations of empty aggregates.  */

bool
empty_aggregate_p (tree type)
{
  if (!AGGREGATE_TYPE_P (type))
    return false;

  /* Want the element type for arrays.  */
  if (TREE_CODE (type) == ARRAY_TYPE)
    return empty_aggregate_p (TREE_TYPE (type));

  /* Recursively check all fields.  */
  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      if (TREE_CODE (field) == FIELD_DECL
	  && !empty_aggregate_p (TREE_TYPE (field)))
	return false;
    }

  return true;
}

/* Returns true if T1 and T2 are related to each other.  */

bool
same_type_p (Type *t1, Type *t2)
{
  /* Types are equal.  */
  if (t1 == t2)
    return true;

  /* Types derive from the same base.  */
  Type *tb1 = t1->toBasetype ();
  Type *tb2 = t2->toBasetype ();
  if (tb1 == tb2)
    return true;

  /* Types are mutably the same type.  */
  if (tb1->ty == tb2->ty && dmd::equivalent (tb1, tb2))
    return true;

  return false;
}

/* Returns `Object' type which all D classes are derived from.  */

Type *
get_object_type (void)
{
  if (ClassDeclaration::object)
    return ClassDeclaration::object->type;

  error ("missing or corrupt object.d");
  return Type::terror;
}


/* Returns a static array of TYPE which has SIZE number of elements.  */

tree
make_array_type (Type *type, unsigned HOST_WIDE_INT size)
{
  /* In [arrays/void-arrays], void arrays can also be static, the length is
     specified in bytes.  */
  if (type->toBasetype ()->ty == TY::Tvoid)
    type = Type::tuns8;

  /* In [arrays/static-arrays], a static array with a dimension of 0 is allowed,
     but no space is allocated for it.  */
  if (size == 0)
    {
      tree range = lang_hooks.types.type_for_size (TYPE_PRECISION (sizetype),
						   TYPE_UNSIGNED (sizetype));
      tree index = build_range_type (range, size_zero_node, NULL_TREE);

      tree t = build_array_type (build_ctype (type), index);
      TYPE_SIZE (t) = bitsize_zero_node;
      TYPE_SIZE_UNIT (t) = size_zero_node;
      return t;
    }

  tree t = build_array_type (build_ctype (type),
			     build_index_type (size_int (size - 1)));
  /* Propagate TREE_ADDRESSABLE to the static array type.  */
  TREE_ADDRESSABLE (t) = TREE_ADDRESSABLE (TREE_TYPE (t));
  return t;
}

/* Builds a record type whose name is NAME.  NFIELDS is the number of fields,
   provided as field ident/type pairs.  */

tree
make_struct_type (const char *name, int nfields, ...)
{
  tree fields = NULL_TREE;
  va_list ap;

  va_start (ap, nfields);

  for (int i = 0; i < nfields; i++)
    {
      tree ident = va_arg (ap, tree);
      tree type = va_arg (ap, tree);
      tree field = build_decl (BUILTINS_LOCATION, FIELD_DECL, ident, type);
      DECL_CHAIN (field) = fields;
      fields = field;
    }

  va_end (ap);

  tree type = make_node (RECORD_TYPE);
  finish_builtin_struct (type, name, fields, NULL_TREE);

  return type;
}

/* Return qualified type variant of TYPE determined by modifier value MOD.  */

tree
insert_type_modifiers (tree type, unsigned mod)
{
  int quals = 0;

  switch (mod)
    {
    case MODconst:
    case MODwild:
    case MODwildconst:
    case MODimmutable:
    case MODshared | MODconst:
    case MODshared | MODwild:
    case MODshared | MODwildconst:
      quals |= TYPE_QUAL_CONST;
      break;

    case 0:
    case MODshared:
      break;

    default:
      gcc_unreachable ();
    }

  tree qualtype = build_qualified_type (type, quals);

  /* Mark whether the type is qualified `shared'.  */
  if (mod & MODshared)
    TYPE_SHARED (qualtype) = 1;

  return qualtype;
}

/* Adds FIELD into the aggregate TYPE at OFFSET.  */

void
insert_aggregate_field (tree type, tree field, size_t offset)
{
  DECL_FIELD_CONTEXT (field) = type;
  SET_DECL_OFFSET_ALIGN (field, TYPE_ALIGN (TREE_TYPE (field)));
  DECL_FIELD_OFFSET (field) = size_int (offset);
  DECL_FIELD_BIT_OFFSET (field) = bitsize_zero_node;

  TREE_ADDRESSABLE (field) = TYPE_SHARED (TREE_TYPE (field));

  TYPE_FIELDS (type) = chainon (TYPE_FIELDS (type), field);
}

/* Build a bit-field integer type for the given WIDTH and UNSIGNEDP.  */

static tree
d_build_bitfield_integer_type (unsigned HOST_WIDE_INT width, int unsignedp)
{
  /* Same as d_type_for_size, but uses exact match for size.  */
  if (width == TYPE_PRECISION (d_byte_type))
    return unsignedp ? d_ubyte_type : d_byte_type;

  if (width == TYPE_PRECISION (d_short_type))
    return unsignedp ? d_ushort_type : d_short_type;

  if (width == TYPE_PRECISION (d_int_type))
    return unsignedp ? d_uint_type : d_int_type;

  if (width == TYPE_PRECISION (d_long_type))
    return unsignedp ? d_ulong_type : d_long_type;

  if (width == TYPE_PRECISION (d_cent_type))
    return unsignedp ? d_ucent_type : d_cent_type;

  for (int i = 0; i < NUM_INT_N_ENTS; i ++)
    {
      if (int_n_enabled_p[i] && width == int_n_data[i].bitsize)
	{
	  if (unsignedp)
	    return int_n_trees[i].unsigned_type;
	  else
	    return int_n_trees[i].signed_type;
	}
    }

  return build_nonstandard_integer_type (width, unsignedp);
}

/* Adds BITFIELD into the aggregate TYPE at OFFSET+BITOFFSET.  */

static void
insert_aggregate_bitfield (tree type, tree bitfield, size_t width,
			   size_t offset, size_t bitoffset)
{
  DECL_FIELD_CONTEXT (bitfield) = type;
  SET_DECL_OFFSET_ALIGN (bitfield, TYPE_ALIGN (TREE_TYPE (bitfield)));
  DECL_SIZE (bitfield) = bitsize_int (width);
  DECL_FIELD_OFFSET (bitfield) = size_int (offset);
  DECL_FIELD_BIT_OFFSET (bitfield) = bitsize_int (bitoffset);

  TREE_ADDRESSABLE (bitfield) = TYPE_SHARED (TREE_TYPE (bitfield));

  DECL_BIT_FIELD (bitfield) = 1;
  DECL_BIT_FIELD_TYPE (bitfield) = TREE_TYPE (bitfield);

  TYPE_FIELDS (type) = chainon (TYPE_FIELDS (type), bitfield);
}

/* For all decls in the FIELDS chain, adjust their field offset by OFFSET.
   This is done as the frontend puts fields into the outer struct, and so
   their offset is from the beginning of the aggregate.
   We want the offset to be from the beginning of the anonymous aggregate.  */

static void
fixup_anonymous_offset (tree fields, tree offset)
{
  /* No adjustment in field offset required.  */
  if (integer_zerop (offset))
    return;

  while (fields != NULL_TREE)
    {
      /* Traverse all nested anonymous aggregates to update the offset of their
	 fields.  Note that the anonymous field itself is not adjusted, as it
	 already has an offset relative to its outer aggregate.  */
      tree ftype = TREE_TYPE (fields);
      if (TYPE_NAME (ftype) && IDENTIFIER_ANON_P (TYPE_IDENTIFIER (ftype)))
	{
	  tree vfields = TYPE_FIELDS (ftype);
	  fixup_anonymous_offset (vfields, offset);
	}
      else
	{
	  tree voffset = DECL_FIELD_OFFSET (fields);
	  DECL_FIELD_OFFSET (fields) = size_binop (MINUS_EXPR, voffset, offset);
	}

      fields = DECL_CHAIN (fields);
    }
}

/* Iterate over all MEMBERS of an aggregate, and add them as fields to CONTEXT.
   If INHERITED_P is true, then the members derive from a base class.
   Returns the number of named fields found.  */

static size_t
layout_aggregate_members (Dsymbols *members, tree context, bool inherited_p)
{
  size_t fields = 0;

  for (size_t i = 0; i < members->length; i++)
    {
      Dsymbol *sym = (*members)[i];
      VarDeclaration *var = sym->isVarDeclaration ();
      if (var != NULL)
	{
	  /* Skip fields that have already been added.  */
	  if (!inherited_p && var->csym != NULL)
	    continue;

	  /* If this variable was really a tuple, add all tuple fields.  */
	  if (var->aliasTuple)
	    {
	      TupleDeclaration *td = var->aliasTuple;
	      Dsymbols tmembers;
	      /* No other way to coerce the underlying type out of the tuple.
		 Frontend should have already validated this.  */
	      for (size_t j = 0; j < td->objects->length; j++)
		{
		  RootObject *ro = (*td->objects)[j];
		  gcc_assert (ro->dyncast () == DYNCAST_EXPRESSION);
		  Expression *e = (Expression *) ro;
		  gcc_assert (e->op == EXP::variable);
		  VarExp *ve = e->isVarExp ();

		  tmembers.push (ve->var);
		}

	      fields += layout_aggregate_members (&tmembers, context,
						  inherited_p);
	      continue;
	    }

	  /* Insert the field declaration at its given offset.  */
	  if (var->isField ())
	    {
	      const char *ident = (var->ident && !var->ident->isAnonymous ())
		? var->ident->toChars () : NULL;
	      tree field = create_field_decl (declaration_type (var), ident,
					      inherited_p, inherited_p);
	      apply_user_attributes (var, field);

	      if (BitFieldDeclaration *bf = var->isBitFieldDeclaration ())
		{
		  /* Bit-fields come from an ImportC context, and require the
		     field be correctly adjusted.  */
		  insert_aggregate_bitfield (context, field, bf->fieldWidth,
					     bf->offset, bf->bitOffset);
		}
	      else
		insert_aggregate_field (context, field, var->offset);

	      /* Because the front-end shares field decls across classes, don't
		 create the corresponding back-end symbol unless we are adding
		 it to the aggregate it is defined in.  */
	      if (!inherited_p)
		{
		  DECL_LANG_SPECIFIC (field) = build_lang_decl (var);
		  var->csym = field;
		}

	      /* Only count the named fields in an aggregate.  */
	      if (ident != NULL)
		fields += 1;

	      continue;
	    }
	}

      /* Anonymous struct/union are flattened by the frontend.  However, we
	 want to keep the record layout in-tact when building the type.  */
      AnonDeclaration *ad = sym->isAnonDeclaration ();
      if (ad != NULL)
	{
	  tree ident = make_anon_name ();
	  tree type = make_node (ad->isunion ? UNION_TYPE : RECORD_TYPE);
	  ANON_AGGR_TYPE_P (type) = 1;
	  d_keep (type);

	  /* Build the type declaration.  */
	  tree decl = build_decl (make_location_t (ad->loc),
				  TYPE_DECL, ident, type);
	  DECL_CONTEXT (decl) = context;
	  DECL_ARTIFICIAL (decl) = 1;

	  TYPE_CONTEXT (type) = context;
	  TYPE_NAME (type) = decl;
	  TYPE_STUB_DECL (type) = decl;

	  /* Recursively iterator over the anonymous members.  */
	  fields += layout_aggregate_members (ad->decl, type, inherited_p);

	  /* Remove from the anon fields the base offset of this anonymous
	     aggregate.  Undoes what is set-up in setFieldOffset, but doesn't
	     affect field accesses.  */
	  tree offset = size_int (ad->anonoffset);
	  fixup_anonymous_offset (TYPE_FIELDS (type), offset);

	  finish_aggregate_type (ad->anonstructsize, ad->anonalignsize, type);

	  /* And make the corresponding data member.  */
	  tree field = create_field_decl (type, NULL, 0, 0);
	  apply_user_attributes (ad, field);
	  insert_aggregate_field (context, field, ad->anonoffset);
	  continue;
	}

      /* Other kinds of attributes don't create a scope.  */
      AttribDeclaration *attrib = sym->isAttribDeclaration ();
      if (attrib != NULL)
	{
	  Dsymbols *decls = dmd::include (attrib, NULL);
	  if (decls != NULL)
	    {
	      fields += layout_aggregate_members (decls, context, inherited_p);
	      continue;
	    }
	}

      /* Same with template mixins and namespaces.  */
      if (sym->isTemplateMixin () || sym->isNspace ())
	{
	  ScopeDsymbol *scopesym = sym->isScopeDsymbol ();
	  if (scopesym->members)
	    {
	      fields += layout_aggregate_members (scopesym->members, context,
						  inherited_p);
	      continue;
	    }
	}
    }

  return fields;
}

/* Write out all fields for aggregate BASE.  For classes, write out all
   interfaces first, then the base class fields.  */

static void
layout_aggregate_type (AggregateDeclaration *decl, tree type,
		       AggregateDeclaration *base)
{
  ClassDeclaration *cd = base->isClassDeclaration ();
  bool inherited_p = (decl != base);

  if (cd != NULL)
    {
      if (cd->baseClass)
	layout_aggregate_type (decl, type, cd->baseClass);
      else
	{
	  /* This is the base class (Object) or interface.  */
	  tree objtype = TREE_TYPE (build_ctype (cd->type));

	  /* Add the vtable pointer, and optionally the monitor fields.  */
	  InterfaceDeclaration *id = cd->isInterfaceDeclaration ();
	  if (!id || id->vtblInterfaces->length == 0)
	    {
	      tree field = create_field_decl (vtbl_ptr_type_node, "__vptr", 1,
					      inherited_p);
	      DECL_VIRTUAL_P (field) = 1;
	      TYPE_VFIELD (type) = field;
	      DECL_FCONTEXT (field) = objtype;
	      insert_aggregate_field (type, field, 0);
	    }

	  if (!id && cd->hasMonitor ())
	    {
	      tree field = create_field_decl (ptr_type_node, "__monitor", 1,
					      inherited_p);
	      insert_aggregate_field (type, field, target.ptrsize);
	    }
	}

      if (cd->vtblInterfaces)
	{
	  for (size_t i = 0; i < cd->vtblInterfaces->length; i++)
	    {
	      BaseClass *bc = (*cd->vtblInterfaces)[i];
	      tree field = create_field_decl (vtbl_ptr_type_node, NULL, 1, 1);
	      insert_aggregate_field (type, field, bc->offset);
	    }
	}
    }

  if (base->members)
    {
      size_t fields = layout_aggregate_members (base->members, type,
						inherited_p);
      gcc_assert (fields == base->fields.length);

      /* Make sure that all fields have been created.  */
      if (!inherited_p)
	{
	  for (size_t i = 0; i < base->fields.length; i++)
	    {
	      VarDeclaration *var = base->fields[i];
	      gcc_assert (var->csym != NULL);
	    }
	}
    }
}

/* Given a record type TYPE compute the finalized record mode if all fields have
   had their types resolved and sizes determined.  */

void
finish_aggregate_mode (tree type)
{
  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      /* Fields of type `typeof(*null)' have no size, so let them force the
	 record type mode to be computed as BLKmode.  */
      if (TYPE_MAIN_VARIANT (TREE_TYPE (field)) == noreturn_type_node)
	break;

      if (DECL_SIZE (field) == NULL_TREE)
	return;
    }

  compute_record_mode (type);

  /* Propagate computed mode to all variants of this aggregate type.  */
  for (tree t = TYPE_MAIN_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
    {
      if (t == type)
	continue;

      SET_TYPE_MODE (t, TYPE_MODE (type));
    }
}

/* If the aggregate type TYPE completes the type of any previous field
   declarations, lay them out now.  */

static void
finish_incomplete_fields (tree type)
{
  for (tree fwdref = TYPE_FORWARD_REFERENCES (type); fwdref != NULL_TREE;
       fwdref = TREE_CHAIN (fwdref))
    {
      tree field = TREE_VALUE (fwdref);
      tree basetype = TREE_TYPE (field);

      /* Arrays of TYPE have layout_type() called from build_array_type(), but
	 would skip over setting TYPE_SIZE. Try completing the type again.  */
      if (TREE_CODE (basetype) == ARRAY_TYPE)
	{
	  while (TREE_CODE (TREE_TYPE (basetype)) == ARRAY_TYPE)
	    basetype = TREE_TYPE (basetype);

	  layout_type (basetype);
	}

      relayout_decl (field);

      /* Relayout of field may change the mode of its RECORD_TYPE.  */
      finish_aggregate_mode (DECL_FIELD_CONTEXT (field));
    }

  /* No more forward references to process.  */
  TYPE_FORWARD_REFERENCES (type) = NULL_TREE;
}

/* Given a record type TYPE, whose size and alignment are determined by
   STRUCTSIZE and ALIGNSIZE.  Apply any type attributes ATTRS and compute
   the finalized record mode.  */

void
finish_aggregate_type (unsigned structsize, unsigned alignsize, tree type)
{
  /* Set size and alignment as requested by frontend.  */
  TYPE_SIZE (type) = bitsize_int (structsize * BITS_PER_UNIT);
  TYPE_SIZE_UNIT (type) = size_int (structsize);
  SET_TYPE_ALIGN (type, alignsize * BITS_PER_UNIT);
  TYPE_PACKED (type) = (alignsize == 1);

  /* Layout all fields now the type is complete.  */
  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      /* If the field type is still being constructed because of recursive
	 references, attach it to that class/struct type, so we can go back
	 and complete the field later.  */
      if (!COMPLETE_TYPE_P (TREE_TYPE (field)))
	{
	  tree basetype = TREE_TYPE (field);
	  while (TREE_CODE (basetype) == ARRAY_TYPE)
	    basetype = TREE_TYPE (basetype);

	  basetype = TYPE_MAIN_VARIANT (basetype);
	  if (RECORD_OR_UNION_TYPE_P (basetype)
	      || TREE_CODE (basetype) == ENUMERAL_TYPE)
	    {
	      gcc_assert (!COMPLETE_TYPE_P (basetype));
	      tree fwdrefs = tree_cons (NULL_TREE, field,
					TYPE_FORWARD_REFERENCES (basetype));
	      TYPE_FORWARD_REFERENCES (basetype) = fwdrefs;
	    }

	  continue;
	}

      layout_decl (field, 0);

      /* Give bit-field its proper type after layout_decl.  */
      if (DECL_BIT_FIELD (field))
	{
	  tree orig_type = DECL_BIT_FIELD_TYPE (field);
	  unsigned HOST_WIDE_INT width = tree_to_uhwi (DECL_SIZE (field));

	  if (width != TYPE_PRECISION (orig_type))
	    {
	      bool unsignedp = TYPE_UNSIGNED (orig_type);

	      TREE_TYPE (field)
		= d_build_bitfield_integer_type (width, unsignedp);
	      SET_DECL_MODE (field, TYPE_MODE (TREE_TYPE (field)));
	    }
	}
    }

  /* Set the back-end type mode after all fields have had their size set.  */
  finish_aggregate_mode (type);

  /* Fix up all forward-referenced variants of this aggregate type.  */
  for (tree t = TYPE_MAIN_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
    {
      if (t == type)
	continue;

      TYPE_FIELDS (t) = TYPE_FIELDS (type);
      TYPE_LANG_SPECIFIC (t) = TYPE_LANG_SPECIFIC (type);
      TYPE_SIZE (t) = TYPE_SIZE (type);
      TYPE_SIZE_UNIT (t) = TYPE_SIZE_UNIT (type);
      TYPE_PACKED (type) = TYPE_PACKED (type);
      SET_TYPE_ALIGN (t, TYPE_ALIGN (type));
      TYPE_USER_ALIGN (t) = TYPE_USER_ALIGN (type);
    }

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (type, TYPE_FILE_SCOPE_P (type));
  finish_incomplete_fields (type);

  /* Finish processing of TYPE_DECL.  */
  rest_of_decl_compilation (TYPE_NAME (type),
			    DECL_FILE_SCOPE_P (TYPE_NAME (type)), 0);
}

/* Returns true if the class or struct type TYPE has already been layed out by
   the lowering of another front-end AST type.  In which case, there will either
   be a reuse of the back-end type, or a multiple definition error.
   DECO is the uniquely mangled decoration for the type.  */

static bool
merge_aggregate_types (Type *type, tree deco)
{
  AggregateDeclaration *sym;

  if (type->ty == TY::Tstruct)
    sym = type->isTypeStruct ()->sym;
  else if (type->ty == TY::Tclass)
    sym = type->isTypeClass ()->sym;
  else
    gcc_unreachable ();

  if (IDENTIFIER_DAGGREGATE (deco))
    {
      AggregateDeclaration *ad = IDENTIFIER_DAGGREGATE (deco);
      /* There should never be a class/struct mismatch in mangled names.  */
      gcc_assert ((sym->isStructDeclaration () && ad->isStructDeclaration ())
		  || (sym->isClassDeclaration () && ad->isClassDeclaration ()));

      /* Non-templated variables shouldn't be defined twice.  */
      if (!sym->isInstantiated ())
	ScopeDsymbol::multiplyDefined (sym->loc, sym, ad);

      type->ctype = build_ctype (ad->type);
      return true;
    }

  return false;
}

/* Implements the visitor interface to build the GCC trees of all
   Type AST classes emitted from the D Front-end, where CTYPE holds
   the cached back-end representation to be returned.  */

class TypeVisitor : public Visitor
{
  using Visitor::visit;

public:
  TypeVisitor (void)
  {
  }

  /* This should be overridden by each type class.  */

  void visit (Type *) final override
  {
    gcc_unreachable ();
  }

  /* Type assigned to erroneous expressions or constructs that
     failed during the semantic stage.  */

  void visit (TypeError *t) final override
  {
    t->ctype = error_mark_node;
  }

  /* Type assigned to generic nullable types.  */

  void visit (TypeNull *t) final override
  {
    t->ctype = ptr_type_node;
  }

  /* Bottom type used for functions that never return.  */

  void visit (TypeNoreturn *t) final override
  {
    t->ctype = noreturn_type_node;
    TYPE_NAME (t->ctype) = get_identifier (t->toChars ());
  }

  /* Basic Data Types.  */

  void visit (TypeBasic *t) final override
  {
    /* [type/basic-data-types]

       void	no type.
       bool	8-bit boolean value.
       byte	8-bit signed value.
       ubyte	8-bit unsigned value.
       short	16-bit signed value.
       ushort	16-bit unsigned value.
       int	32-bit signed value.
       uint	32-bit unsigned value.
       long	64-bit signed value.
       ulong	64-bit unsigned value.
       cent	128-bit signed value.
       ucent	128-bit unsigned value.
       float	32-bit IEEE 754 floating-point value.
       double	64-bit IEEE 754 floating-point value.
       real	largest FP size implemented in hardware.
       ifloat	imaginary float.
       idouble	imaginary double.
       ireal	imaginary real.
       cfloat	complex float.
       cdouble	complex double.
       creal	complex real.
       char	UTF-8 code unit.
       wchar	UTF-16 code unit.
       dchar	UTF-32 code unit.  */

    switch (t->ty)
      {
      case TY::Tvoid:	     t->ctype = void_type_node; break;
      case TY::Tbool:	     t->ctype = d_bool_type; break;
      case TY::Tint8:	     t->ctype = d_byte_type; break;
      case TY::Tuns8:	     t->ctype = d_ubyte_type; break;
      case TY::Tint16:	     t->ctype = d_short_type; break;
      case TY::Tuns16:	     t->ctype = d_ushort_type; break;
      case TY::Tint32:	     t->ctype = d_int_type; break;
      case TY::Tuns32:	     t->ctype = d_uint_type; break;
      case TY::Tint64:	     t->ctype = d_long_type; break;
      case TY::Tuns64:	     t->ctype = d_ulong_type; break;
      case TY::Tint128:	     t->ctype = d_cent_type; break;
      case TY::Tuns128:	     t->ctype = d_ucent_type; break;
      case TY::Tfloat32:     t->ctype = float_type_node; break;
      case TY::Tfloat64:     t->ctype = double_type_node; break;
      case TY::Tfloat80:     t->ctype = long_double_type_node; break;
      case TY::Timaginary32: t->ctype = ifloat_type_node; break;
      case TY::Timaginary64: t->ctype = idouble_type_node; break;
      case TY::Timaginary80: t->ctype = ireal_type_node; break;
      case TY::Tcomplex32:   t->ctype = complex_float_type_node; break;
      case TY::Tcomplex64:   t->ctype = complex_double_type_node; break;
      case TY::Tcomplex80:   t->ctype = complex_long_double_type_node; break;
      case TY::Tchar:	     t->ctype = char8_type_node; break;
      case TY::Twchar:	     t->ctype = char16_type_node; break;
      case TY::Tdchar:	     t->ctype = char32_type_node; break;
      default:		     gcc_unreachable ();
      }

    TYPE_NAME (t->ctype) = get_identifier (t->toChars ());
  }


  /* Derived Data Types.  */

  /* Build a simple pointer to data type, analogous to C pointers.  */

  void visit (TypePointer *t) final override
  {
    t->ctype = build_pointer_type (build_ctype (t->next));
  }

  /* Build a dynamic array type, consisting of a length and a pointer
     to the array data.  */

  void visit (TypeDArray *t) final override
  {
    /* In [abi/arrays], dynamic array layout is:
	.length	array dimension.
	.ptr	pointer to array data.  */
    t->ctype = make_struct_type (t->toChars (), 2,
				 get_identifier ("length"),
				 build_ctype (Type::tsize_t),
				 get_identifier ("ptr"),
				 build_pointer_type (build_ctype (t->next)));
    TYPE_DYNAMIC_ARRAY (t->ctype) = 1;
    TYPE_LANG_SPECIFIC (t->ctype) = build_lang_type (t);
    d_keep (t->ctype);
  }

  /* Build a static array type, distinguished from dynamic arrays by
     having a length fixed at compile-time, analogous to C arrays.  */

  void visit (TypeSArray *t) final override
  {
    if (t->dim->isConst () && t->dim->type->isIntegral ())
      {
	uinteger_t size = t->dim->toUInteger ();
	t->ctype = make_array_type (t->next, size);
      }
    else
      {
	error ("invalid expression for static array dimension: %s",
	       t->dim->toChars ());
	gcc_unreachable ();
      }
  }

  /* Build a vector type, a fixed array of floating or integer types.  */

  void visit (TypeVector *t) final override
  {
    int nunits = t->basetype->isTypeSArray ()->dim->toUInteger ();
    tree inner = build_ctype (t->elementType ());

    /* Same rationale as void static arrays.  */
    if (inner == void_type_node)
      inner = build_ctype (Type::tuns8);

    t->ctype = build_vector_type (inner, nunits);
    TYPE_NAME (t->ctype) = get_identifier (t->toChars ());
    layout_type (t->ctype);
  }

  /* Build an associative array type, distinguished from arrays by having an
     index that's not necessarily an integer, and can be sparsely populated.  */

  void visit (TypeAArray *t) final override
  {
    /* In [abi/associative-arrays], associative arrays are a struct that only
       consist of a pointer to an opaque, implementation defined type.  */
    t->ctype = make_struct_type (t->toChars (), 1,
				 get_identifier ("ptr"), ptr_type_node);
    TYPE_ASSOCIATIVE_ARRAY (t->ctype) = 1;
    TYPE_LANG_SPECIFIC (t->ctype) = build_lang_type (t);
    d_keep (t->ctype);
  }

  /* Build type for a function declaration, which consists of a return type,
     and a list of parameter types, and a linkage attribute.  */

  void visit (TypeFunction *t) final override
  {
    tree fnparams = NULL_TREE;
    tree fntype;

    /* [function/variadic]

       Variadic functions with D linkage have an additional hidden argument
       with the name _arguments passed to the function.  */
    if (t->isDstyleVariadic ())
      {
	tree type = build_ctype (Type::typeinfotypelist->type);
	fnparams = chainon (fnparams, build_tree_list (0, type));
      }

    const size_t n_args = t->parameterList.length ();

    for (size_t i = 0; i < n_args; i++)
      {
	tree type = parameter_type (t->parameterList[i]);

	/* Type `noreturn` is a terminator, as no other arguments can possibly
	   be evaluated after it.  */
	if (type == noreturn_type_node)
	  break;

	fnparams = chainon (fnparams, build_tree_list (0, type));
      }

    /* When the last parameter is void_list_node, that indicates a fixed length
       parameter list, otherwise function is treated as variadic.  */
    if (t->parameterList.varargs != VARARGvariadic)
      fnparams = chainon (fnparams, void_list_node);

    if (t->next != NULL)
      {
	fntype = build_ctype (t->next);
	if (t->isRef ())
	  fntype = build_reference_type (fntype);
      }
    else
      fntype = void_type_node;

    /* Could the function type be self referenced by parameters?  */
    t->ctype = build_function_type (fntype, fnparams);
    TYPE_LANG_SPECIFIC (t->ctype) = build_lang_type (t);
    d_keep (t->ctype);

    /* Qualify function types that have the type `noreturn` as volatile.  */
    if (fntype == noreturn_type_node)
      t->ctype = build_qualified_type (t->ctype, TYPE_QUAL_VOLATILE);

    /* Handle any special support for calling conventions.  */
    switch (t->linkage)
      {
      case LINK::windows:
	{
	  /* [attribute/linkage]

	     The Windows convention is distinct from the C convention only
	     on Win32, where it is equivalent to the stdcall convention.  */
	  unsigned link_system, link_windows;
	  if (targetdm.d_has_stdcall_convention (&link_system, &link_windows))
	    {
	      if (link_windows)
		t->ctype = insert_type_attribute (t->ctype, "stdcall");
	    }
	  break;
	}

      case LINK::c:
      case LINK::cpp:
      case LINK::d:
      case LINK::objc:
	/* [abi/function-calling-conventions]

	  The extern (C) and extern (D) calling convention matches
	  the C calling convention used by the supported C compiler
	  on the host system.  */
	break;

      default:
	gcc_unreachable ();
      }
  }

  /* Build a delegate type, an aggregate of two pieces of data, an object
     reference and a pointer to a non-static member function, or a pointer
     to a closure and a pointer to a nested function.  */

  void visit (TypeDelegate *t) final override
  {
    /* In [abi/delegates], delegate layout is:
	.ptr	    context pointer.
	.funcptr    pointer to function.  */
    tree fntype = build_ctype (t->next);
    tree dgtype = build_vthis_function (void_type_node, fntype);

    TYPE_ATTRIBUTES (dgtype) = TYPE_ATTRIBUTES (fntype);
    TYPE_LANG_SPECIFIC (dgtype) = TYPE_LANG_SPECIFIC (fntype);

    t->ctype = make_struct_type (t->toChars (), 2,
				 get_identifier ("ptr"),
				 build_ctype (Type::tvoidptr),
				 get_identifier ("funcptr"),
				 build_pointer_type (dgtype));
    TYPE_DELEGATE (t->ctype) = 1;
    TYPE_LANG_SPECIFIC (t->ctype) = build_lang_type (t);
    d_keep (t->ctype);
  }


  /* User Defined Types.  */

  /* Build a named enum type, a distinct value whose values are restrict to
     a group of constants of the same underlying base type.  */

  void visit (TypeEnum *t) final override
  {
    tree basetype = (t->sym->memtype)
      ? build_ctype (t->sym->memtype) : void_type_node;

    if (t->sym->isSpecial ())
      {
	/* Special enums are opaque types that bind to C types.  */
	const char *ident = t->toChars ();
	Type *underlying = NULL;

	/* Skip over the prefixing `__c_'.  */
	gcc_assert (startswith (ident, "__c_"));
	ident = ident + strlen ("__c_");

	/* To keep things compatible within the code generation we stick to
	   mapping to equivalent D types.  However it should be OK to use the
	   GCC provided C types here as the front-end enforces that everything
	   must be explicitly cast from a D type to any of the opaque types.  */
	if (strcmp (ident, "long") == 0)
	  underlying = build_frontend_type (long_integer_type_node);
	else if (strcmp (ident, "ulong") == 0)
	  underlying = build_frontend_type (long_unsigned_type_node);
	else if (strcmp (ident, "wchar_t") == 0)
	  underlying =
	    build_frontend_type (make_unsigned_type (WCHAR_TYPE_SIZE));
	else if (strcmp (ident, "longlong") == 0)
	  underlying = build_frontend_type (long_long_integer_type_node);
	else if (strcmp (ident, "ulonglong") == 0)
	  underlying = build_frontend_type (long_long_unsigned_type_node);
	else if (strcmp (ident, "long_double") == 0)
	  underlying = build_frontend_type (long_double_type_node);
	else if (strcmp (ident, "complex_real") == 0)
	  underlying = build_frontend_type (complex_long_double_type_node);
	else if (strcmp (ident, "complex_float") == 0)
	  underlying = build_frontend_type (complex_float_type_node);
	else if (strcmp (ident, "complex_double") == 0)
	  underlying = build_frontend_type (complex_double_type_node);

	/* Conversion failed or there's an unhandled special type.  */
	gcc_assert (underlying != NULL);

	t->ctype = build_variant_type_copy (build_ctype (underlying));

	/* When the size of the declared enum base type doesn't match the target
	   C type that this enum is being used as a placeholder for, we can't
	   use the generated underlying type as it'll conflict with all sizes
	   the front-end has computed during semantic.  */
	if (TYPE_SIZE (t->ctype) != TYPE_SIZE (basetype))
	  {
	    warning_at (make_location_t (t->sym->loc),
			OPT_Wmismatched_special_enum,
			"size of %qs (%wd) differ from its declared size (%wd)",
			t->sym->ident->toChars (), int_size_in_bytes (t->ctype),
			int_size_in_bytes (basetype));
	    t->ctype = basetype;
	  }

	build_type_decl (t->ctype, t->sym);
      }
    else if (t->sym->ident == NULL
	     || !INTEGRAL_TYPE_P (basetype)
	     || TREE_CODE (basetype) == BOOLEAN_TYPE)
      {
	/* Enums in D2 can either be anonymous, or have a base type that is not
	   necessarily integral. For these, we simplify this a little by using
	   the base type directly instead of building an ENUMERAL_TYPE.  */
	t->ctype = build_variant_type_copy (basetype);

	if (t->sym->ident != NULL)
	  build_type_decl (t->ctype, t->sym);
      }
    else
      {
	t->ctype = make_node (ENUMERAL_TYPE);
	TYPE_LANG_SPECIFIC (t->ctype) = build_lang_type (t);
	d_keep (t->ctype);

	ENUM_IS_SCOPED (t->ctype) = 1;
	TREE_TYPE (t->ctype) = basetype;

	if (flag_short_enums)
	  TYPE_PACKED (t->ctype) = 1;

	tree values = NULL_TREE;
	if (t->sym->members)
	  {
	    for (size_t i = 0; i < t->sym->members->length; i++)
	      {
		EnumMember *member = (*t->sym->members)[i]->isEnumMember ();
		/* Templated functions can seep through to the back-end
		   just ignore for now.  */
		if (member == NULL)
		  continue;

		tree ident = get_identifier (member->ident->toChars ());
		tree value = build_integer_cst (member->value ()->toInteger (),
						basetype);

		/* Build an identifier for the enumeration constant.  */
		tree decl = build_decl (make_location_t (member->loc),
					CONST_DECL, ident, basetype);
		DECL_CONTEXT (decl) = t->ctype;
		TREE_CONSTANT (decl) = 1;
		TREE_READONLY (decl) = 1;
		DECL_INITIAL (decl) = value;

		/* Add this enumeration constant to the list for this type.  */
		values = chainon (values, build_tree_list (ident, decl));
	      }
	  }

	TYPE_VALUES (t->ctype) = values;
	build_type_decl (t->ctype, t->sym);
      }

    apply_user_attributes (t->sym, t->ctype);

    /* Finish the enumeration type.  */
    if (TREE_CODE (t->ctype) == ENUMERAL_TYPE)
      {
	TYPE_MIN_VALUE (t->ctype) = TYPE_MIN_VALUE (basetype);
	TYPE_MAX_VALUE (t->ctype) = TYPE_MAX_VALUE (basetype);
	TYPE_UNSIGNED (t->ctype) = TYPE_UNSIGNED (basetype);
	SET_TYPE_ALIGN (t->ctype, TYPE_ALIGN (basetype));
	TYPE_SIZE (t->ctype) = NULL_TREE;
	TYPE_PRECISION (t->ctype) = dmd::size (t, t->sym->loc) * 8;

	layout_type (t->ctype);

	/* Finish debugging output for this type.  */
	rest_of_type_compilation (t->ctype, TYPE_FILE_SCOPE_P (t->ctype));
	finish_incomplete_fields (t->ctype);

	/* Finish processing of TYPE_DECL.  */
	rest_of_decl_compilation (TYPE_NAME (t->ctype),
				  DECL_FILE_SCOPE_P (TYPE_NAME (t->ctype)), 0);
      }
  }

  /* Build a struct or union type.  Layout should be exactly represented
     as an equivalent C struct, except for non-POD or nested structs.  */

  void visit (TypeStruct *t) final override
  {
    /* Merge types in the back-end if the front-end did not itself do so.  */
    tree deco = get_identifier (d_mangle_decl (t->sym));
    if (merge_aggregate_types (t, deco))
      return;

    /* Need to set this right away in case of self-references.  */
    t->ctype = make_node (t->sym->isUnionDeclaration ()
			  ? UNION_TYPE : RECORD_TYPE);
    d_keep (t->ctype);
    IDENTIFIER_DAGGREGATE (deco) = t->sym;

    TYPE_LANG_SPECIFIC (t->ctype) = build_lang_type (t);
    TYPE_CXX_ODR_P (t->ctype) = 1;

    if (t->sym->members)
      {
	/* Must set up the overall size and alignment before determining
	   the context or laying out fields as those types may make
	   recursive references to this type.  */
	unsigned structsize = t->sym->structsize;
	unsigned alignsize = t->sym->alignment.isDefault ()
	  ? t->sym->alignsize : t->sym->alignment.get ();

	/* Put out all fields.  */
	layout_aggregate_type (t->sym, t->ctype, t->sym);
	build_type_decl (t->ctype, t->sym);
	set_visibility_for_decl (t->ctype, t->sym);
	apply_user_attributes (t->sym, t->ctype);
	finish_aggregate_type (structsize, alignsize, t->ctype);
      }
    else
      {
	build_type_decl (t->ctype, t->sym);
	apply_user_attributes (t->sym, t->ctype);
      }

    /* For structs with a user defined postblit, copy constructor, or a
       destructor, also set TREE_ADDRESSABLE on the type and all variants.
       This will make the struct be passed around by reference.  */
    if (!t->sym->isPOD ())
      {
	for (tree tv = t->ctype; tv != NULL_TREE; tv = TYPE_NEXT_VARIANT (tv))
	  {
	    TREE_ADDRESSABLE (tv) = 1;
	    SET_TYPE_MODE (tv, BLKmode);
	  }
      }
  }

  /* Build a class type.  Whereas structs are value types, classes are
     reference types, with all the object-orientated features.  */

  void visit (TypeClass *t) final override
  {
    /* Merge types in the back-end if the front-end did not itself do so.  */
    tree deco = get_identifier (d_mangle_decl (t->sym));
    if (merge_aggregate_types (t, deco))
      return;

    /* Need to set ctype right away in case of self-references to
       the type during this call.  */
    tree basetype = make_node (RECORD_TYPE);
    t->ctype = build_pointer_type (basetype);
    d_keep (t->ctype);
    IDENTIFIER_DAGGREGATE (deco) = t->sym;

    /* Note that lang_specific data is assigned to both the reference
       and the underlying record type.  */
    TYPE_LANG_SPECIFIC (t->ctype) = build_lang_type (t);
    TYPE_LANG_SPECIFIC (basetype) = TYPE_LANG_SPECIFIC (t->ctype);
    CLASS_TYPE_P (basetype) = 1;
    TYPE_CXX_ODR_P (basetype) = 1;

    /* Put out all fields, including from each base class.  */
    layout_aggregate_type (t->sym, basetype, t->sym);
    build_type_decl (basetype, t->sym);
    set_visibility_for_decl (basetype, t->sym);
    apply_user_attributes (t->sym, basetype);
    finish_aggregate_type (t->sym->structsize, t->sym->alignsize, basetype);

    /* Classes only live in memory, so always set the TREE_ADDRESSABLE bit.  */
    for (tree tv = basetype; tv != NULL_TREE; tv = TYPE_NEXT_VARIANT (tv))
      {
	TREE_ADDRESSABLE (tv) = 1;
	SET_TYPE_MODE (tv, BLKmode);
      }

    /* Type is final, there are no derivations.  */
    if (t->sym->storage_class & STCfinal)
      TYPE_FINAL_P (basetype) = 1;

    /* Create BINFO even if debugging is off.  This is needed to keep
       references to inherited types.  */
    if (!t->sym->isInterfaceDeclaration ())
      TYPE_BINFO (basetype) = build_class_binfo (NULL_TREE, t->sym);
    else
      {
	unsigned offset = 0;

	TYPE_BINFO (basetype) = build_interface_binfo (NULL_TREE, t->sym,
						       offset);
      }

    /* Associate all virtual methods with the class too.  */
    for (size_t i = 0; i < t->sym->vtbl.length; i++)
      {
	FuncDeclaration *fd = t->sym->vtbl[i]->isFuncDeclaration ();
	tree method = fd ? get_symbol_decl (fd) : error_mark_node;

	if (!error_operand_p (method)
	    && DECL_CONTEXT (method) == basetype
	    && !chain_member (method, TYPE_FIELDS (basetype)))
	  TYPE_FIELDS (basetype) = chainon (TYPE_FIELDS (basetype), method);
      }
  }
};


/* Build a tree from a frontend Type.  */

tree
build_ctype (Type *t)
{
  if (!t->ctype)
    {
      TypeVisitor v;

      /* Strip const modifiers from type before building.  This is done
	 to ensure that back-end treats e.g: const (T) as a variant of T,
	 and not as two distinct types.  */
      if (t->isNaked ())
	t->accept (&v);
      else
	{
	  Type *tb = dmd::castMod (t, 0);
	  if (!tb->ctype)
	    tb->accept (&v);
	  t->ctype = insert_type_modifiers (tb->ctype, t->mod);
	}
    }

  return t->ctype;
}
