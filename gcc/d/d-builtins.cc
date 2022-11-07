/* d-builtins.cc -- GCC builtins support for D.
   Copyright (C) 2006-2022 Free Software Foundation, Inc.

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
#include "dmd/cond.h"
#include "dmd/declaration.h"
#include "dmd/expression.h"
#include "dmd/identifier.h"
#include "dmd/module.h"
#include "dmd/mtype.h"
#include "dmd/target.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "target.h"
#include "common/common-target.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "builtins.h"

#include "d-tree.h"
#include "d-frontend.h"
#include "d-target.h"


static GTY(()) vec <tree, va_gc> *gcc_builtins_functions = NULL;
static GTY(()) vec <tree, va_gc> *gcc_builtins_types = NULL;

/* Record built-in types and their associated decls for re-use when
   generating the `gcc.builtins' module.  */

struct builtin_data
{
  Type *dtype;
  tree ctype;
  Dsymbol *dsym;

  builtin_data (Type *t, tree c, Dsymbol *d = NULL)
    : dtype(t), ctype(c), dsym(d)
  { }
};

static vec <builtin_data> builtin_converted_decls;

/* Build D frontend type from tree TYPE type given.  This will set the
   back-end type symbol directly for complex types to save build_ctype()
   the work.  For other types, it is not useful or will cause errors, such
   as casting from `C char' to `D char', which also means that `char *`
   needs to be specially handled.  */

Type *
build_frontend_type (tree type)
{
  Type *dtype;
  MOD mod = 0;

  if (TYPE_READONLY (type))
    mod |= MODconst;
  if (TYPE_VOLATILE (type))
    mod |= MODshared;

  /* If we've seen the type before, re-use the converted decl.  */
  unsigned saved_builtin_decls_length = builtin_converted_decls.length ();
  for (size_t i = 0; i < saved_builtin_decls_length; ++i)
    {
      tree t = builtin_converted_decls[i].ctype;
      if (TYPE_MAIN_VARIANT (t) == TYPE_MAIN_VARIANT (type))
	return builtin_converted_decls[i].dtype;
    }

  switch (TREE_CODE (type))
    {
    case POINTER_TYPE:
      dtype = build_frontend_type (TREE_TYPE (type));
      if (dtype)
	{
	  /* Check for char * first.  Needs to be done for chars/string.  */
	  if (TYPE_MAIN_VARIANT (TREE_TYPE (type)) == char_type_node)
	    return Type::tchar->addMod (dtype->mod)->pointerTo ()->addMod (mod);

	  if (dtype->ty == TY::Tfunction)
	    return (TypePointer::create (dtype))->addMod (mod);

	  return dtype->pointerTo ()->addMod (mod);
	}
      break;

    case REFERENCE_TYPE:
      dtype = build_frontend_type (TREE_TYPE (type));
      if (dtype)
	{
	  /* Want to assign ctype directly so that the REFERENCE_TYPE code
	     can be turned into as an `inout' argument.  Can't use pointerTo(),
	     because the returned Type is shared.  */
	  dtype = (TypePointer::create (dtype))->addMod (mod);
	  dtype->ctype = type;
	  builtin_converted_decls.safe_push (builtin_data (dtype, type));
	  return dtype;
	}
      break;

    case BOOLEAN_TYPE:
      /* Should be no need for size checking.  */
      return Type::tbool->addMod (mod);

    case INTEGER_TYPE:
    {
      unsigned size = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (type));
      bool unsignedp = TYPE_UNSIGNED (type);

      /* For now, skip support for cent/ucent until the frontend
	 has better support for handling it.  */
      for (size_t i = (size_t) TY::Tint8; i <= (size_t) TY::Tuns64; i++)
	{
	  dtype = Type::basic[i];

	  /* Search for type matching size and signedness.  */
	  if (unsignedp != dtype->isunsigned ()
	      || size != dtype->size ())
	    continue;

	  return dtype->addMod (mod);
	}
      break;
    }

    case REAL_TYPE:
    {
      unsigned size = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (type));

      for (size_t i = (size_t) TY::Tfloat32; i <= (size_t) TY::Tfloat80; i++)
	{
	  dtype = Type::basic[i];

	  /* Search for type matching size.  */
	  if (dtype->size () != size)
	    continue;

	  return dtype->addMod (mod);
	}
      break;
    }

    case COMPLEX_TYPE:
    {
      unsigned size = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (type));
      for (size_t i = (size_t) TY::Tcomplex32; i <= (size_t) TY::Tcomplex80;
	   i++)
	{
	  dtype = Type::basic[i];

	  /* Search for type matching size.  */
	  if (dtype->size () != size)
	    continue;

	  return dtype->addMod (mod);
	}
      break;
    }

    case VOID_TYPE:
      return Type::tvoid->addMod (mod);

    case ARRAY_TYPE:
      dtype = build_frontend_type (TREE_TYPE (type));
      if (dtype)
	{
	  tree index = TYPE_DOMAIN (type);
	  tree ub = TYPE_MAX_VALUE (index);
	  tree lb = TYPE_MIN_VALUE (index);

	  tree length = fold_build2 (MINUS_EXPR, TREE_TYPE (lb), ub, lb);
	  length = size_binop (PLUS_EXPR, size_one_node,
			       convert (sizetype, length));

	  dtype = dtype->sarrayOf (TREE_INT_CST_LOW (length))->addMod (mod);
	  builtin_converted_decls.safe_push (builtin_data (dtype, type));
	  return dtype;
	}
      break;

    case VECTOR_TYPE:
    {
      unsigned HOST_WIDE_INT nunits;
      if (!TYPE_VECTOR_SUBPARTS (type).is_constant (&nunits))
	break;

      dtype = build_frontend_type (TREE_TYPE (type));
      if (!dtype)
	break;

      dtype = dtype->sarrayOf (nunits)->addMod (mod);
      if (target.isVectorTypeSupported (dtype->size (), dtype->nextOf ()))
	break;

      dtype = (TypeVector::create (dtype))->addMod (mod);
      builtin_converted_decls.safe_push (builtin_data (dtype, type));
      return dtype;
    }

    case RECORD_TYPE:
    {
      Identifier *ident = TYPE_IDENTIFIER (type) ?
	Identifier::idPool (IDENTIFIER_POINTER (TYPE_IDENTIFIER (type))) : NULL;

      /* Neither the `object' and `gcc.builtins' modules will not exist when
	 this is called.  Use a stub `object' module parent in the meantime.
	 If `gcc.builtins' is later imported, the parent will be overridden
	 with the correct module symbol.  */
      static Identifier *object = Identifier::idPool ("object");
      static Module *stubmod = Module::create ("object.d", object, 0, 0);

      StructDeclaration *sdecl = StructDeclaration::create (Loc (), ident,
							    false);
      sdecl->parent = stubmod;
      sdecl->structsize = int_size_in_bytes (type);
      sdecl->alignsize = TYPE_ALIGN_UNIT (type);
      sdecl->alignment.setDefault ();
      sdecl->sizeok = Sizeok::done;
      sdecl->type = (TypeStruct::create (sdecl))->addMod (mod);
      sdecl->type->ctype = type;
      sdecl->type->merge2 ();

      /* Add both named and anonymous fields as members of the struct.
	 Anonymous fields still need a name in D, so call them "__pad%u".  */
      unsigned anonfield_id = 0;
      sdecl->members = d_gc_malloc<Dsymbols> ();

      for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	{
	  Type *ftype = build_frontend_type (TREE_TYPE (field));
	  if (!ftype)
	    {
	      /* Drop any field types that got cached before the conversion
		 of this record type failed.  */
	      builtin_converted_decls.truncate (saved_builtin_decls_length);
	      return NULL;
	    }

	  Identifier *fident;
	  if (DECL_NAME (field) == NULL_TREE)
	    {
	      char name[16];
	      snprintf (name, sizeof (name), "__pad%u", anonfield_id++);
	      fident = Identifier::idPool (name);
	    }
	  else
	    {
	      const char *name = IDENTIFIER_POINTER (DECL_NAME (field));
	      fident = Identifier::idPool (name);
	    }

	  VarDeclaration *vd = VarDeclaration::create (Loc (), ftype, fident,
						       NULL);
	  vd->parent = sdecl;
	  vd->offset = tree_to_uhwi (byte_position (field));
	  vd->semanticRun = PASS::semanticdone;
	  vd->csym = field;
	  sdecl->members->push (vd);
	  sdecl->fields.push (vd);
	}

      dtype = sdecl->type;
      builtin_converted_decls.safe_push (builtin_data (dtype, type, sdecl));
      return dtype;
    }

    case FUNCTION_TYPE:
      dtype = build_frontend_type (TREE_TYPE (type));
      if (dtype)
	{
	  tree parms = TYPE_ARG_TYPES (type);
	  VarArg varargs_p = VARARGvariadic;

	  Parameters *args = d_gc_malloc<Parameters> ();
	  args->reserve (list_length (parms));

	  /* Attempt to convert all parameter types.  */
	  for (tree parm = parms; parm != NULL_TREE; parm = TREE_CHAIN (parm))
	    {
	      tree argtype = TREE_VALUE (parm);
	      if (argtype == void_type_node)
		{
		  varargs_p = VARARGnone;
		  break;
		}

	      StorageClass sc = STCundefined;
	      if (TREE_CODE (argtype) == REFERENCE_TYPE)
		{
		  argtype = TREE_TYPE (argtype);
		  sc |= STCref;
		}

	      Type *targ = build_frontend_type (argtype);
	      if (!targ)
		{
		  /* Drop any parameter types that got cached before the
		     conversion of this function type failed.  */
		  builtin_converted_decls.truncate (saved_builtin_decls_length);
		  return NULL;
		}

	      args->push (Parameter::create (sc, targ, NULL, NULL, NULL));
	    }

	  /* GCC generic and placeholder built-ins are marked as variadic, yet
	     have no named parameters, and so can't be represented in D.  */
	  if (args->length != 0 || varargs_p == VARARGnone)
	    {
	      dtype = TypeFunction::create (args, dtype, varargs_p, LINK::c);
	      return dtype->addMod (mod);
	    }
	}
      break;

    default:
      break;
    }

  return NULL;
}

/* Attempt to convert GCC evaluated CST to a D Frontend Expression.
   LOC is the location in the source file where this CST is being evaluated.
   This is used for getting the CTFE value out of a const-folded builtin,
   returns NULL if it cannot convert CST.  */

Expression *
d_eval_constant_expression (const Loc &loc, tree cst)
{
  STRIP_TYPE_NOPS (cst);
  Type *type = build_frontend_type (TREE_TYPE (cst));

  if (type)
    {
      /* Convert our GCC CST tree into a D Expression.  This seems like we are
	 trying too hard, as these will only be converted back to a tree again
	 later in the codegen pass, but satisfies the need to have GCC built-ins
	 CTFE-able in the frontend.  */
      tree_code code = TREE_CODE (cst);
      if (code == COMPLEX_CST)
	{
	  real_value re = TREE_REAL_CST (TREE_REALPART (cst));
	  real_value im = TREE_REAL_CST (TREE_IMAGPART (cst));
	  complex_t value = complex_t (ldouble (re), ldouble (im));
	  return ComplexExp::create (loc, value, type);
	}
      else if (code == INTEGER_CST)
	{
	  dinteger_t value = TREE_INT_CST_LOW (cst);
	  return IntegerExp::create (loc, value, type);
	}
      else if (code == REAL_CST)
	{
	  real_value value = TREE_REAL_CST (cst);
	  return RealExp::create (loc, ldouble (value), type);
	}
      else if (code == STRING_CST)
	{
	  const void *string = TREE_STRING_POINTER (cst);
	  size_t len = TREE_STRING_LENGTH (cst) - 1;
	  return StringExp::create (loc, CONST_CAST (void *, string), len);
	}
      else if (code == VECTOR_CST)
	{
	  dinteger_t nunits = VECTOR_CST_NELTS (cst).to_constant ();
	  Expressions *elements = d_gc_malloc<Expressions> ();
	  elements->setDim (nunits);

	  for (size_t i = 0; i < nunits; i++)
	    {
	      Expression *elem
		= d_eval_constant_expression (loc, VECTOR_CST_ELT (cst, i));
	      if (elem == NULL)
		return NULL;

	      (*elements)[i] = elem;
	    }

	  Expression *e = ArrayLiteralExp::create (loc, elements);
	  e->type = type->isTypeVector ()->basetype;

	  return VectorExp::create (loc, e, type);
	}
      else if (code == ADDR_EXPR)
	{
	  /* Special handling for trees constructed by build_string_literal.
	     What we receive is an `&"string"[0]' expression, strip off the
	     outer ADDR_EXPR and ARRAY_REF to get to the underlying CST.  */
	  tree pointee = TREE_OPERAND (cst, 0);

	  if (TREE_CODE (pointee) != ARRAY_REF
	      || TREE_OPERAND (pointee, 1) != integer_zero_node
	      || TREE_CODE (TREE_OPERAND (pointee, 0)) != STRING_CST)
	    return NULL;

	  return d_eval_constant_expression (loc, TREE_OPERAND (pointee, 0));
	}
    }

  return NULL;
}

/* Callback for TARGET_D_CPU_VERSIONS and TARGET_D_OS_VERSIONS.
   Adds IDENT to the list of predefined version identifiers.  */

void
d_add_builtin_version (const char* ident)
{
  VersionCondition::addPredefinedGlobalIdent (ident);
}

/* Initialize the list of all the predefined version identifiers.  */

void
d_init_versions (void)
{
  VersionCondition::addPredefinedGlobalIdent ("GNU");
  VersionCondition::addPredefinedGlobalIdent ("D_Version2");

  if (BYTES_BIG_ENDIAN)
    VersionCondition::addPredefinedGlobalIdent ("BigEndian");
  else
    VersionCondition::addPredefinedGlobalIdent ("LittleEndian");

  if (targetm_common.except_unwind_info (&global_options) == UI_SJLJ)
    VersionCondition::addPredefinedGlobalIdent ("GNU_SjLj_Exceptions");
  else if (targetm_common.except_unwind_info (&global_options) == UI_SEH)
    VersionCondition::addPredefinedGlobalIdent ("GNU_SEH_Exceptions");
  else if (targetm_common.except_unwind_info (&global_options) == UI_DWARF2)
    VersionCondition::addPredefinedGlobalIdent ("GNU_DWARF2_Exceptions");

  if (!targetm.have_tls)
    VersionCondition::addPredefinedGlobalIdent ("GNU_EMUTLS");

  if (STACK_GROWS_DOWNWARD)
    VersionCondition::addPredefinedGlobalIdent ("GNU_StackGrowsDown");

  /* Should define this anyway to set us apart from the competition.  */
  VersionCondition::addPredefinedGlobalIdent ("GNU_InlineAsm");

  /* LP64 only means 64bit pointers in D.  */
  if (POINTER_SIZE == 64)
    VersionCondition::addPredefinedGlobalIdent ("D_LP64");

  /* Setting `global.params.cov' forces module info generation which is
     not needed for the GCC coverage implementation.  Instead, just
     test flag_test_coverage while leaving `global.params.cov' unset.  */
  if (flag_test_coverage)
    VersionCondition::addPredefinedGlobalIdent ("D_Coverage");
  if (flag_pic)
    VersionCondition::addPredefinedGlobalIdent ("D_PIC");
  if (flag_pie)
    VersionCondition::addPredefinedGlobalIdent ("D_PIE");

  if (global.params.ddoc.doOutput)
    VersionCondition::addPredefinedGlobalIdent ("D_Ddoc");

  if (global.params.useUnitTests)
    VersionCondition::addPredefinedGlobalIdent ("unittest");

  if (global.params.useAssert == CHECKENABLEon)
    VersionCondition::addPredefinedGlobalIdent ("assert");

  if (global.params.useIn == CHECKENABLEon)
    VersionCondition::addPredefinedGlobalIdent("D_PreConditions");

  if (global.params.useOut == CHECKENABLEon)
    VersionCondition::addPredefinedGlobalIdent("D_PostConditions");

  if (global.params.useInvariants == CHECKENABLEon)
    VersionCondition::addPredefinedGlobalIdent("D_Invariants");

  if (global.params.useArrayBounds == CHECKENABLEoff)
    VersionCondition::addPredefinedGlobalIdent ("D_NoBoundsChecks");

  if (global.params.betterC)
    VersionCondition::addPredefinedGlobalIdent ("D_BetterC");
  else
    {
      VersionCondition::addPredefinedGlobalIdent ("D_ModuleInfo");
      VersionCondition::addPredefinedGlobalIdent ("D_Exceptions");
      VersionCondition::addPredefinedGlobalIdent ("D_TypeInfo");
    }

  if (optimize)
    VersionCondition::addPredefinedGlobalIdent ("D_Optimized");

  VersionCondition::addPredefinedGlobalIdent ("all");

  /* Emit all target-specific version identifiers.  */
  targetdm.d_cpu_versions ();
  targetdm.d_os_versions ();

  VersionCondition::addPredefinedGlobalIdent ("CppRuntime_Gcc");
}

/* A helper for d_build_builtins_module.  Return a new ALIAS for TYPE.
   Analogous to `alias ALIAS = TYPE' in D code.  */

static AliasDeclaration *
build_alias_declaration (const char *alias, Type *type)
{
  return AliasDeclaration::create (Loc (), Identifier::idPool (alias), type);
}

/* A helper function for Target::loadModule.  Generates all code for the
   `gcc.builtins' module, whose frontend symbol should be M.  */

void
d_build_builtins_module (Module *m)
{
  Dsymbols *members = d_gc_malloc<Dsymbols> ();
  tree decl;

  for (size_t i = 0; vec_safe_iterate (gcc_builtins_functions, i, &decl); ++i)
    {
      const char *name = IDENTIFIER_POINTER (DECL_NAME (decl));
      Type *t = build_frontend_type (TREE_TYPE (decl));
      TypeFunction *tf = t ? t->isTypeFunction () : NULL;

      /* Cannot create built-in function type for DECL.  */
      if (!tf)
	continue;

      /* A few notes on D2 attributes applied to builtin functions:
	 - It is assumed that built-ins solely provided by the compiler are
	   considered @safe and pure.
	 - Built-ins that correspond to `extern(C)' functions in the standard
	   library that have `__attribute__(nothrow)' are considered `@trusted'.
	 - The purity of a built-in can vary depending on compiler flags set
	   upon initialization, or by the `-foptions' passed, such as
	   flag_unsafe_math_optimizations.
	 - Built-ins never use the GC or raise a D exception, and so are always
	   marked as `nothrow' and `@nogc'.  */
      tf->purity = DECL_PURE_P (decl) ? PURE::const_
	: TREE_READONLY (decl) ? PURE::const_
	: DECL_IS_NOVOPS (decl) ? PURE::weak
	: !DECL_ASSEMBLER_NAME_SET_P (decl) ? PURE::weak
	: PURE::impure;
      tf->trust = !DECL_ASSEMBLER_NAME_SET_P (decl) ? TRUST::safe
	: TREE_NOTHROW (decl) ? TRUST::trusted
	: TRUST::system;
      tf->isnothrow (true);
      tf->isnogc (true);

      FuncDeclaration *func
	= FuncDeclaration::create (Loc (), Loc (),
				   Identifier::idPool (name),
				   STCextern, tf);
      DECL_LANG_SPECIFIC (decl) = build_lang_decl (func);
      func->csym = decl;
      func->builtin = BUILTIN::gcc;

      members->push (func);
    }

  for (size_t i = 0; vec_safe_iterate (gcc_builtins_types, i, &decl); ++i)
    {
      const char *name = IDENTIFIER_POINTER (DECL_NAME (decl));
      Type *t = build_frontend_type (TREE_TYPE (decl));

      /* Cannot create built-in type for DECL.  */
      if (!t)
	continue;

      members->push (build_alias_declaration (name, t));
    }

  /* Iterate through the target-specific builtin types for va_list.  */
  if (targetm.enum_va_list_p)
    {
      const char *name;
      tree type;

      for (int i = 0; targetm.enum_va_list_p (i, &name, &type); ++i)
	{
	  Type *t = build_frontend_type (type);
	  /* Cannot create built-in type.  */
	  if (!t)
	    continue;

	  members->push (build_alias_declaration (name, t));
	}
    }

  /* Push out declarations for any RECORD_TYPE types encountered when building
     all builtin functions and types.  */
  for (size_t i = 0; i < builtin_converted_decls.length (); ++i)
    {
      /* Currently, there is no need to run semantic, but we do want to output
	 initializers, typeinfo, and others on demand.  */
      Dsymbol *dsym = builtin_converted_decls[i].dsym;
      if (dsym != NULL && !dsym->isAnonymous ())
	{
	  dsym->parent = m;
	  members->push (dsym);
	}
    }

  /* Expose target-specific va_list type.  */
  Type *tvalist = target.va_listType (Loc (), NULL);
  TypeStruct *ts = tvalist->isTypeStruct ();
  if (ts == NULL || !ts->sym->isAnonymous ())
    members->push (build_alias_declaration ("__builtin_va_list", tvalist));
  else
    {
      ts->sym->ident = Identifier::idPool ("__builtin_va_list");
      members->push (ts->sym);
    }

  /* Expose target-specific integer types to the builtins module.  */
  {
    Type *t = build_frontend_type (long_integer_type_node);
    members->push (build_alias_declaration ("__builtin_clong", t));

    t = build_frontend_type (long_unsigned_type_node);
    members->push (build_alias_declaration ("__builtin_culong", t));

    t = build_frontend_type (long_long_integer_type_node);
    members->push (build_alias_declaration ("__builtin_clonglong", t));

    t = build_frontend_type (long_long_unsigned_type_node);
    members->push (build_alias_declaration ("__builtin_culonglong", t));

    t = build_frontend_type (lang_hooks.types.type_for_mode (byte_mode, 0));
    members->push (build_alias_declaration ("__builtin_machine_byte", t));

    t = build_frontend_type (lang_hooks.types.type_for_mode (byte_mode, 1));
    members->push (build_alias_declaration ("__builtin_machine_ubyte", t));

    t = build_frontend_type (lang_hooks.types.type_for_mode (word_mode, 0));
    members->push (build_alias_declaration ("__builtin_machine_int", t));

    t = build_frontend_type (lang_hooks.types.type_for_mode (word_mode, 1));
    members->push (build_alias_declaration ("__builtin_machine_uint", t));

    t = build_frontend_type (lang_hooks.types.type_for_mode (ptr_mode, 0));
    members->push (build_alias_declaration ("__builtin_pointer_int", t));

    t = build_frontend_type (lang_hooks.types.type_for_mode (ptr_mode, 1));
    members->push (build_alias_declaration ("__builtin_pointer_uint", t));

    /* _Unwind_Word has its own target specific mode.  */
    machine_mode mode = targetm.unwind_word_mode ();
    t = build_frontend_type (lang_hooks.types.type_for_mode (mode, 0));
    members->push (build_alias_declaration ("__builtin_unwind_int", t));

    t = build_frontend_type (lang_hooks.types.type_for_mode (mode, 1));
    members->push (build_alias_declaration ("__builtin_unwind_uint", t));
  }

  m->members->push (LinkDeclaration::create (Loc (), LINK::c, members));
}

/* Remove all type modifiers from TYPE, returning the naked type.  */

static Type *
strip_type_modifiers (Type *type)
{
  if (type->ty == TY::Tpointer)
    {
      Type *tnext = strip_type_modifiers (type->nextOf ());
      return tnext->pointerTo ();
    }

  return type->castMod (0);
}

/* Returns true if types T1 and T2 representing return types or types of
   function arguments are close enough to be considered interchangeable.  */

static bool
matches_builtin_type (Type *t1, Type *t2)
{
  Type *tb1 = strip_type_modifiers (t1);
  Type *tb2 = strip_type_modifiers (t2);

  if (same_type_p (t1, t2))
    return true;

  if (((tb1->isTypePointer () && tb2->isTypePointer ())
       || (tb1->isTypeVector () && tb2->isTypeVector ()))
      && tb1->implicitConvTo (tb2) != MATCH::nomatch)
    return true;

  if (tb1->isintegral () == tb2->isintegral ()
      && tb1->size () == tb2->size ())
    return true;

  return false;
}

/* Check whether the declared function type T1 is covariant with the built-in
   function type T2.  Returns true if they are covariant.  */

static bool
covariant_with_builtin_type_p (Type *t1, Type *t2)
{
  /* Check whether the declared function matches the built-in.  */
  if (same_type_p (t1, t2) || t1->covariant (t2) == Covariant::yes)
    return true;

  /* May not be covariant because of D attributes applied on t1.
     Strip them all off and compare again.  */
  TypeFunction *tf1 = t1->isTypeFunction ();
  TypeFunction *tf2 = t2->isTypeFunction ();

  /* Check for obvious reasons why types may be distinct.  */
  if (tf1 == NULL || tf2 == NULL
      || tf1->isref () != tf2->isref ()
      || tf1->parameterList.varargs != tf2->parameterList.varargs
      || tf1->parameterList.length () != tf2->parameterList.length ())
    return false;

  /* Check return type and each parameter type for mismatch.  */
  if (!matches_builtin_type (tf1->next, tf2->next))
    return false;

  const size_t nparams = tf1->parameterList.length ();
  for (size_t i = 0; i < nparams; i++)
    {
      Parameter *fparam1 = tf1->parameterList[i];
      Parameter *fparam2 = tf2->parameterList[i];

      if (fparam1->isReference () != fparam2->isReference ()
	  || fparam1->isLazy () != fparam2->isLazy ())
	return false;

      if (!matches_builtin_type (fparam1->type, fparam2->type))
	return false;
    }

  return true;
}

/* Search for any `extern(C)' functions that match any known GCC library builtin
   function in D and override its internal back-end symbol.  */

static void
maybe_set_builtin_1 (Dsymbol *d)
{
  AttribDeclaration *ad = d->isAttribDeclaration ();
  FuncDeclaration *fd = d->isFuncDeclaration ();

  if (ad != NULL)
    {
      /* Recursively search through attribute decls.  */
      Dsymbols *decls = ad->include (NULL);
      if (decls && decls->length)
	{
	  for (size_t i = 0; i < decls->length; i++)
	    {
	      Dsymbol *sym = (*decls)[i];
	      maybe_set_builtin_1 (sym);
	    }
	}
    }
  else if (fd && !fd->fbody && fd->resolvedLinkage () == LINK::c)
    {
      tree ident = get_identifier (fd->ident->toChars ());
      tree decl = IDENTIFIER_DECL_TREE (ident);

      if (decl && TREE_CODE (decl) == FUNCTION_DECL
	  && DECL_ASSEMBLER_NAME_SET_P (decl)
	  && fndecl_built_in_p (decl, BUILT_IN_NORMAL))
	{
	  /* Found a match, tell the frontend this is a builtin.  */
	  DECL_LANG_SPECIFIC (decl) = build_lang_decl (fd);
	  fd->csym = decl;
	  fd->builtin = BUILTIN::gcc;

	  /* Copy front-end attributes to the builtin.  */
	  apply_user_attributes (fd, fd->csym);

	  /* Function has `pragma(mangle)' specified, override its name.  */
	  if (fd->mangleOverride.length)
	    {
	      tree mangle =
		get_identifier_with_length (fd->mangleOverride.ptr,
					    fd->mangleOverride.length);
	      const char *asmname = IDENTIFIER_POINTER (mangle);
	      set_builtin_user_assembler_name (decl, asmname);
	    }

	  /* Warn when return and argument types of the user defined function is
	     not covariant with the built-in function type.  */
	  if (Type *type = build_frontend_type (TREE_TYPE (decl)))
	    {
	      if (!covariant_with_builtin_type_p (fd->type, type))
		{
		  warning_at (make_location_t (fd->loc),
			      OPT_Wbuiltin_declaration_mismatch,
			      "conflicting types for built-in function %qs; "
			      "expected %qs",
			      fd->toChars (), type->toChars ());
		}
	    }
	}
    }
}

/* A helper function for Target::loadModule.  Traverse all members in module M
   to search for any functions that can be mapped to any GCC builtin.  */

void
d_maybe_set_builtin (Module *m)
{
  if (!m || !m->members)
    return;

  for (size_t i = 0; i < m->members->length; i++)
    {
      Dsymbol *sym = (*m->members)[i];
      maybe_set_builtin_1 (sym);
    }
}

/* Used to help initialize the builtin-types.def table.  When a type of
   the correct size doesn't exist, use error_mark_node instead of NULL.
   The latter results in segfaults even when a decl using the type doesn't
   get invoked.  */

static tree
builtin_type_for_size (int size, bool unsignedp)
{
  tree type = lang_hooks.types.type_for_size (size, unsignedp);
  return type ? type : error_mark_node;
}

/* Support for DEF_BUILTIN.  */

static void
do_build_builtin_fn (built_in_function fncode,
		     const char *name,
		     built_in_class fnclass,
		     tree fntype, bool both_p, bool fallback_p,
		     tree fnattrs, bool implicit_p)
{
  tree decl;
  const char *libname;

  if (fntype == error_mark_node)
    return;

  gcc_assert ((!both_p && !fallback_p)
	      || startswith (name, "__builtin_"));

  libname = name + strlen ("__builtin_");

  decl = add_builtin_function (name, fntype, fncode, fnclass,
			       fallback_p ? libname : NULL, fnattrs);

  set_builtin_decl (fncode, decl, implicit_p);
}

/* Standard data types to be used in builtin argument declarations.  */

static GTY(()) tree string_type_node;
static GTY(()) tree const_string_type_node;
static GTY(()) tree wint_type_node;
static GTY(()) tree intmax_type_node;
static GTY(()) tree uintmax_type_node;
static GTY(()) tree signed_size_type_node;


/* Build nodes that would have been created by the C front-end; necessary
   for including builtin-types.def and ultimately builtins.def.  */

static void
d_build_c_type_nodes (void)
{
  string_type_node = build_pointer_type (char_type_node);
  const_string_type_node
    = build_pointer_type (build_qualified_type (char_type_node,
						TYPE_QUAL_CONST));

  if (strcmp (UINTMAX_TYPE, "unsigned int") == 0)
    {
      intmax_type_node = integer_type_node;
      uintmax_type_node = unsigned_type_node;
    }
  else if (strcmp (UINTMAX_TYPE, "long unsigned int") == 0)
    {
      intmax_type_node = long_integer_type_node;
      uintmax_type_node = long_unsigned_type_node;
    }
  else if (strcmp (UINTMAX_TYPE, "long long unsigned int") == 0)
    {
      intmax_type_node = long_long_integer_type_node;
      uintmax_type_node = long_long_unsigned_type_node;
    }
  else
    gcc_unreachable ();

  signed_size_type_node = signed_type_for (size_type_node);
  wint_type_node = unsigned_type_node;
  pid_type_node = integer_type_node;
}

/* Build nodes that are used by the D front-end.
   These are distinct from C types.  */

static void
d_build_d_type_nodes (void)
{
  /* Integral types.  */
  d_byte_type = make_signed_type (8);
  d_ubyte_type = make_unsigned_type (8);

  d_short_type = make_signed_type (16);
  d_ushort_type = make_unsigned_type (16);

  d_int_type = make_signed_type (32);
  d_uint_type = make_unsigned_type (32);

  d_long_type = make_signed_type (64);
  d_ulong_type = make_unsigned_type (64);

  d_cent_type = make_signed_type (128);
  d_ucent_type = make_unsigned_type (128);

  {
    /* Re-define size_t as a D type.  */
    machine_mode type_mode = TYPE_MODE (size_type_node);
    size_type_node = lang_hooks.types.type_for_mode (type_mode, 1);
  }

  /* Bool and Character types.  */
  d_bool_type = make_unsigned_type (1);
  TREE_SET_CODE (d_bool_type, BOOLEAN_TYPE);

  char8_type_node = make_unsigned_type (8);
  TYPE_STRING_FLAG (char8_type_node) = 1;

  char16_type_node = make_unsigned_type (16);
  TYPE_STRING_FLAG (char16_type_node) = 1;

  char32_type_node = make_unsigned_type (32);
  TYPE_STRING_FLAG (char32_type_node) = 1;

  /* Imaginary types.  */
  ifloat_type_node = build_distinct_type_copy (float_type_node);
  TYPE_IMAGINARY_FLOAT (ifloat_type_node) = 1;

  idouble_type_node = build_distinct_type_copy (double_type_node);
  TYPE_IMAGINARY_FLOAT (idouble_type_node) = 1;

  ireal_type_node = build_distinct_type_copy (long_double_type_node);
  TYPE_IMAGINARY_FLOAT (ireal_type_node) = 1;

  /* Noreturn type.  */
  noreturn_type_node = build_distinct_type_copy (void_type_node);

  /* Calling build_ctype() links the front-end Type to the GCC node,
     and sets the TYPE_NAME to the D language type.  */
  for (unsigned ty = 0; ty < (unsigned) TY::TMAX; ty++)
    {
      if (Type::basic[ty] != NULL)
	build_ctype (Type::basic[ty]);
    }

  /* Used for ModuleInfo, ClassInfo, and Interface decls.  */
  unknown_type_node = make_node (RECORD_TYPE);

  /* Make sure we get a unique function type, so we can give
     its pointer type a name.  (This wins for gdb).  */
  {
    tree vfunc_type = make_node (FUNCTION_TYPE);
    TREE_TYPE (vfunc_type) = d_int_type;
    TYPE_ARG_TYPES (vfunc_type) = NULL_TREE;
    layout_type (vfunc_type);

    vtable_entry_type = build_pointer_type (vfunc_type);
  }

  vtbl_ptr_type_node = build_pointer_type (vtable_entry_type);
  layout_type (vtbl_ptr_type_node);

  /* When an object is accessed via an interface, this type appears
     as the first entry in its vtable.  */
  {
    tree domain = build_index_type (size_int (3));
    vtbl_interface_type_node = build_array_type (ptr_type_node, domain);
  }

  /* Use `void[]' as a generic dynamic array type.  */
  array_type_node = make_struct_type ("__builtin_void[]", 2,
				      get_identifier ("length"), size_type_node,
				      get_identifier ("ptr"), ptr_type_node);
  TYPE_DYNAMIC_ARRAY (array_type_node) = 1;

  null_array_node = d_array_value (array_type_node, size_zero_node,
				   null_pointer_node);
}

/* Handle default attributes.  */

enum built_in_attribute
{
#define DEF_ATTR_NULL_TREE(ENUM) ENUM,
#define DEF_ATTR_INT(ENUM, VALUE) ENUM,
#define DEF_ATTR_STRING(ENUM, VALUE) ENUM,
#define DEF_ATTR_IDENT(ENUM, STRING) ENUM,
#define DEF_ATTR_TREE_LIST(ENUM, PURPOSE, VALUE, CHAIN) ENUM,
#include "builtin-attrs.def"
#undef DEF_ATTR_NULL_TREE
#undef DEF_ATTR_INT
#undef DEF_ATTR_STRING
#undef DEF_ATTR_IDENT
#undef DEF_ATTR_TREE_LIST
  ATTR_LAST
};

static GTY(()) tree built_in_attributes[(int) ATTR_LAST];

/* Initialize the attribute table for all the supported builtins.  */

static void
d_init_attributes (void)
{
  /* Fill in the built_in_attributes array.  */
#define DEF_ATTR_NULL_TREE(ENUM)	\
  built_in_attributes[(int) ENUM] = NULL_TREE;
# define DEF_ATTR_INT(ENUM, VALUE)	\
  built_in_attributes[(int) ENUM] = build_int_cst (NULL_TREE, VALUE);
#define DEF_ATTR_STRING(ENUM, VALUE)	\
  built_in_attributes[(int) ENUM] = build_string (strlen (VALUE), VALUE);
#define DEF_ATTR_IDENT(ENUM, STRING)	\
  built_in_attributes[(int) ENUM] = get_identifier (STRING);
#define DEF_ATTR_TREE_LIST(ENUM, PURPOSE, VALUE, CHAIN)	\
  built_in_attributes[(int) ENUM]			\
  = tree_cons (built_in_attributes[(int) PURPOSE],	\
	       built_in_attributes[(int) VALUE],	\
	       built_in_attributes[(int) CHAIN]);
#include "builtin-attrs.def"
#undef DEF_ATTR_NULL_TREE
#undef DEF_ATTR_INT
#undef DEF_ATTR_STRING
#undef DEF_ATTR_IDENT
#undef DEF_ATTR_TREE_LIST
}

/* Builtin types.  */

enum d_builtin_type
{
#define DEF_PRIMITIVE_TYPE(NAME, VALUE) NAME,
#define DEF_FUNCTION_TYPE_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_FUNCTION_TYPE_3(NAME, RETURN, ARG1, ARG2, ARG3) NAME,
#define DEF_FUNCTION_TYPE_4(NAME, RETURN, ARG1, ARG2, ARG3, ARG4) NAME,
#define DEF_FUNCTION_TYPE_5(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) NAME,
#define DEF_FUNCTION_TYPE_6(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6) NAME,
#define DEF_FUNCTION_TYPE_7(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7) NAME,
#define DEF_FUNCTION_TYPE_8(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8) NAME,
#define DEF_FUNCTION_TYPE_9(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8, ARG9) NAME,
#define DEF_FUNCTION_TYPE_10(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10) NAME,
#define DEF_FUNCTION_TYPE_11(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10, ARG11) NAME,
#define DEF_FUNCTION_TYPE_VAR_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_VAR_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_VAR_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_FUNCTION_TYPE_VAR_3(NAME, RETURN, ARG1, ARG2, ARG3) NAME,
#define DEF_FUNCTION_TYPE_VAR_4(NAME, RETURN, ARG1, ARG2, ARG3, ARG4) NAME,
#define DEF_FUNCTION_TYPE_VAR_5(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) \
				NAME,
#define DEF_FUNCTION_TYPE_VAR_6(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6) NAME,
#define DEF_FUNCTION_TYPE_VAR_7(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6, ARG7) NAME,
#define DEF_FUNCTION_TYPE_VAR_11(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				 ARG6, ARG7, ARG8, ARG9, ARG10, ARG11) NAME,
#define DEF_POINTER_TYPE(NAME, TYPE) NAME,
#include "builtin-types.def"
#undef DEF_PRIMITIVE_TYPE
#undef DEF_FUNCTION_TYPE_0
#undef DEF_FUNCTION_TYPE_1
#undef DEF_FUNCTION_TYPE_2
#undef DEF_FUNCTION_TYPE_3
#undef DEF_FUNCTION_TYPE_4
#undef DEF_FUNCTION_TYPE_5
#undef DEF_FUNCTION_TYPE_6
#undef DEF_FUNCTION_TYPE_7
#undef DEF_FUNCTION_TYPE_8
#undef DEF_FUNCTION_TYPE_9
#undef DEF_FUNCTION_TYPE_10
#undef DEF_FUNCTION_TYPE_11
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_FUNCTION_TYPE_VAR_3
#undef DEF_FUNCTION_TYPE_VAR_4
#undef DEF_FUNCTION_TYPE_VAR_5
#undef DEF_FUNCTION_TYPE_VAR_6
#undef DEF_FUNCTION_TYPE_VAR_7
#undef DEF_FUNCTION_TYPE_VAR_11
#undef DEF_POINTER_TYPE
  BT_LAST
};

typedef enum d_builtin_type builtin_type;

/* A temporary array used in communication with def_fn_type.  */
static GTY(()) tree builtin_types[(int) BT_LAST + 1];

/* A helper function for d_init_builtins.  Build function type for DEF with
   return type RET and N arguments.  If VAR is true, then the function should
   be variadic after those N arguments.

   Takes special care not to ICE if any of the types involved are
   error_mark_node, which indicates that said type is not in fact available
   (see builtin_type_for_size).  In which case the function type as a whole
   should be error_mark_node.  */

static void
def_fn_type (builtin_type def, builtin_type ret, bool var, int n, ...)
{
  tree t;
  tree *args = XALLOCAVEC (tree, n);
  va_list list;
  int i;

  va_start (list, n);
  for (i = 0; i < n; ++i)
    {
      builtin_type a = (builtin_type) va_arg (list, int);
      t = builtin_types[a];
      if (t == error_mark_node)
	goto egress;
      args[i] = t;
    }

  t = builtin_types[ret];
  if (t == error_mark_node)
    goto egress;
  if (var)
    t = build_varargs_function_type_array (t, n, args);
  else
    t = build_function_type_array (t, n, args);

 egress:
  builtin_types[def] = t;
  va_end (list);
}

/* Create builtin types and functions.  VA_LIST_REF_TYPE_NODE and
   VA_LIST_ARG_TYPE_NODE are used in builtin-types.def.  */

static void
d_define_builtins (tree va_list_ref_type_node ATTRIBUTE_UNUSED,
		   tree va_list_arg_type_node ATTRIBUTE_UNUSED)
{
#define DEF_PRIMITIVE_TYPE(ENUM, VALUE) \
  builtin_types[(int) ENUM] = VALUE;
#define DEF_FUNCTION_TYPE_0(ENUM, RETURN) \
  def_fn_type (ENUM, RETURN, 0, 0);
#define DEF_FUNCTION_TYPE_1(ENUM, RETURN, ARG1) \
  def_fn_type (ENUM, RETURN, 0, 1, ARG1);
#define DEF_FUNCTION_TYPE_2(ENUM, RETURN, ARG1, ARG2) \
  def_fn_type (ENUM, RETURN, 0, 2, ARG1, ARG2);
#define DEF_FUNCTION_TYPE_3(ENUM, RETURN, ARG1, ARG2, ARG3) \
  def_fn_type (ENUM, RETURN, 0, 3, ARG1, ARG2, ARG3);
#define DEF_FUNCTION_TYPE_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4) \
  def_fn_type (ENUM, RETURN, 0, 4, ARG1, ARG2, ARG3, ARG4);
#define DEF_FUNCTION_TYPE_5(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) \
  def_fn_type (ENUM, RETURN, 0, 5, ARG1, ARG2, ARG3, ARG4, ARG5);
#define DEF_FUNCTION_TYPE_6(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6)					\
  def_fn_type (ENUM, RETURN, 0, 6, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
#define DEF_FUNCTION_TYPE_7(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7)					\
  def_fn_type (ENUM, RETURN, 0, 7, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7);
#define DEF_FUNCTION_TYPE_8(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8)				\
  def_fn_type (ENUM, RETURN, 0, 8, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,  \
	       ARG7, ARG8);
#define DEF_FUNCTION_TYPE_9(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8, ARG9)			\
  def_fn_type (ENUM, RETURN, 0, 9, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,  \
	       ARG7, ARG8, ARG9);
#define DEF_FUNCTION_TYPE_10(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8, ARG9, ARG10)		 \
  def_fn_type (ENUM, RETURN, 0, 10, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,  \
	       ARG7, ARG8, ARG9, ARG10);
#define DEF_FUNCTION_TYPE_11(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8, ARG9, ARG10, ARG11)	 \
  def_fn_type (ENUM, RETURN, 0, 11, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,  \
	       ARG7, ARG8, ARG9, ARG10, ARG11);
#define DEF_FUNCTION_TYPE_VAR_0(ENUM, RETURN) \
  def_fn_type (ENUM, RETURN, 1, 0);
#define DEF_FUNCTION_TYPE_VAR_1(ENUM, RETURN, ARG1) \
  def_fn_type (ENUM, RETURN, 1, 1, ARG1);
#define DEF_FUNCTION_TYPE_VAR_2(ENUM, RETURN, ARG1, ARG2) \
  def_fn_type (ENUM, RETURN, 1, 2, ARG1, ARG2);
#define DEF_FUNCTION_TYPE_VAR_3(ENUM, RETURN, ARG1, ARG2, ARG3) \
  def_fn_type (ENUM, RETURN, 1, 3, ARG1, ARG2, ARG3);
#define DEF_FUNCTION_TYPE_VAR_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4) \
  def_fn_type (ENUM, RETURN, 1, 4, ARG1, ARG2, ARG3, ARG4);
#define DEF_FUNCTION_TYPE_VAR_5(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) \
  def_fn_type (ENUM, RETURN, 1, 5, ARG1, ARG2, ARG3, ARG4, ARG5);
#define DEF_FUNCTION_TYPE_VAR_6(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6)					    \
  def_fn_type (ENUM, RETURN, 1, 6, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
#define DEF_FUNCTION_TYPE_VAR_7(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6, ARG7)				    \
  def_fn_type (ENUM, RETURN, 1, 7, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7);
#define DEF_FUNCTION_TYPE_VAR_11(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				 ARG6, ARG7, ARG8, ARG9, ARG10, ARG11)       \
  def_fn_type (ENUM, RETURN, 1, 11, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,      \
	       ARG7, ARG8, ARG9, ARG10, ARG11);
#define DEF_POINTER_TYPE(ENUM, TYPE) \
  builtin_types[(int) ENUM] = build_pointer_type (builtin_types[(int) TYPE]);

#include "builtin-types.def"

#undef DEF_PRIMITIVE_TYPE
#undef DEF_FUNCTION_TYPE_1
#undef DEF_FUNCTION_TYPE_2
#undef DEF_FUNCTION_TYPE_3
#undef DEF_FUNCTION_TYPE_4
#undef DEF_FUNCTION_TYPE_5
#undef DEF_FUNCTION_TYPE_6
#undef DEF_FUNCTION_TYPE_7
#undef DEF_FUNCTION_TYPE_8
#undef DEF_FUNCTION_TYPE_9
#undef DEF_FUNCTION_TYPE_10
#undef DEF_FUNCTION_TYPE_11
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_FUNCTION_TYPE_VAR_3
#undef DEF_FUNCTION_TYPE_VAR_4
#undef DEF_FUNCTION_TYPE_VAR_5
#undef DEF_FUNCTION_TYPE_VAR_6
#undef DEF_FUNCTION_TYPE_VAR_7
#undef DEF_FUNCTION_TYPE_VAR_11
#undef DEF_POINTER_TYPE
  builtin_types[(int) BT_LAST] = NULL_TREE;

  d_init_attributes ();

#define DEF_BUILTIN(ENUM, NAME, CLASS, TYPE, LIBTYPE, BOTH_P, FALLBACK_P, \
		    NONANSI_P, ATTRS, IMPLICIT, COND)			  \
  if (NAME && COND)							  \
    do_build_builtin_fn (ENUM, NAME, CLASS,				  \
			 builtin_types[(int) TYPE],			  \
			 BOTH_P, FALLBACK_P,				  \
			 built_in_attributes[(int) ATTRS], IMPLICIT);
#include "builtins.def"
#undef DEF_BUILTIN
}

/* Build builtin functions and types for the D language frontend.  */

void
d_init_builtins (void)
{
  d_build_c_type_nodes ();
  d_build_d_type_nodes ();

  if (TREE_CODE (va_list_type_node) == ARRAY_TYPE)
    {
      /* It might seem natural to make the argument type a pointer, but there
	 is no implicit casting from arrays to pointers in D.  */
      d_define_builtins (va_list_type_node, va_list_type_node);
    }
  else
    {
      d_define_builtins (build_reference_type (va_list_type_node),
			 va_list_type_node);
    }

  targetm.init_builtins ();
  build_common_builtin_nodes ();
}

/* Registration of machine- or os-specific builtin types.
   Add to builtin types list for maybe processing later
   if `gcc.builtins' was imported into the current module.  */

void
d_register_builtin_type (tree type, const char *name)
{
  tree decl = build_decl (UNKNOWN_LOCATION, TYPE_DECL,
			  get_identifier (name), type);
  DECL_ARTIFICIAL (decl) = 1;

  if (!TYPE_NAME (type))
    TYPE_NAME (type) = decl;

  vec_safe_push (gcc_builtins_types, decl);
}

/* Add DECL to builtin functions list for maybe processing later
   if `gcc.builtins' was imported into the current module.  */

tree
d_builtin_function (tree decl)
{
  if (!flag_no_builtin && DECL_ASSEMBLER_NAME_SET_P (decl))
    {
      /* Associate the assembler identifier with the built-in.  */
      tree ident = DECL_ASSEMBLER_NAME (decl);
      IDENTIFIER_DECL_TREE (ident) = decl;
    }

  vec_safe_push (gcc_builtins_functions, decl);
  return decl;
}

/* Same as d_builtin_function, but used to delay putting in back-end builtin
   functions until the ISA that defines the builtin has been declared.
   However in D, there is no global namespace.  All builtins get pushed into the
   `gcc.builtins' module, which is constructed during the semantic analysis
   pass, which has already finished by the time target attributes are evaluated.
   So builtins are not pushed because they would be ultimately ignored.
   The purpose of having this function then is to improve compile-time
   reflection support to allow user-code to determine whether a given back end
   function is enabled by the ISA.  */

tree
d_builtin_function_ext_scope (tree decl)
{
  return decl;
}

#include "gt-d-d-builtins.h"
