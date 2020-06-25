/* decl.cc -- Lower D frontend declarations to GCC trees.
   Copyright (C) 2006-2020 Free Software Foundation, Inc.

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

#include "dmd/aggregate.h"
#include "dmd/attrib.h"
#include "dmd/cond.h"
#include "dmd/ctfe.h"
#include "dmd/declaration.h"
#include "dmd/enum.h"
#include "dmd/errors.h"
#include "dmd/globals.h"
#include "dmd/hdrgen.h"
#include "dmd/identifier.h"
#include "dmd/import.h"
#include "dmd/init.h"
#include "dmd/mangle.h"
#include "dmd/module.h"
#include "dmd/nspace.h"
#include "dmd/target.h"
#include "dmd/template.h"

#include "tree.h"
#include "tree-iterator.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "target.h"
#include "common/common-target.h"
#include "cgraph.h"
#include "toplev.h"
#include "stringpool.h"
#include "varasm.h"
#include "stor-layout.h"
#include "attribs.h"
#include "function.h"
#include "debug.h"
#include "tree-pretty-print.h"

#include "d-tree.h"


/* Return identifier for the external mangled name of DECL.  */

const char *
d_mangle_decl (Dsymbol *decl)
{
  if (decl->isFuncDeclaration ())
    return mangleExact ((FuncDeclaration *) decl);
  else
    {
      OutBuffer buf;
      mangleToBuffer (decl, &buf);
      return buf.extractChars ();
    }
}

/* Generate a mangled identifier using NAME and SUFFIX, prefixed by the
   assembler name for DECL.  */

tree
mangle_internal_decl (Dsymbol *decl, const char *name, const char *suffix)
{
  const char *prefix = d_mangle_decl (decl);
  unsigned namelen = strlen (name);
  unsigned buflen = (2 + strlen (prefix) + namelen + strlen (suffix)) * 2;
  char *buf = (char *) alloca (buflen);

  snprintf (buf, buflen, "_D%s%u%s%s", prefix, namelen, name, suffix);
  tree ident = get_identifier (buf);

  /* Symbol is not found in user code, but generate a readable name for it
     anyway for debug and diagnostic reporting.  */
  snprintf (buf, buflen, "%s.%s", decl->toPrettyChars (), name);
  IDENTIFIER_PRETTY_NAME (ident) = get_identifier (buf);

  return ident;
}

/* Returns true if DECL is from the gcc.attribute module.  */

static bool
gcc_attribute_p (Dsymbol *decl)
{
  ModuleDeclaration *md = decl->getModule ()->md;

  if (md && md->packages && md->packages->length == 1)
    {
      if (!strcmp ((*md->packages)[0]->toChars (), "gcc")
	  && !strcmp (md->id->toChars (), "attribute"))
	return true;
    }

  return false;
}

/* Implements the visitor interface to lower all Declaration AST classes
   emitted from the D Front-end to GCC trees.
   All visit methods accept one parameter D, which holds the frontend AST
   of the declaration to compile.  These also don't return any value, instead
   generated code are appened to global_declarations or added to the
   current_binding_level by d_pushdecl().  */

class DeclVisitor : public Visitor
{
  using Visitor::visit;

  /* If we're lowering the body of a version(unittest) condition.  */
  bool in_version_unittest_;

public:
  DeclVisitor (void)
  {
    this->in_version_unittest_ = false;
  }

  /* Helper for generating code for the dsymbol AST class D.
     Sets up the location of the symbol before lowering.  */

  void build_dsymbol (Dsymbol *d)
  {
    location_t saved_location = input_location;
    input_location = make_location_t (d->loc);
    d->accept (this);
    input_location = saved_location;
  }

  /* This should be overridden by each declaration class.  */

  void visit (Dsymbol *)
  {
  }

  /* Compile a D module, and all members of it.  */

  void visit (Module *d)
  {
    if (d->semanticRun >= PASSobj)
      return;

    build_module_tree (d);
    d->semanticRun = PASSobj;
  }

  /* Write the imported symbol to debug.  */

  void visit (Import *d)
  {
    if (d->semanticRun >= PASSobj)
      return;

    /* Implements import declarations by telling the debug back-end we are
       importing the NAMESPACE_DECL of the module or IMPORTED_DECL of the
       declaration into the current lexical scope CONTEXT.  NAME is set if
       this is a renamed import.  */
    if (d->isstatic)
      return;

    /* Get the context of this import, this should never be null.  */
    tree context = d_module_context ();

    if (d->ident == NULL)
      {
	/* Importing declaration list.  */
	for (size_t i = 0; i < d->names.length; i++)
	  {
	    AliasDeclaration *aliasdecl = d->aliasdecls[i];
	    tree decl = build_import_decl (aliasdecl);

	    /* Skip over unhandled imports.  */
	    if (decl == NULL_TREE)
	      continue;

	    Identifier *alias = d->aliases[i];
	    tree name = (alias != NULL)
	      ? get_identifier (alias->toChars ()) : NULL_TREE;

	    debug_hooks->imported_module_or_decl (decl, name, context,
						  false, false);
	  }
      }
    else
      {
	/* Importing the entire module.  */
	tree decl = build_import_decl (d->mod);

	tree name = (d->aliasId != NULL)
	  ? get_identifier (d->aliasId->toChars ()) : NULL_TREE;

	debug_hooks->imported_module_or_decl (decl, name, context,
					      false, false);
      }

    d->semanticRun = PASSobj;
  }

  /* Expand any local variables found in tuples.  */

  void visit (TupleDeclaration *d)
  {
    for (size_t i = 0; i < d->objects->length; i++)
      {
	RootObject *o = (*d->objects)[i];
	if (o->dyncast () == DYNCAST_EXPRESSION)
	  {
	    DsymbolExp *de = ((Expression *) o)->isDsymbolExp ();
	    if (de != NULL && de->s->isDeclaration ())
	      this->build_dsymbol (de->s);
	  }
      }
  }

  /* Walk over all declarations in the attribute scope.  */

  void visit (AttribDeclaration *d)
  {
    Dsymbols *ds = d->include (NULL);

    if (!ds)
      return;

    for (size_t i = 0; i < ds->length; i++)
      this->build_dsymbol ((*ds)[i]);
  }

  /* Pragmas are a way to pass special information to the compiler and to add
     vendor specific extensions to D.  We don't do anything here, yet.  */

  void visit (PragmaDeclaration *d)
  {
    if (!global.params.ignoreUnsupportedPragmas)
      {
	if (d->ident == Identifier::idPool ("lib")
	    || d->ident == Identifier::idPool ("startaddress"))
	  {
	    warning_at (make_location_t (d->loc), OPT_Wunknown_pragmas,
			"pragma(%s) not implemented", d->ident->toChars ());
	  }
      }

    visit ((AttribDeclaration *) d);
  }

  /* Conditional compilation is the process of selecting which code to compile
     and which code to not compile.  Look for version conditions that may  */

  void visit (ConditionalDeclaration *d)
  {
    bool old_condition = this->in_version_unittest_;

    if (global.params.useUnitTests)
      {
	VersionCondition *vc = d->condition->isVersionCondition ();
	if (vc && vc->ident == Identifier::idPool ("unittest"))
	  this->in_version_unittest_ = true;
      }

    visit ((AttribDeclaration *) d);

    this->in_version_unittest_ = old_condition;
  }

  /* Walk over all members in the namespace scope.  */

  void visit (Nspace *d)
  {
    if (isError (d) || !d->members)
      return;

    for (size_t i = 0; i < d->members->length; i++)
      this->build_dsymbol ((*d->members)[i]);
  }

  /* Templates are D's approach to generic programming.  They have no members
     that can be emitted, however if the template is nested and used as a
     voldemort type, then it's members must be compiled before the parent
     function finishes.  */

  void visit (TemplateDeclaration *d)
  {
    /* Type cannot be directly named outside of the scope it's declared in, so
       the only way it can be escaped is if the function has auto return.  */
    FuncDeclaration *fd = d_function_chain ? d_function_chain->function : NULL;

    if (!fd || !fd->isAuto ())
      return;

    /* Check if the function returns an instantiated type that may contain
       nested members.  Only applies to classes or structs.  */
    Type *tb = fd->type->nextOf ()->baseElemOf ();

    while (tb->ty == Tarray || tb->ty == Tpointer)
      tb = tb->nextOf ()->baseElemOf ();

    TemplateInstance *ti = NULL;

    if (tb->ty == Tstruct)
      ti = tb->isTypeStruct ()->sym->isInstantiated ();
    else if (tb->ty == Tclass)
      ti = tb->isTypeClass ()->sym->isInstantiated ();

    /* Return type is instantiated from this template declaration, walk over
       all members of the instance.  */
    if (ti && ti->tempdecl == d)
      this->build_dsymbol (ti);
  }

  /* Walk over all members in the instantiated template.  */

  void visit (TemplateInstance *d)
  {
    if (isError (d)|| !d->members)
      return;

    if (!d->needsCodegen ())
      return;

    for (size_t i = 0; i < d->members->length; i++)
      this->build_dsymbol ((*d->members)[i]);
  }

  /* Walk over all members in the mixin template scope.  */

  void visit (TemplateMixin *d)
  {
    if (isError (d)|| !d->members)
      return;

    for (size_t i = 0; i < d->members->length; i++)
      this->build_dsymbol ((*d->members)[i]);
  }

  /* Write out compiler generated TypeInfo, initializer and functions for the
     given struct declaration, walking over all static members.  */

  void visit (StructDeclaration *d)
  {
    if (d->semanticRun >= PASSobj)
      return;

    if (d->type->ty == Terror)
      {
	error_at (make_location_t (d->loc),
		  "had semantic errors when compiling");
	return;
      }

    /* Add this decl to the current binding level.  */
    tree ctype = build_ctype (d->type);
    if (TYPE_NAME (ctype))
      d_pushdecl (TYPE_NAME (ctype));

    /* Anonymous structs/unions only exist as part of others,
       do not output forward referenced structs.  */
    if (d->isAnonymous () || !d->members)
      return;

    /* Don't emit any symbols from gcc.attribute module.  */
    if (gcc_attribute_p (d))
      return;

    /* Generate TypeInfo.  */
    if (have_typeinfo_p (Type::dtypeinfo))
      create_typeinfo (d->type, NULL);

    /* Generate static initializer.  */
    d->sinit = aggregate_initializer_decl (d);
    DECL_INITIAL (d->sinit) = layout_struct_initializer (d);

    if (d->isInstantiated ())
      d_linkonce_linkage (d->sinit);

    d_finish_decl (d->sinit);

    /* Put out the members.  There might be static constructors in the members
       list, and they cannot be put in separate object files.  */
    for (size_t i = 0; i < d->members->length; i++)
      this->build_dsymbol ((*d->members)[i]);

    /* Put out xopEquals, xopCmp and xopHash.  */
    if (d->xeq && d->xeq != d->xerreq)
      this->build_dsymbol (d->xeq);

    if (d->xcmp && d->xcmp != d->xerrcmp)
      this->build_dsymbol (d->xcmp);

    if (d->xhash)
      this->build_dsymbol (d->xhash);

    d->semanticRun = PASSobj;
  }

  /* Finish semantic analysis of functions in vtbl for class CD.  */

  bool finish_vtable (ClassDeclaration *d)
  {
    bool has_errors = false;

    /* Finish semantic analysis of functions in vtbl[].  */
    for (size_t i = d->vtblOffset (); i < d->vtbl.length; i++)
      {
	FuncDeclaration *fd = d->vtbl[i]->isFuncDeclaration ();

	if (!fd || (!fd->fbody && d->isAbstract ()))
	  continue;

	/* Ensure function has a return value.  */
	if (!fd->functionSemantic ())
	  has_errors = true;

	/* No name hiding to check for.  */
	if (!d->isFuncHidden (fd) || fd->isFuture ())
	  continue;

	/* The function fd is hidden from the view of the class.
	   If it overlaps with any function in the vtbl[], then
	   issue an error.  */
	for (size_t j = 1; j < d->vtbl.length; j++)
	  {
	    if (j == i)
	      continue;

	    FuncDeclaration *fd2 = d->vtbl[j]->isFuncDeclaration ();
	    if (!fd2->ident->equals (fd->ident))
	      continue;

	    /* The function is marked as @__future, a deprecation has
	       already been given by the frontend.  */
	    if (fd2->isFuture ())
	      continue;

	    if (fd->leastAsSpecialized (fd2) || fd2->leastAsSpecialized (fd))
	      {
		error_at (make_location_t (fd->loc), "use of %qs",
			  fd->toPrettyChars ());
		inform (make_location_t (fd2->loc), "is hidden by %qs",
			fd2->toPrettyChars ());
		inform (make_location_t (d->loc),
			"use %<alias %s = %s.%s;%> to introduce base class "
			"overload set", fd->toChars (),
			fd->parent->toChars (), fd->toChars ());
		has_errors = true;
		break;
	      }
	  }
      }

    return !has_errors;
  }

  /* Write out compiler generated TypeInfo, initializer and vtables for the
     given class declaration, walking over all static members.  */

  void visit (ClassDeclaration *d)
  {
    if (d->semanticRun >= PASSobj)
      return;

    if (d->type->ty == Terror)
      {
	error_at (make_location_t (d->loc),
		  "had semantic errors when compiling");
	return;
      }

    if (!d->members)
      return;

    /* Put out the members.  */
    for (size_t i = 0; i < d->members->length; i++)
      this->build_dsymbol ((*d->members)[i]);

    /* If something goes wrong during final semantic pass, don't bother with
       the rest as we may have incomplete info.  */
    if (!this->finish_vtable (d))
      return;

    /* Generate C symbols.  */
    d->csym = get_classinfo_decl (d);
    d->vtblsym = get_vtable_decl (d);
    d->sinit = aggregate_initializer_decl (d);

    /* Generate static initializer.  */
    DECL_INITIAL (d->sinit) = layout_class_initializer (d);
    d_linkonce_linkage (d->sinit);
    d_finish_decl (d->sinit);

    /* Put out the TypeInfo.  */
    if (have_typeinfo_p (Type::dtypeinfo))
      create_typeinfo (d->type, NULL);

    DECL_INITIAL (d->csym) = layout_classinfo (d);
    d_linkonce_linkage (d->csym);
    d_finish_decl (d->csym);

    /* Put out the vtbl[].  */
    vec <constructor_elt, va_gc> *elms = NULL;

    /* First entry is ClassInfo reference.  */
    if (d->vtblOffset ())
      CONSTRUCTOR_APPEND_ELT (elms, size_zero_node, build_address (d->csym));

    for (size_t i = d->vtblOffset (); i < d->vtbl.length; i++)
      {
	FuncDeclaration *fd = d->vtbl[i]->isFuncDeclaration ();

	if (fd && (fd->fbody || !d->isAbstract ()))
	  {
	    CONSTRUCTOR_APPEND_ELT (elms, size_int (i),
				    build_address (get_symbol_decl (fd)));
	  }
      }

    DECL_INITIAL (d->vtblsym)
      = build_constructor (TREE_TYPE (d->vtblsym), elms);
    d_comdat_linkage (d->vtblsym);
    d_finish_decl (d->vtblsym);

    /* Add this decl to the current binding level.  */
    tree ctype = TREE_TYPE (build_ctype (d->type));
    if (TYPE_NAME (ctype))
      d_pushdecl (TYPE_NAME (ctype));

    d->semanticRun = PASSobj;
  }

  /* Write out compiler generated TypeInfo and vtables for the given interface
     declaration, walking over all static members.  */

  void visit (InterfaceDeclaration *d)
  {
    if (d->semanticRun >= PASSobj)
      return;

    if (d->type->ty == Terror)
      {
	error_at (make_location_t (d->loc),
		  "had semantic errors when compiling");
	return;
      }

    if (!d->members)
      return;

    /* Put out the members.  */
    for (size_t i = 0; i < d->members->length; i++)
      this->build_dsymbol ((*d->members)[i]);

    /* Generate C symbols.  */
    d->csym = get_classinfo_decl (d);

    /* Put out the TypeInfo.  */
    if (have_typeinfo_p (Type::dtypeinfo))
      {
	create_typeinfo (d->type, NULL);
	this->build_dsymbol (d->type->vtinfo);
      }

    DECL_INITIAL (d->csym) = layout_classinfo (d);
    d_linkonce_linkage (d->csym);
    d_finish_decl (d->csym);

    /* Add this decl to the current binding level.  */
    tree ctype = TREE_TYPE (build_ctype (d->type));
    if (TYPE_NAME (ctype))
      d_pushdecl (TYPE_NAME (ctype));

    d->semanticRun = PASSobj;
  }

  /* Write out compiler generated TypeInfo and initializer for the given
     enum declaration.  */

  void visit (EnumDeclaration *d)
  {
    if (d->semanticRun >= PASSobj)
      return;

    if (d->errors || d->type->ty == Terror)
      {
	error_at (make_location_t (d->loc),
		  "had semantic errors when compiling");
	return;
      }

    if (d->isAnonymous ())
      return;

    /* Generate TypeInfo.  */
    if (have_typeinfo_p (Type::dtypeinfo))
      create_typeinfo (d->type, NULL);

    TypeEnum *tc = d->type->isTypeEnum ();
    if (tc->sym->members && !d->type->isZeroInit ())
      {
	/* Generate static initializer.  */
	d->sinit = enum_initializer_decl (d);
	DECL_INITIAL (d->sinit) = build_expr (tc->sym->defaultval, true);

	if (d->isInstantiated ())
	  d_linkonce_linkage (d->sinit);

	d_finish_decl (d->sinit);

	/* Add this decl to the current binding level.  */
	tree ctype = build_ctype (d->type);
	if (TREE_CODE (ctype) == ENUMERAL_TYPE && TYPE_NAME (ctype))
	  d_pushdecl (TYPE_NAME (ctype));
      }

    d->semanticRun = PASSobj;
  }

  /* Finish up a variable declaration and push it into the current scope.
     This can either be a static, local or manifest constant.  */

  void visit (VarDeclaration *d)
  {
    if (d->semanticRun >= PASSobj)
      return;

    if (d->type->ty == Terror)
      {
	error_at (make_location_t (d->loc),
		  "had semantic errors when compiling");
	return;
      }

    if (d->aliassym)
      {
	this->build_dsymbol (d->toAlias ());
	return;
      }

    /* Do not store variables we cannot take the address of,
       but keep the values for purposes of debugging.  */
    if (!d->canTakeAddressOf ())
      {
	/* Don't know if there is a good way to handle instantiations.  */
	if (d->isInstantiated ())
	  return;

	/* Cannot make an expression out of a void initializer.  */
	if (!d->_init || d->_init->isVoidInitializer ())
	  return;

	tree decl = get_symbol_decl (d);
	Expression *ie = initializerToExpression (d->_init);

	/* CONST_DECL was initially intended for enumerals and may be used for
	   scalars in general, but not for aggregates.  Here a non-constant
	   value is generated anyway so as the CONST_DECL only serves as a
	   placeholder for the value, however the DECL itself should never be
	   referenced in any generated code, or passed to the back-end.  */
	if (!d->type->isscalar ())
	  DECL_INITIAL (decl) = build_expr (ie, false);
	else
	  {
	    DECL_INITIAL (decl) = build_expr (ie, true);
	    d_pushdecl (decl);
	    rest_of_decl_compilation (decl, 1, 0);
	  }
      }
    else if (d->isDataseg () && !(d->storage_class & STCextern))
      {
	tree decl = get_symbol_decl (d);

	/* Duplicated VarDeclarations map to the same symbol.  Check if this
	   is the one declaration which will be emitted.  */
	tree ident = DECL_ASSEMBLER_NAME (decl);
	if (IDENTIFIER_DSYMBOL (ident) && IDENTIFIER_DSYMBOL (ident) != d)
	  return;

	/* How big a symbol can be should depend on back-end.  */
	tree size = build_integer_cst (d->type->size (d->loc),
				       build_ctype (Type::tsize_t));
	if (!valid_constant_size_p (size))
	  {
	    error_at (make_location_t (d->loc), "size is too large");
	    return;
	  }

	if (d->_init)
	  {
	    /* Use the explicit initializer, this includes `void`.  */
	    if (!d->_init->isVoidInitializer ())
	      {
		Expression *e = initializerToExpression (d->_init, d->type);
		DECL_INITIAL (decl) = build_expr (e, true);
	      }
	  }
	else
	  {
	    /* Use default initializer for the type.  */
	    if (TypeStruct *ts = d->type->isTypeStruct ())
	      DECL_INITIAL (decl) = layout_struct_initializer (ts->sym);
	    else
	      {
		Expression *e = d->type->defaultInitLiteral (d->loc);
		DECL_INITIAL (decl) = build_expr (e, true);
	      }
	  }

	/* Frontend should have already caught this.  */
	gcc_assert (!integer_zerop (size)
		    || d->type->toBasetype ()->ty == Tsarray);

	d_finish_decl (decl);

	/* Maybe record the var against the current module.  */
	register_module_decl (d);
      }
    else if (!d->isDataseg () && !d->isMember ())
      {
	/* This is needed for VarDeclarations in mixins that are to be local
	   variables of a function.  Otherwise, it would be enough to make
	   a check for isVarDeclaration() in DeclarationExp codegen.  */
	declare_local_var (d);

	if (d->_init)
	  {
	    tree decl = get_symbol_decl (d);

	    if (!d->_init->isVoidInitializer ())
	      {
		ExpInitializer *vinit = d->_init->isExpInitializer ();
		Expression *ie = initializerToExpression (vinit);
		tree exp = build_expr (ie);

		/* Maybe put variable on list of things needing destruction.  */
		if (d->needsScopeDtor ())
		  {
		    vec_safe_push (d_function_chain->vars_in_scope, decl);
		    /* Force a TARGET_EXPR to add the corresponding cleanup.  */
		    exp = force_target_expr (compound_expr (exp, decl));
		    TARGET_EXPR_CLEANUP (exp) = build_expr (d->edtor);
		  }

		add_stmt (exp);
	      }
	    else if (d->size (d->loc) != 0)
	      {
		/* Zero-length arrays do not have an initializer.  */
		warning (OPT_Wuninitialized, "uninitialized variable '%s'",
			 d->ident ? d->ident->toChars () : "(no name)");
	      }
	  }
      }

    d->semanticRun = PASSobj;
  }

  /* Generate and compile a static TypeInfo declaration, but only if it is
     needed in the current compilation.  */

  void visit (TypeInfoDeclaration *d)
  {
    if (d->semanticRun >= PASSobj)
      return;

    if (speculative_type_p (d->tinfo))
      return;

    tree t = get_typeinfo_decl (d);
    DECL_INITIAL (t) = layout_typeinfo (d);
    d_finish_decl (t);
    d->semanticRun = PASSobj;
  }

  /* Finish up a function declaration and compile it all the way
     down to assembler language output.  */

  void visit (FuncDeclaration *d)
  {
    /* Already generated the function.  */
    if (d->semanticRun >= PASSobj)
      return;

    /* Don't emit any symbols from gcc.attribute module.  */
    if (gcc_attribute_p (d))
      return;

    /* Not emitting unittest functions.  */
    if (!global.params.useUnitTests && d->isUnitTestDeclaration ())
      return;

    /* Check if any errors occurred when running semantic.  */
    if (TypeFunction *tf = d->type->isTypeFunction ())
      {
	if (tf->next == NULL || tf->next->ty == Terror)
	  return;
      }

    if (d->semantic3Errors)
      return;

    if (d->isNested ())
      {
	FuncDeclaration *fdp = d;
	while (fdp && fdp->isNested ())
	  {
	    fdp = fdp->toParent2 ()->isFuncDeclaration ();

	    if (fdp == NULL)
	      break;

	    /* Parent failed to compile, but errors were gagged.  */
	    if (fdp->semantic3Errors)
	      return;
	  }
      }

    /* Ensure all semantic passes have run.  */
    if (d->semanticRun < PASSsemantic3)
      {
	d->functionSemantic3 ();
	Module::runDeferredSemantic3 ();
      }

    if (global.errors)
      return;

    /* Duplicated FuncDeclarations map to the same symbol.  Check if this
       is the one declaration which will be emitted.  */
    tree fndecl = get_symbol_decl (d);
    tree ident = DECL_ASSEMBLER_NAME (fndecl);
    if (IDENTIFIER_DSYMBOL (ident) && IDENTIFIER_DSYMBOL (ident) != d)
      return;

    if (!d->fbody)
      {
	rest_of_decl_compilation (fndecl, 1, 0);
	return;
      }

    if (global.params.verbose)
      message ("function  %s", d->toPrettyChars ());

    /* Start generating code for this function.  */
    gcc_assert (d->semanticRun == PASSsemantic3done);
    d->semanticRun = PASSobj;

    tree old_context = start_function (d);

    tree parm_decl = NULL_TREE;
    tree param_list = NULL_TREE;

    /* Special arguments...  */

    /* `this' parameter:
       For nested functions, D still generates a vthis, but it
       should not be referenced in any expression.  */
    if (d->vthis)
      {
	parm_decl = get_symbol_decl (d->vthis);
	DECL_ARTIFICIAL (parm_decl) = 1;
	TREE_READONLY (parm_decl) = 1;

	if (d->vthis->type == Type::tvoidptr)
	  {
	    /* Replace generic pointer with back-end closure type
	       (this wins for gdb).  */
	    tree frame_type = FRAMEINFO_TYPE (get_frameinfo (d));
	    gcc_assert (frame_type != NULL_TREE);
	    TREE_TYPE (parm_decl) = build_pointer_type (frame_type);
	  }

	param_list = chainon (param_list, parm_decl);
	d_function_chain->static_chain = parm_decl;
      }

    /* _arguments parameter.  */
    if (d->v_arguments)
      {
	parm_decl = get_symbol_decl (d->v_arguments);
	param_list = chainon (param_list, parm_decl);
      }

    /* formal function parameters.  */
    size_t n_parameters = d->parameters ? d->parameters->length : 0;

    for (size_t i = 0; i < n_parameters; i++)
      {
	VarDeclaration *param = (*d->parameters)[i];
	parm_decl = get_symbol_decl (param);
	/* Chain them in the correct order.  */
	param_list = chainon (param_list, parm_decl);
      }

    DECL_ARGUMENTS (fndecl) = param_list;
    DECL_IN_UNITTEST_CONDITION_P (fndecl) = this->in_version_unittest_;
    rest_of_decl_compilation (fndecl, 1, 0);

    /* If this is a member function that nested (possibly indirectly) in another
       function, construct an expession for this member function's static chain
       by going through parent link of nested classes.  */
    if (d->isThis ())
      {
	AggregateDeclaration *ad = d->isThis ();
	tree this_tree = get_symbol_decl (d->vthis);

	while (ad->isNested ())
	  {
	    Dsymbol *pd = ad->toParent2 ();
	    tree vthis_field = get_symbol_decl (ad->vthis);
	    this_tree = component_ref (build_deref (this_tree), vthis_field);

	    ad = pd->isAggregateDeclaration ();
	    if (ad == NULL)
	      {
		cfun->language->static_chain = this_tree;
		break;
	      }
	  }
      }

    /* May change cfun->static_chain.  */
    build_closure (d);

    if (d->vresult)
      declare_local_var (d->vresult);

    if (d->v_argptr)
      push_stmt_list ();

    /* Named return value optimisation support for D.
       Implemented by overriding all the RETURN_EXPRs and replacing all
       occurrences of VAR with the RESULT_DECL for the function.
       This is only worth doing for functions that can return in memory.  */
    if (d->nrvo_can)
      {
	tree restype = TREE_TYPE (DECL_RESULT (fndecl));

	if (!AGGREGATE_TYPE_P (restype))
	  d->nrvo_can = 0;
	else
	  d->nrvo_can = aggregate_value_p (restype, fndecl);
      }

    if (d->nrvo_can)
      {
	tree resdecl = DECL_RESULT (fndecl);

	TREE_TYPE (resdecl)
	  = build_reference_type (TREE_TYPE (resdecl));
	DECL_BY_REFERENCE (resdecl) = 1;
	TREE_ADDRESSABLE (resdecl) = 0;
	relayout_decl (resdecl);

	if (d->nrvo_var)
	  {
	    tree var = get_symbol_decl (d->nrvo_var);

	    /* Copy name from VAR to RESULT.  */
	    DECL_NAME (resdecl) = DECL_NAME (var);
	    /* Don't forget that we take its address.  */
	    TREE_ADDRESSABLE (var) = 1;
	    resdecl = build_deref (resdecl);

	    SET_DECL_VALUE_EXPR (var, resdecl);
	    DECL_HAS_VALUE_EXPR_P (var) = 1;
	    SET_DECL_LANG_NRVO (var, resdecl);
	  }
      }

    build_function_body (d);

    /* Initialize the _argptr variable.  */
    if (d->v_argptr)
      {
	tree body = pop_stmt_list ();
	tree var = get_decl_tree (d->v_argptr);
	var = build_address (var);

	tree init = build_call_expr (builtin_decl_explicit (BUILT_IN_VA_START),
				     2, var, parm_decl);
	declare_local_var (d->v_argptr);
	add_stmt (init);

	tree cleanup = build_call_expr (builtin_decl_explicit (BUILT_IN_VA_END),
					1, var);
	add_stmt (build2 (TRY_FINALLY_EXPR, void_type_node, body, cleanup));
      }

    finish_function (old_context);

    /* Maybe record the function against the current module.  */
    register_module_decl (d);
  }
};

/* Main entry point for the DeclVisitor interface to send
   the Declaration AST class D to GCC back-end.  */

void
build_decl_tree (Dsymbol *d)
{
  location_t saved_location = input_location;

  /* Set input location, empty DECL_SOURCE_FILE can crash debug generator.  */
  if (d->loc.filename)
    input_location = make_location_t (d->loc);
  else
    input_location = make_location_t (Loc ("<no_file>", 1, 0));

  DeclVisitor v = DeclVisitor ();
  v.build_dsymbol (d);

  input_location = saved_location;
}

/* Return the decl for the symbol, create it if it doesn't already exist.  */

tree
get_symbol_decl (Declaration *decl)
{
  if (decl->csym)
    return decl->csym;

  /* Deal with placeholder symbols immediately:
     SymbolDeclaration is used as a shell around an initializer symbol.  */
  SymbolDeclaration *sd = decl->isSymbolDeclaration ();
  if (sd)
    {
      decl->csym = aggregate_initializer_decl (sd->dsym);
      return decl->csym;
    }

  /* Global static TypeInfo declaration.  */
  if (decl->isTypeInfoDeclaration ())
    return get_typeinfo_decl ((TypeInfoDeclaration *) decl);

  /* FuncAliasDeclaration is used to import functions from another scope.  */
  FuncAliasDeclaration *fad = decl->isFuncAliasDeclaration ();
  if (fad)
    {
      decl->csym = get_symbol_decl (fad->funcalias);
      return decl->csym;
    }

  /* It is possible for a field declaration symbol to be requested
     before the parent type has been built.  */
  if (decl->isField ())
    {
      AggregateDeclaration *ad = decl->toParent ()->isAggregateDeclaration ();
      gcc_assert (ad != NULL);

      /* Finishing off the type should create the associated FIELD_DECL.  */
      build_ctype (ad->type);
      gcc_assert (decl->csym != NULL);

      return decl->csym;
    }

  /* Build the tree for the symbol.  */
  FuncDeclaration *fd = decl->isFuncDeclaration ();
  if (fd)
    {
      /* Run full semantic on functions we need to know about.  */
      if (!fd->functionSemantic ())
	{
	  decl->csym = error_mark_node;
	  return decl->csym;
	}

      decl->csym = build_decl (make_location_t (decl->loc), FUNCTION_DECL,
			       get_identifier (decl->ident->toChars ()),
			       NULL_TREE);

      /* Set function type afterwards as there could be self references.  */
      TREE_TYPE (decl->csym) = build_ctype (fd->type);

      /* Set DECL_INITIAL now if the function has a definition.  */
      if (fd->fbody)
	DECL_INITIAL (decl->csym) = error_mark_node;
      else
	DECL_EXTERNAL (decl->csym) = 1;
    }
  else
    {
      /* Build the variable declaration.  */
      VarDeclaration *vd = decl->isVarDeclaration ();
      gcc_assert (vd != NULL);

      tree_code code = vd->isParameter () ? PARM_DECL
	: !vd->canTakeAddressOf () ? CONST_DECL
	: VAR_DECL;
      decl->csym = build_decl (make_location_t (decl->loc), code,
			       get_identifier (decl->ident->toChars ()),
			       declaration_type (vd));

      /* If any alignment was set on the declaration.  */
      if (vd->alignment != STRUCTALIGN_DEFAULT)
	{
	  SET_DECL_ALIGN (decl->csym, vd->alignment * BITS_PER_UNIT);
	  DECL_USER_ALIGN (decl->csym) = 1;
	}

      if (vd->storage_class & STCextern)
	DECL_EXTERNAL (decl->csym) = 1;
    }

  /* Set the declaration mangled identifier if static.  */
  if (decl->isCodeseg () || decl->isDataseg ())
    {
      tree mangled_name;

      if (decl->mangleOverride.length)
	{
	  mangled_name =
	    get_identifier_with_length (decl->mangleOverride.ptr,
					decl->mangleOverride.length);
	}
      else
	mangled_name = get_identifier (d_mangle_decl (decl));

      mangled_name = targetm.mangle_decl_assembler_name (decl->csym,
							 mangled_name);
      /* The frontend doesn't handle duplicate definitions of unused symbols
	 with the same mangle.  So a check is done here instead.  */
      if (IDENTIFIER_DSYMBOL (mangled_name))
	{
	  Declaration *other = IDENTIFIER_DSYMBOL (mangled_name);
	  tree olddecl = decl->csym;
	  decl->csym = get_symbol_decl (other);

	  /* The current declaration is a prototype or marked extern, merge
	     applied user attributes and return.  */
	  if (DECL_EXTERNAL (olddecl) && !DECL_INITIAL (olddecl))
	    {
	      apply_user_attributes (decl, decl->csym);
	      return decl->csym;
	    }
	  /* The previous declaration is a prototype or marked extern, set the
	     current declaration as the main reference of the symbol.  */
	  else if (DECL_EXTERNAL (decl->csym) && !DECL_INITIAL (decl->csym))
	    {
	      IDENTIFIER_DSYMBOL (mangled_name) = decl;
	      DECL_EXTERNAL (decl->csym) = 0;
	    }
	  /* Non-extern, non-templated decls shouldn't be defined twice.  */
	  else if (!decl->isInstantiated ())
	    ScopeDsymbol::multiplyDefined (decl->loc, decl, other);
	}
      else
	{
	  IDENTIFIER_PRETTY_NAME (mangled_name)
	    = get_identifier (decl->toPrettyChars (true));
	  IDENTIFIER_DSYMBOL (mangled_name) = decl;

	  SET_DECL_ASSEMBLER_NAME (decl->csym, mangled_name);
	}
    }

  DECL_LANG_SPECIFIC (decl->csym) = build_lang_decl (decl);
  DECL_CONTEXT (decl->csym) = d_decl_context (decl);

  if (TREE_CODE (decl->csym) == PARM_DECL)
    {
      /* Pass non-trivial structs by invisible reference.  */
      if (TREE_ADDRESSABLE (TREE_TYPE (decl->csym)))
	{
	  tree argtype = build_reference_type (TREE_TYPE (decl->csym));
	  argtype = build_qualified_type (argtype, TYPE_QUAL_RESTRICT);
	  gcc_assert (!DECL_BY_REFERENCE (decl->csym));
	  TREE_TYPE (decl->csym) = argtype;
	  DECL_BY_REFERENCE (decl->csym) = 1;
	  TREE_ADDRESSABLE (decl->csym) = 0;
	  relayout_decl (decl->csym);
	  decl->storage_class |= STCref;
	}

      DECL_ARG_TYPE (decl->csym) = TREE_TYPE (decl->csym);
      gcc_assert (TREE_CODE (DECL_CONTEXT (decl->csym)) == FUNCTION_DECL);
    }
  else if (TREE_CODE (decl->csym) == CONST_DECL)
    {
      /* Manifest constants have no address in memory.  */
      TREE_CONSTANT (decl->csym) = 1;
      TREE_READONLY (decl->csym) = 1;
    }
  else if (TREE_CODE (decl->csym) == FUNCTION_DECL)
    {
      /* The real function type may differ from its declaration.  */
      tree fntype = TREE_TYPE (decl->csym);
      tree newfntype = NULL_TREE;

      if (fd->isNested ())
	{
	  /* Add an extra argument for the frame/closure pointer, this is also
	     required to be compatible with D delegates.  */
	  newfntype = build_vthis_function (void_type_node, fntype);
	}
      else if (fd->isThis ())
	{
	  /* Add an extra argument for the `this' parameter.  The handle type is
	     used even if there is no debug info.  It is needed to make sure
	     virtual member functions are not called statically.  */
	  AggregateDeclaration *ad = fd->isMember2 ();
	  tree handle = build_ctype (ad->handleType ());

	  /* If handle is a pointer type, get record type.  */
	  if (!ad->isStructDeclaration ())
	    handle = TREE_TYPE (handle);

	  newfntype = build_vthis_function (handle, fntype);

	  /* Set the vindex on virtual functions.  */
	  if (fd->isVirtual () && fd->vtblIndex != -1)
	    {
	      DECL_VINDEX (decl->csym) = size_int (fd->vtblIndex);
	      DECL_VIRTUAL_P (decl->csym) = 1;
	    }
	}
      else if (fd->isMain () || fd->isCMain ())
	{
	  /* The main function is named `D main' to distinguish from C main.  */
	  if (fd->isMain ())
	    DECL_NAME (decl->csym) = get_identifier (fd->toPrettyChars (true));

	  /* `void main' is implicitly converted to returning an int.  */
	  newfntype = build_function_type (d_int_type, TYPE_ARG_TYPES (fntype));
	}

      if (newfntype != NULL_TREE)
	{
	  /* Copy the old attributes from the original type.  */
	  TYPE_ATTRIBUTES (newfntype) = TYPE_ATTRIBUTES (fntype);
	  TYPE_LANG_SPECIFIC (newfntype) = TYPE_LANG_SPECIFIC (fntype);
	  TREE_ADDRESSABLE (newfntype) = TREE_ADDRESSABLE (fntype);
	  TREE_TYPE (decl->csym) = newfntype;
	  d_keep (newfntype);
	}

      /* Miscellaneous function flags.  */

      /* In [pragma/inline], functions decorated with `pragma(inline)' affects
	 whether they are inlined or not.  */
      if (fd->inlining == PINLINEalways)
	DECL_DECLARED_INLINE_P (decl->csym) = 1;
      else if (fd->inlining == PINLINEnever)
	DECL_UNINLINABLE (decl->csym) = 1;

      /* Function was declared `naked'.  */
      if (fd->naked)
	{
	  insert_decl_attribute (decl->csym, "naked");
	  DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (decl->csym) = 1;
	}

      /* Vector array operations are always compiler generated.  */
      if (fd->isArrayOp)
	{
	  TREE_PUBLIC (decl->csym) = 1;
	  DECL_ARTIFICIAL (decl->csym) = 1;
	  DECL_DECLARED_INLINE_P (decl->csym) = 1;
	  d_comdat_linkage (decl->csym);
	}

      /* And so are ensure and require contracts.  */
      if (fd->ident == Identifier::idPool ("ensure")
	  || fd->ident == Identifier::idPool ("require"))
	{
	  DECL_ARTIFICIAL (decl->csym) = 1;
	  TREE_PUBLIC (decl->csym) = 1;
	}

      if (decl->storage_class & STCfinal)
	DECL_FINAL_P (decl->csym) = 1;

      /* Check whether this function is expanded by the frontend.  */
      DECL_INTRINSIC_CODE (decl->csym) = INTRINSIC_NONE;
      maybe_set_intrinsic (fd);

      /* For nested functions in particular, unnest fndecl in the cgraph, as
	 all static chain passing is handled by the front-end.  Do this even
	 if we are not emitting the body.  */
      struct cgraph_node *node = cgraph_node::get_create (decl->csym);
      if (node->origin)
	node->unnest ();
    }

  /* Mark compiler generated temporaries as artificial.  */
  if (decl->storage_class & STCtemp)
    DECL_ARTIFICIAL (decl->csym) = 1;

  /* Propagate shared on the decl.  */
  if (TYPE_SHARED (TREE_TYPE (decl->csym)))
    TREE_ADDRESSABLE (decl->csym) = 1;

  /* Symbol was marked volatile.  */
  if (decl->storage_class & STCvolatile)
    TREE_THIS_VOLATILE (decl->csym) = 1;

  /* Protection attributes are used by the debugger.  */
  if (decl->protection.kind == Prot::private_)
    TREE_PRIVATE (decl->csym) = 1;
  else if (decl->protection.kind == Prot::protected_)
    TREE_PROTECTED (decl->csym) = 1;

  /* Likewise, so could the deprecated attribute.  */
  if (decl->storage_class & STCdeprecated)
    TREE_DEPRECATED (decl->csym) = 1;

#if TARGET_DLLIMPORT_DECL_ATTRIBUTES
  /* Have to test for import first.  */
  if (decl->isImportedSymbol ())
    {
      insert_decl_attribute (decl->csym, "dllimport");
      DECL_DLLIMPORT_P (decl->csym) = 1;
    }
  else if (decl->isExport ())
    insert_decl_attribute (decl->csym, "dllexport");
#endif

  if (decl->isDataseg () || decl->isCodeseg () || decl->isThreadlocal ())
    {
      /* Set TREE_PUBLIC by default, but allow private template to override.  */
      if (!fd || !fd->isNested ())
	TREE_PUBLIC (decl->csym) = 1;

      TREE_STATIC (decl->csym) = 1;
      /* The decl has not been defined -- yet.  */
      DECL_EXTERNAL (decl->csym) = 1;

      if (decl->isInstantiated ())
	d_linkonce_linkage (decl->csym);
    }

  /* Symbol is going in thread local storage.  */
  if (decl->isThreadlocal () && !DECL_ARTIFICIAL (decl->csym))
    {
      if (global.params.vtls)
	message (decl->loc, "`%s` is thread local", decl->toChars ());

      set_decl_tls_model (decl->csym, decl_default_tls_model (decl->csym));
    }

  /* Apply any user attributes that may affect semantic meaning.  */
  apply_user_attributes (decl, decl->csym);

  /* %% Probably should be a little more intelligent about setting this.  */
  TREE_USED (decl->csym) = 1;
  d_keep (decl->csym);

  return decl->csym;
}

/* Returns a declaration for a VAR_DECL.  Used to create compiler-generated
   global variables.  */

tree
declare_extern_var (tree ident, tree type)
{
  /* If the VAR_DECL has already been declared, return it.  */
  if (IDENTIFIER_DECL_TREE (ident))
    return IDENTIFIER_DECL_TREE (ident);

  tree name = IDENTIFIER_PRETTY_NAME (ident)
    ? IDENTIFIER_PRETTY_NAME (ident) : ident;
  tree decl = build_decl (input_location, VAR_DECL, name, type);

  IDENTIFIER_DECL_TREE (ident) = decl;
  d_keep (decl);

  SET_DECL_ASSEMBLER_NAME (decl, ident);
  DECL_ARTIFICIAL (decl) = 1;
  TREE_STATIC (decl) = 1;
  TREE_PUBLIC (decl) = 1;

  /* The decl has not been defined -- yet.  */
  DECL_EXTERNAL (decl) = 1;

  return decl;
}

/* Add local variable VAR into the current function body.  */

void
declare_local_var (VarDeclaration *var)
{
  gcc_assert (!var->isDataseg () && !var->isMember ());
  gcc_assert (current_function_decl != NULL_TREE);

  FuncDeclaration *fd = cfun->language->function;
  tree decl = get_symbol_decl (var);

  gcc_assert (!TREE_STATIC (decl));
  d_pushdecl (decl);
  DECL_CONTEXT (decl) = current_function_decl;

  /* Compiler generated symbols.  */
  if (var == fd->vresult || var == fd->v_argptr)
    DECL_ARTIFICIAL (decl) = 1;

  if (DECL_LANG_FRAME_FIELD (decl))
    {
      /* Fixes debugging local variables.  */
      SET_DECL_VALUE_EXPR (decl, get_decl_tree (var));
      DECL_HAS_VALUE_EXPR_P (decl) = 1;
    }
}

/* Return an unnamed local temporary of type TYPE.  */

tree
build_local_temp (tree type)
{
  tree decl = build_decl (input_location, VAR_DECL, NULL_TREE, type);

  DECL_CONTEXT (decl) = current_function_decl;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  d_pushdecl (decl);

  return decl;
}

/* Return the correct decl to be used for DECL.  For VAR_DECLs, this could
   instead be a FIELD_DECL from a closure, or a RESULT_DECL from a named return
   value.  For PARM_DECLs, this could be a FIELD_DECL for a non-local `this'.
   For all other kinds of decls, this just returns the result of
   get_symbol_decl().  */

tree
get_decl_tree (Declaration *decl)
{
  tree t = get_symbol_decl (decl);
  FuncDeclaration *fd = cfun ? cfun->language->function : NULL;
  VarDeclaration *vd = decl->isVarDeclaration ();

  /* If cfun is NULL, then this is a global static.  */
  if (vd == NULL || fd == NULL)
    return t;

  /* Get the named return value.  */
  if (DECL_LANG_NRVO (t))
    return DECL_LANG_NRVO (t);

  /* Get the closure holding the var decl.  */
  if (DECL_LANG_FRAME_FIELD (t))
    {
      FuncDeclaration *parent = vd->toParent2 ()->isFuncDeclaration ();
      tree frame_ref = get_framedecl (fd, parent);

      return component_ref (build_deref (frame_ref),
			    DECL_LANG_FRAME_FIELD (t));
    }

  /* Get the non-local `this' value by going through parent link
     of nested classes, this routine pretty much undoes what
     getRightThis in the frontend removes from codegen.  */
  if (vd->parent != fd && vd->isThisDeclaration ())
    {
      /* Find the first parent that is a member function.  */
      while (!fd->isMember2 ())
	{
	  gcc_assert (fd->vthis);
	  fd = fd->toParent2 ()->isFuncDeclaration ();
	  gcc_assert (fd != NULL);
	}

      AggregateDeclaration *ad = fd->isThis ();
      gcc_assert (ad != NULL);

      t = get_decl_tree (fd->vthis);
      Dsymbol *outer = fd;

      while (outer != vd->parent)
	{
	  gcc_assert (ad != NULL);
	  outer = ad->toParent2 ();

	  /* Get the this->this parent link.  */
	  tree vfield = get_symbol_decl (ad->vthis);
	  t = component_ref (build_deref (t), vfield);

	  ad = outer->isAggregateDeclaration ();
	  if (ad != NULL)
	    continue;

	  fd = outer->isFuncDeclaration ();
	  while (fd != NULL)
	    {
	      /* If outer function creates a closure, then the `this'
		 value would be the closure pointer, and the real
		 `this' the first field of that closure.  */
	      tree ff = get_frameinfo (fd);
	      if (FRAMEINFO_CREATES_FRAME (ff))
		{
		  t = build_nop (build_pointer_type (FRAMEINFO_TYPE (ff)), t);
		  t = indirect_ref (build_ctype (fd->vthis->type), t);
		}

	      if (fd == vd->parent)
		break;

	      /* Continue looking for the right `this'.  */
	      outer = outer->toParent2 ();
	      fd = outer->isFuncDeclaration ();
	    }

	  ad = outer->isAggregateDeclaration ();
	}

      return t;
    }

  /* Auto variable that the back end will handle for us.  */
  return t;
}

/* Finish up a variable declaration and compile it all the way to
   the assembler language output.  */

void
d_finish_decl (tree decl)
{
  gcc_assert (!error_operand_p (decl));

  /* We are sending this symbol to object file, can't be extern.  */
  TREE_STATIC (decl) = 1;
  DECL_EXTERNAL (decl) = 0;

  /* Update the TLS model as the linkage has been modified.  */
  if (DECL_THREAD_LOCAL_P (decl))
    set_decl_tls_model (decl, decl_default_tls_model (decl));

  relayout_decl (decl);

  if (flag_checking && DECL_INITIAL (decl))
    {
      /* Initializer must never be bigger than symbol size.  */
      dinteger_t tsize = int_size_in_bytes (TREE_TYPE (decl));
      dinteger_t dtsize = int_size_in_bytes (TREE_TYPE (DECL_INITIAL (decl)));

      if (tsize < dtsize)
	{
	  tree name = DECL_ASSEMBLER_NAME (decl);

	  internal_error ("Mismatch between declaration %qE size (%wd) and "
			  "its initializer size (%wd).",
			  IDENTIFIER_PRETTY_NAME (name)
			  ? IDENTIFIER_PRETTY_NAME (name) : name,
			  tsize, dtsize);
	}
    }

  /* Without weak symbols, symbol should be put in .common, but that can't
     be done if there is a nonzero initializer.  */
  if (DECL_COMDAT (decl) && DECL_COMMON (decl)
      && initializer_zerop (DECL_INITIAL (decl)))
    DECL_INITIAL (decl) = error_mark_node;

  /* Add this decl to the current binding level.  */
  d_pushdecl (decl);

  rest_of_decl_compilation (decl, 1, 0);
}

/* Thunk code is based on g++.  */

static int thunk_labelno;

/* Create a static alias to function.  */

static tree
make_alias_for_thunk (tree function)
{
  tree alias;
  char buf[256];

  /* Thunks may reference extern functions which cannot be aliased.  */
  if (DECL_EXTERNAL (function))
    return function;

  targetm.asm_out.generate_internal_label (buf, "LTHUNK", thunk_labelno);
  thunk_labelno++;

  alias = build_decl (DECL_SOURCE_LOCATION (function), FUNCTION_DECL,
		      get_identifier (buf), TREE_TYPE (function));
  DECL_LANG_SPECIFIC (alias) = DECL_LANG_SPECIFIC (function);
  lang_hooks.dup_lang_specific_decl (alias);
  DECL_CONTEXT (alias) = NULL_TREE;
  TREE_READONLY (alias) = TREE_READONLY (function);
  TREE_THIS_VOLATILE (alias) = TREE_THIS_VOLATILE (function);
  TREE_PUBLIC (alias) = 0;

  DECL_EXTERNAL (alias) = 0;
  DECL_ARTIFICIAL (alias) = 1;

  DECL_DECLARED_INLINE_P (alias) = 0;
  DECL_INITIAL (alias) = error_mark_node;
  DECL_ARGUMENTS (alias) = copy_list (DECL_ARGUMENTS (function));

  TREE_ADDRESSABLE (alias) = 1;
  TREE_USED (alias) = 1;
  SET_DECL_ASSEMBLER_NAME (alias, DECL_NAME (alias));

  if (!flag_syntax_only)
    {
      cgraph_node *aliasn;
      aliasn = cgraph_node::create_same_body_alias (alias, function);
      gcc_assert (aliasn != NULL);
    }
  return alias;
}

/* Emit the definition of a D vtable thunk.  */

static void
finish_thunk (tree thunk, tree function)
{
  /* Setup how D thunks are outputted.  */
  int fixed_offset = -THUNK_LANG_OFFSET (thunk);
  bool this_adjusting = true;
  tree alias;

  if (TARGET_USE_LOCAL_THUNK_ALIAS_P (function))
    alias = make_alias_for_thunk (function);
  else
    alias = function;

  TREE_ADDRESSABLE (function) = 1;
  TREE_USED (function) = 1;

  if (flag_syntax_only)
    {
      TREE_ASM_WRITTEN (thunk) = 1;
      return;
    }

  if (TARGET_USE_LOCAL_THUNK_ALIAS_P (function)
      && targetm_common.have_named_sections)
    {
      tree fn = function;
      symtab_node *symbol = symtab_node::get (function);

      if (symbol != NULL && symbol->alias)
	{
	  if (symbol->analyzed)
	    fn = symtab_node::get (function)->ultimate_alias_target ()->decl;
	  else
	    fn = symtab_node::get (function)->alias_target;
	}
      resolve_unique_section (fn, 0, flag_function_sections);

      if (DECL_SECTION_NAME (fn) != NULL && DECL_ONE_ONLY (fn))
	{
	  resolve_unique_section (thunk, 0, flag_function_sections);

	  /* Output the thunk into the same section as function.  */
	  set_decl_section_name (thunk, DECL_SECTION_NAME (fn));
	  symtab_node::get (thunk)->implicit_section
	    = symtab_node::get (fn)->implicit_section;
	}
    }

  /* Set up cloned argument trees for the thunk.  */
  tree t = NULL_TREE;
  for (tree a = DECL_ARGUMENTS (function); a; a = DECL_CHAIN (a))
    {
      tree x = copy_node (a);
      DECL_CHAIN (x) = t;
      DECL_CONTEXT (x) = thunk;
      SET_DECL_RTL (x, NULL);
      DECL_HAS_VALUE_EXPR_P (x) = 0;
      TREE_ADDRESSABLE (x) = 0;
      t = x;
    }
  DECL_ARGUMENTS (thunk) = nreverse (t);
  TREE_ASM_WRITTEN (thunk) = 1;

  cgraph_node *funcn, *thunk_node;

  funcn = cgraph_node::get_create (function);
  gcc_assert (funcn);
  thunk_node = funcn->create_thunk (thunk, thunk, this_adjusting,
				    fixed_offset, 0, 0, 0, alias);

  if (DECL_ONE_ONLY (function))
    thunk_node->add_to_same_comdat_group (funcn);

  /* Target assemble_mi_thunk doesn't work across section boundaries
     on many targets, instead force thunk to be expanded in gimple.  */
  if (DECL_EXTERNAL (function))
    {
      /* cgraph::expand_thunk writes over current_function_decl, so if this
	 could ever be in use by the codegen pass, we want to know about it.  */
      gcc_assert (current_function_decl == NULL_TREE);

      if (!stdarg_p (TREE_TYPE (thunk)))
	{
	  thunk_node->create_edge (funcn, NULL, thunk_node->count);
	  thunk_node->expand_thunk (false, true);
	}

      /* Tell the back-end to not bother inlining the function, this is
	 assumed not to work as it could be referencing symbols outside
	 of the current compilation unit.  */
      DECL_UNINLINABLE (function) = 1;
    }
}

/* Return a thunk to DECL.  Thunks adjust the incoming `this' pointer by OFFSET.
   Adjustor thunks are created and pointers to them stored in the method entries
   in the vtable in order to set the this pointer to the start of the object
   instance corresponding to the implementing method.  */

tree
make_thunk (FuncDeclaration *decl, int offset)
{
  tree function = get_symbol_decl (decl);

  if (!DECL_ARGUMENTS (function) || !DECL_RESULT (function))
    {
      /* Compile the function body before generating the thunk, this is done
	 even if the decl is external to the current module.  */
      if (decl->fbody)
	build_decl_tree (decl);
      else
	{
	  /* Build parameters for functions that are not being compiled,
	     so that they can be correctly cloned in finish_thunk.  */
	  tree fntype = TREE_TYPE (function);
	  tree params = NULL_TREE;

	  for (tree t = TYPE_ARG_TYPES (fntype); t; t = TREE_CHAIN (t))
	    {
	      if (t == void_list_node)
		break;

	      tree param = build_decl (DECL_SOURCE_LOCATION (function),
				       PARM_DECL, NULL_TREE, TREE_VALUE (t));
	      DECL_ARG_TYPE (param) = TREE_TYPE (param);
	      DECL_ARTIFICIAL (param) = 1;
	      DECL_IGNORED_P (param) = 1;
	      DECL_CONTEXT (param) = function;
	      params = chainon (params, param);
	    }

	  DECL_ARGUMENTS (function) = params;

	  /* Also build the result decl, which is needed when force creating
	     the thunk in gimple inside cgraph_node::expand_thunk.  */
	  tree resdecl = build_decl (DECL_SOURCE_LOCATION (function),
				     RESULT_DECL, NULL_TREE,
				     TREE_TYPE (fntype));
	  DECL_ARTIFICIAL (resdecl) = 1;
	  DECL_IGNORED_P (resdecl) = 1;
	  DECL_CONTEXT (resdecl) = function;
	  DECL_RESULT (function) = resdecl;
	}
    }

  /* Don't build the thunk if the compilation step failed.  */
  if (global.errors)
    return error_mark_node;

  /* See if we already have the thunk in question.  */
  for (tree t = DECL_LANG_THUNKS (function); t; t = DECL_CHAIN (t))
    {
      if (THUNK_LANG_OFFSET (t) == offset)
	return t;
    }

  tree thunk = build_decl (DECL_SOURCE_LOCATION (function),
			   FUNCTION_DECL, NULL_TREE, TREE_TYPE (function));
  DECL_LANG_SPECIFIC (thunk) = DECL_LANG_SPECIFIC (function);
  lang_hooks.dup_lang_specific_decl (thunk);
  THUNK_LANG_OFFSET (thunk) = offset;

  TREE_READONLY (thunk) = TREE_READONLY (function);
  TREE_THIS_VOLATILE (thunk) = TREE_THIS_VOLATILE (function);
  TREE_NOTHROW (thunk) = TREE_NOTHROW (function);

  DECL_CONTEXT (thunk) = d_decl_context (decl);

  /* Thunks inherit the public access of the function they are targetting.
     When the function is outside the current compilation unit however, then the
     thunk must be kept private to not conflict.  */
  TREE_PUBLIC (thunk) = TREE_PUBLIC (function) && !DECL_EXTERNAL (function);

  DECL_EXTERNAL (thunk) = 0;

  /* Thunks are always addressable.  */
  TREE_ADDRESSABLE (thunk) = 1;
  TREE_USED (thunk) = 1;
  DECL_ARTIFICIAL (thunk) = 1;
  DECL_DECLARED_INLINE_P (thunk) = 0;

  DECL_VISIBILITY (thunk) = DECL_VISIBILITY (function);
  DECL_COMDAT (thunk) = DECL_COMDAT (function);
  DECL_WEAK (thunk) = DECL_WEAK (function);

  tree target_name = DECL_ASSEMBLER_NAME (function);
  unsigned identlen = IDENTIFIER_LENGTH (target_name) + 14;
  const char *ident = XNEWVEC (const char, identlen);
  snprintf (CONST_CAST (char *, ident), identlen,
	    "_DT%u%s", offset, IDENTIFIER_POINTER (target_name));

  DECL_NAME (thunk) = get_identifier (ident);
  SET_DECL_ASSEMBLER_NAME (thunk, DECL_NAME (thunk));

  d_keep (thunk);

  finish_thunk (thunk, function);

  /* Add it to the list of thunks associated with the function.  */
  DECL_LANG_THUNKS (thunk) = NULL_TREE;
  DECL_CHAIN (thunk) = DECL_LANG_THUNKS (function);
  DECL_LANG_THUNKS (function) = thunk;

  return thunk;
}

/* Create the FUNCTION_DECL for a function definition.
   This function creates a binding context for the function body
   as well as setting up the FUNCTION_DECL in current_function_decl.
   Returns the previous function context if it was already set.  */

tree
start_function (FuncDeclaration *fd)
{
  tree fndecl = get_symbol_decl (fd);

  /* Function has been defined, check now whether we intend to send it to
     object file, or it really is extern.  Such as inlinable functions from
     modules not in this compilation, or thunk aliases.  */
  TemplateInstance *ti = fd->isInstantiated ();
  if (ti && ti->needsCodegen ())
    {
      /* Warn about templates instantiated in this compilation.  */
      if (ti == fd->parent)
	{
	  warning (OPT_Wtemplates, "%s %qs instantiated",
		   ti->kind (), ti->toPrettyChars (false));
	}

      DECL_EXTERNAL (fndecl) = 0;
    }
  else
    {
      Module *md = fd->getModule ();
      if (md && md->isRoot ())
	DECL_EXTERNAL (fndecl) = 0;
    }

  DECL_INITIAL (fndecl) = error_mark_node;

  /* Add this decl to the current binding level.  */
  d_pushdecl (fndecl);

  /* Save the current function context.  */
  tree old_context = current_function_decl;

  if (old_context)
    push_function_context ();

  /* Let GCC know the current scope is this function.  */
  current_function_decl = fndecl;

  tree restype = TREE_TYPE (TREE_TYPE (fndecl));
  tree resdecl = build_decl (make_location_t (fd->loc), RESULT_DECL,
			     NULL_TREE, restype);

  DECL_RESULT (fndecl) = resdecl;
  DECL_CONTEXT (resdecl) = fndecl;
  DECL_ARTIFICIAL (resdecl) = 1;
  DECL_IGNORED_P (resdecl) = 1;

  /* Initialize the RTL code for the function.  */
  allocate_struct_function (fndecl, false);

  /* Store the end of the function.  */
  if (fd->endloc.filename)
    cfun->function_end_locus = make_location_t (fd->endloc);
  else
    cfun->function_end_locus = DECL_SOURCE_LOCATION (fndecl);

  cfun->language = ggc_cleared_alloc <language_function> ();
  cfun->language->function = fd;

  /* Default chain value is `null' unless parent found.  */
  cfun->language->static_chain = null_pointer_node;

  /* Find module for this function.  */
  for (Dsymbol *p = fd->parent; p != NULL; p = p->parent)
    {
      cfun->language->module = p->isModule ();
      if (cfun->language->module)
	break;
    }
  gcc_assert (cfun->language->module != NULL);

  /* Begin the statement tree for this function.  */
  push_stmt_list ();
  push_binding_level (level_function);

  return old_context;
}

/* Finish up a function declaration and compile that function all
   the way to assembler language output.  The free the storage for
   the function definition.  Restores the previous function context.  */

void
finish_function (tree old_context)
{
  tree fndecl = current_function_decl;

  /* Tie off the statement tree for this function.  */
  tree block = pop_binding_level ();
  tree body = pop_stmt_list ();
  tree bind = build3 (BIND_EXPR, void_type_node,
		      BLOCK_VARS (block), body, block);

  gcc_assert (vec_safe_is_empty (d_function_chain->stmt_list));

  /* Back-end expects a statement list to come from somewhere, however
     pop_stmt_list returns expressions when there is a single statement.
     So here we create a statement list unconditionally.  */
  if (TREE_CODE (body) != STATEMENT_LIST)
    {
      tree stmtlist = alloc_stmt_list ();
      append_to_statement_list_force (body, &stmtlist);
      BIND_EXPR_BODY (bind) = stmtlist;
    }
  else if (!STATEMENT_LIST_HEAD (body))
    {
      /* For empty functions add a void return.  */
      append_to_statement_list_force (return_expr (NULL_TREE), &body);
    }

  DECL_SAVED_TREE (fndecl) = bind;

  if (!errorcount && !global.errors)
    {
      /* Dump the D-specific tree IR.  */
      dump_function (TDI_original, fndecl);

      cgraph_node::finalize_function (fndecl, true);
    }

  /* We're leaving the context of this function, so free it.  */
  ggc_free (cfun->language);
  cfun->language = NULL;
  set_cfun (NULL);

  if (old_context)
    pop_function_context ();

  current_function_decl = old_context;
}

/* Mark DECL, which is a VAR_DECL or FUNCTION_DECL as a symbol that
   must be emitted in this, output module.  */

void
mark_needed (tree decl)
{
  TREE_USED (decl) = 1;

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      struct cgraph_node *node = cgraph_node::get_create (decl);
      node->forced_by_abi = true;
    }
  else if (VAR_P (decl))
    {
      struct varpool_node *node = varpool_node::get_create (decl);
      node->forced_by_abi = true;
    }
}

/* Get the offset to the BC's vtbl[] initializer from the start of CD.
   Returns "~0u" if the base class is not found in any vtable interfaces.  */

unsigned
base_vtable_offset (ClassDeclaration *cd, BaseClass *bc)
{
  unsigned csymoffset = target.classinfosize;
  unsigned interfacesize = int_size_in_bytes (vtbl_interface_type_node);
  csymoffset += cd->vtblInterfaces->length * interfacesize;

  for (size_t i = 0; i < cd->vtblInterfaces->length; i++)
    {
      BaseClass *b = (*cd->vtblInterfaces)[i];
      if (b == bc)
	return csymoffset;
      csymoffset += b->sym->vtbl.length * target.ptrsize;
    }

  /* Check all overriding interface vtbl[]s.  */
  for (ClassDeclaration *cd2 = cd->baseClass; cd2; cd2 = cd2->baseClass)
    {
      for (size_t k = 0; k < cd2->vtblInterfaces->length; k++)
	{
	  BaseClass *bs = (*cd2->vtblInterfaces)[k];
	  if (bs->fillVtbl (cd, NULL, 0))
	    {
	      if (bc == bs)
		return csymoffset;
	      csymoffset += bs->sym->vtbl.length * target.ptrsize;
	    }
	}
    }

  return ~0u;
}

/* Get the VAR_DECL of the vtable symbol for DECL.  If this does not yet exist,
   create it.  The vtable is accessible via ClassInfo, but since it is needed
   frequently (like for rtti comparisons), make it directly accessible.  */

tree
get_vtable_decl (ClassDeclaration *decl)
{
  if (decl->vtblsym)
    return decl->vtblsym;

  tree ident = mangle_internal_decl (decl, "__vtbl", "Z");
  /* Note: Using a static array type for the VAR_DECL, the DECL_INITIAL value
     will have a different type.  However the back-end seems to accept this.  */
  tree type = build_ctype (Type::tvoidptr->sarrayOf (decl->vtbl.length));

  decl->vtblsym = declare_extern_var (ident, type);
  DECL_LANG_SPECIFIC (decl->vtblsym) = build_lang_decl (NULL);

  /* Class is a reference, want the record type.  */
  DECL_CONTEXT (decl->vtblsym) = TREE_TYPE (build_ctype (decl->type));
  TREE_READONLY (decl->vtblsym) = 1;
  DECL_VIRTUAL_P (decl->vtblsym) = 1;

  SET_DECL_ALIGN (decl->vtblsym, TARGET_VTABLE_ENTRY_ALIGN);
  DECL_USER_ALIGN (decl->vtblsym) = true;

  return decl->vtblsym;
}

/* Helper function of build_class_instance.  Find the field inside aggregate
   TYPE identified by IDENT at field OFFSET.  */

static tree
find_aggregate_field (tree type, tree ident, tree offset)
{
  tree fields = TYPE_FIELDS (type);

  for (tree field = fields; field != NULL_TREE; field = TREE_CHAIN (field))
    {
      if (DECL_NAME (field) == NULL_TREE
	  && RECORD_OR_UNION_TYPE_P (TREE_TYPE (field))
	  && ANON_AGGR_TYPE_P (TREE_TYPE (field)))
	{
	  /* Search nesting anonymous structs and unions.  */
	  tree vfield = find_aggregate_field (TREE_TYPE (field),
					      ident, offset);
	  if (vfield != NULL_TREE)
	    return vfield;
	}
      else if (DECL_NAME (field) == ident
	       && (offset == NULL_TREE
		   || DECL_FIELD_OFFSET (field) == offset))
	{
	  /* Found matching field at offset.  */
	  return field;
	}
    }

  return NULL_TREE;
}

/* Helper function of build_new_class_expr.  Return a constructor that matches
   the layout of the class expression EXP.  */

static tree
build_class_instance (ClassReferenceExp *exp)
{
  ClassDeclaration *cd = exp->originalClass ();
  tree type = TREE_TYPE (build_ctype (exp->value->stype));
  vec <constructor_elt, va_gc> *ve = NULL;

  /* The set base vtable field.  */
  tree vptr = build_address (get_vtable_decl (cd));
  CONSTRUCTOR_APPEND_ELT (ve, TYPE_FIELDS (type), vptr);

  /* Go through the inheritance graph from top to bottom.  This will add all
     values to the constructor out of order, however build_struct_literal
     will re-order all values before returning the finished literal.  */
  for (ClassDeclaration *bcd = cd; bcd != NULL; bcd = bcd->baseClass)
    {
      /* Anonymous vtable interface fields are laid out before the fields of
	 each class.  The interface offset is used to determine where to put
	 the classinfo offset reference.  */
      for (size_t i = 0; i < bcd->vtblInterfaces->length; i++)
	{
	  BaseClass *bc = (*bcd->vtblInterfaces)[i];

	  for (ClassDeclaration *cd2 = cd; 1; cd2 = cd2->baseClass)
	    {
	      gcc_assert (cd2 != NULL);

	      unsigned csymoffset = base_vtable_offset (cd2, bc);
	      /* If the base class vtable was found.  */
	      if (csymoffset != ~0u)
		{
		  tree csym = build_address (get_classinfo_decl (cd2));
		  csym = build_offset (csym, size_int (csymoffset));

		  tree field = find_aggregate_field (type, NULL_TREE,
						     size_int (bc->offset));
		  gcc_assert (field != NULL_TREE);

		  CONSTRUCTOR_APPEND_ELT (ve, field, csym);
		  break;
		}
	    }
	}

      /* Generate initial values of all fields owned by current class.
	 Use both the name and offset to find the right field.  */
      for (size_t i = 0; i < bcd->fields.length; i++)
	{
	  VarDeclaration *vfield = bcd->fields[i];
	  int index = exp->findFieldIndexByName (vfield);
	  gcc_assert (index != -1);

	  Expression *value = (*exp->value->elements)[index];
	  if (!value)
	    continue;

	  /* Use find_aggregate_field to get the overridden field decl,
	     instead of the field associated with the base class.  */
	  tree field = get_symbol_decl (bcd->fields[i]);
	  field = find_aggregate_field (type, DECL_NAME (field),
					DECL_FIELD_OFFSET (field));
	  gcc_assert (field != NULL_TREE);

	  CONSTRUCTOR_APPEND_ELT (ve, field, build_expr (value, true));
	}
    }

  return build_struct_literal (type, ve);
}

/* Get the VAR_DECL of a class instance representing EXPR as static data.
   If this does not yet exist, create it.  This is used to support initializing
   a static variable that is of a class type using values known during CTFE.
   In user code, it is analogous to the following code snippet.

    enum E = new C(1, 2, 3);

   That we write the contents of `C(1, 2, 3)' to static data is only a compiler
   implementation detail.  The initialization of these symbols could be done at
   run-time using during as part of the module initialization or shared static
   constructors phase of run-time start-up - whichever comes after `gc_init()'.
   And infact that would be the better thing to do here eventually.  */

tree
build_new_class_expr (ClassReferenceExp *expr)
{
  if (expr->value->sym)
    return expr->value->sym;

  /* Build the reference symbol.  */
  tree type = build_ctype (expr->value->stype);
  expr->value->sym = build_artificial_decl (TREE_TYPE (type), NULL_TREE, "C");

  DECL_INITIAL (expr->value->sym) = build_class_instance (expr);
  d_pushdecl (expr->value->sym);
  rest_of_decl_compilation (expr->value->sym, 1, 0);

  return expr->value->sym;
}

/* Get the VAR_DECL of the static initializer symbol for the struct/class DECL.
   If this does not yet exist, create it.  The static initializer data is
   accessible via TypeInfo, and is also used in `new class' and default
   initializing struct literals.  */

tree
aggregate_initializer_decl (AggregateDeclaration *decl)
{
  if (decl->sinit)
    return decl->sinit;

  /* Class is a reference, want the record type.  */
  tree type = build_ctype (decl->type);
  StructDeclaration *sd = decl->isStructDeclaration ();
  if (!sd)
    type = TREE_TYPE (type);

  tree ident = mangle_internal_decl (decl, "__init", "Z");

  decl->sinit = declare_extern_var (ident, type);
  DECL_LANG_SPECIFIC (decl->sinit) = build_lang_decl (NULL);

  DECL_CONTEXT (decl->sinit) = type;
  TREE_READONLY (decl->sinit) = 1;

  /* Honor struct alignment set by user.  */
  if (sd && sd->alignment != STRUCTALIGN_DEFAULT)
    {
      SET_DECL_ALIGN (decl->sinit, sd->alignment * BITS_PER_UNIT);
      DECL_USER_ALIGN (decl->sinit) = true;
    }

  return decl->sinit;
}

/* Generate the data for the static initializer.  */

tree
layout_class_initializer (ClassDeclaration *cd)
{
  NewExp *ne = NewExp::create (cd->loc, NULL, NULL, cd->type, NULL);
  ne->type = cd->type;

  Expression *e = ne->ctfeInterpret ();
  gcc_assert (e->op == TOKclassreference);

  return build_class_instance (e->isClassReferenceExp ());
}

tree
layout_struct_initializer (StructDeclaration *sd)
{
  StructLiteralExp *sle = StructLiteralExp::create (sd->loc, sd, NULL);

  if (!sd->fill (sd->loc, sle->elements, true))
    gcc_unreachable ();

  sle->type = sd->type;
  return build_expr (sle, true);
}

/* Get the VAR_DECL of the static initializer symbol for the enum DECL.
   If this does not yet exist, create it.  The static initializer data is
   accessible via TypeInfo_Enum, but the field member type is a byte[] that
   requires a pointer to a symbol reference.  */

tree
enum_initializer_decl (EnumDeclaration *decl)
{
  if (decl->sinit)
    return decl->sinit;

  tree type = build_ctype (decl->type);

  Identifier *ident_save = decl->ident;
  if (!decl->ident)
    decl->ident = Identifier::generateId ("__enum");
  tree ident = mangle_internal_decl (decl, "__init", "Z");
  decl->ident = ident_save;

  decl->sinit = declare_extern_var (ident, type);
  DECL_LANG_SPECIFIC (decl->sinit) = build_lang_decl (NULL);

  DECL_CONTEXT (decl->sinit) = d_decl_context (decl);
  TREE_READONLY (decl->sinit) = 1;

  return decl->sinit;
}

/* Return an anonymous static variable of type TYPE, initialized with INIT,
   and optionally prefixing the name with PREFIX.  */

tree
build_artificial_decl (tree type, tree init, const char *prefix)
{
  tree decl = build_decl (UNKNOWN_LOCATION, VAR_DECL, NULL_TREE, type);
  const char *name = prefix ? prefix : "___s";
  char *label;

  ASM_FORMAT_PRIVATE_NAME (label, name, DECL_UID (decl));
  SET_DECL_ASSEMBLER_NAME (decl, get_identifier (label));
  DECL_NAME (decl) = DECL_ASSEMBLER_NAME (decl);

  TREE_PUBLIC (decl) = 0;
  TREE_STATIC (decl) = 1;
  TREE_USED (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;

  /* Perhaps at some point the initializer constant should be hashed
     to remove duplicates.  */
  DECL_INITIAL (decl) = init;

  return decl;
}

/* Build TYPE_DECL for the declaration DSYM.  */

void
build_type_decl (tree type, Dsymbol *dsym)
{
  if (TYPE_STUB_DECL (type))
    return;

  gcc_assert (!POINTER_TYPE_P (type));

  /* If a templated type, use the template instance name, as that includes all
     template parameters.  */
  const char *name = dsym->parent->isTemplateInstance ()
    ? ((TemplateInstance *) dsym->parent)->toChars () : dsym->ident->toChars ();

  tree decl = build_decl (make_location_t (dsym->loc), TYPE_DECL,
			  get_identifier (name), type);
  SET_DECL_ASSEMBLER_NAME (decl, get_identifier (d_mangle_decl (dsym)));
  TREE_PUBLIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_CONTEXT (decl) = d_decl_context (dsym);

  TYPE_CONTEXT (type) = DECL_CONTEXT (decl);
  TYPE_NAME (type) = decl;

  /* Not sure if there is a need for separate TYPE_DECLs in
     TYPE_NAME and TYPE_STUB_DECL.  */
  if (TREE_CODE (type) == ENUMERAL_TYPE || RECORD_OR_UNION_TYPE_P (type))
    TYPE_STUB_DECL (type) = decl;

  rest_of_decl_compilation (decl, SCOPE_FILE_SCOPE_P (decl), 0);
}

/* Create a declaration for field NAME of a given TYPE, setting the flags
   for whether the field is ARTIFICIAL and/or IGNORED.  */

tree
create_field_decl (tree type, const char *name, int artificial, int ignored)
{
  tree decl = build_decl (input_location, FIELD_DECL,
			  name ? get_identifier (name) : NULL_TREE, type);
  DECL_ARTIFICIAL (decl) = artificial;
  DECL_IGNORED_P (decl) = ignored;

  return decl;
}

/* Return the COMDAT group into which DECL should be placed.  */

static tree
d_comdat_group (tree decl)
{
  /* If already part of a comdat group, use that.  */
  if (DECL_COMDAT_GROUP (decl))
    return DECL_COMDAT_GROUP (decl);

  return DECL_ASSEMBLER_NAME (decl);
}

/* Set DECL up to have the closest approximation of "initialized common"
   linkage available.  */

void
d_comdat_linkage (tree decl)
{
  if (flag_weak)
    make_decl_one_only (decl, d_comdat_group (decl));
  else if (TREE_CODE (decl) == FUNCTION_DECL
	   || (VAR_P (decl) && DECL_ARTIFICIAL (decl)))
    /* We can just emit function and compiler-generated variables statically;
       having multiple copies is (for the most part) only a waste of space.  */
    TREE_PUBLIC (decl) = 0;
  else if (DECL_INITIAL (decl) == NULL_TREE
	   || DECL_INITIAL (decl) == error_mark_node)
    /* Fallback, cannot have multiple copies.  */
    DECL_COMMON (decl) = 1;

  if (TREE_PUBLIC (decl))
    DECL_COMDAT (decl) = 1;
}

/* Set DECL up to have the closest approximation of "linkonce" linkage.  */

void
d_linkonce_linkage (tree decl)
{
  /* Weak definitions have to be public.  */
  if (!TREE_PUBLIC (decl))
    return;

  /* Necessary to allow DECL_ONE_ONLY or DECL_WEAK functions to be inlined.  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    DECL_DECLARED_INLINE_P (decl) = 1;

  /* No weak support, fallback to COMDAT linkage.  */
  if (!flag_weak)
   return d_comdat_linkage (decl);

  make_decl_one_only (decl, d_comdat_group (decl));
}
