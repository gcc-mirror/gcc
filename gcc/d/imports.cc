/* imports.cc -- Build imported modules/declarations.
   Copyright (C) 2014-2024 Free Software Foundation, Inc.

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
#include "dmd/declaration.h"
#include "dmd/enum.h"
#include "dmd/identifier.h"
#include "dmd/import.h"
#include "dmd/module.h"

#include "tree.h"
#include "stringpool.h"

#include "d-tree.h"

static hash_map<Dsymbol *, tree> *imported_decls;

/* Implements the visitor interface to build debug trees for all
   module and import declarations, where RESULT_ holds the back-end
   representation to be cached and returned from the caller.  */
class ImportVisitor : public Visitor
{
  using Visitor::visit;

  tree result_;

  /* Build the declaration DECL as an imported symbol.  */
  tree make_import (tree decl)
  {
    gcc_assert (decl != NULL_TREE);

    tree import = build_decl (input_location, IMPORTED_DECL,
			      DECL_NAME (decl), void_type_node);
    IMPORTED_DECL_ASSOCIATED_DECL (import) = decl;
    d_keep (import);

    return import;
  }

public:
  ImportVisitor (void)
  {
    this->result_ = NULL_TREE;
  }

  tree result (void)
  {
    return this->result_;
  }

  /* This should be overridden by each symbol class.  */
  void visit (Dsymbol *) final override
  {
    gcc_unreachable ();
  }

  /* Build the module decl for M, this is considered toplevel, regardless
     of whether there are any parent packages in the module system.  */
  void visit (Module *m) final override
  {
    Loc loc = (m->md != NULL) ? m->md->loc
      : Loc (m->srcfile.toChars (), 1, 0);

    this->result_ = build_decl (make_location_t (loc), NAMESPACE_DECL,
				get_identifier (m->toPrettyChars ()),
				void_type_node);
    d_keep (this->result_);

    if (!m->isRoot ())
      DECL_EXTERNAL (this->result_) = 1;

    TREE_PUBLIC (this->result_) = 1;
    DECL_CONTEXT (this->result_) = NULL_TREE;
  }

  /* Build an import of another module symbol.  */

  void visit (Import *m) final override
  {
    tree module = build_import_decl (m->mod);
    this->result_ = this->make_import (module);
  }

  /* Build an import for any kind of user defined type.
     Use the TYPE_DECL associated with the type symbol.  */
  void visit (EnumDeclaration *d) final override
  {
    tree type = build_ctype (d->type);
    /* Not all kinds of D enums create a TYPE_DECL.  */
    if (TREE_CODE (type) == ENUMERAL_TYPE)
      {
	type = TYPE_MAIN_VARIANT (type);
	this->result_ = this->make_import (TYPE_STUB_DECL (type));
      }
  }

  void visit (AggregateDeclaration *d) final override
  {
    tree type = build_ctype (d->type);
    type = TYPE_MAIN_VARIANT (type);
    this->result_ = this->make_import (TYPE_STUB_DECL (type));
  }

  void visit (ClassDeclaration *d) final override
  {
    /* Want the RECORD_TYPE, not POINTER_TYPE.  */
    tree type = TREE_TYPE (build_ctype (d->type));
    type = TYPE_MAIN_VARIANT (type);
    this->result_ = this->make_import (TYPE_STUB_DECL (type));
  }

  void visit (VarDeclaration *d) final override
  {
    /* Not all kinds of manifest constants create a CONST_DECL.  */
    if (!d->canTakeAddressOf () && !d->type->isscalar ())
      return;

    visit ((Declaration *) d);
  }

  /* For now, ignore importing other kinds of dsymbols.  */
  void visit (ScopeDsymbol *) final override
  {
  }

  /* Alias symbols aren't imported, but their targets are.  */
  void visit (AliasDeclaration *d) final override
  {
    Dsymbol *dsym = d->toAlias ();

    if (dsym == d)
      {
	Type *type = d->getType ();

	/* Type imports should really be part of their own visit method.  */
	if (type != NULL)
	  {
	    if (type->ty == TY::Tenum)
	      dsym = type->isTypeEnum ()->sym;
	    else if (type->ty == TY::Tstruct)
	      dsym = type->isTypeStruct ()->sym;
	    else if (type->ty == TY::Tclass)
	      dsym = type->isTypeClass ()->sym;
	  }
      }

    /* This symbol is really an alias for another, visit the other.  */
    if (dsym != d)
      dsym->accept (this);
  }

  /* Visit the underlying alias symbol of overloadable aliases.  */
  void visit (OverDeclaration *d) final override
  {
    if (d->aliassym != NULL)
      d->aliassym->accept (this);
  }

  /* Build IMPORTED_DECLs for all overloads in a set.  */
  void visit (OverloadSet *d) final override
  {
    vec<tree, va_gc> *tset = NULL;

    vec_alloc (tset, d->a.length);

    for (size_t i = 0; i < d->a.length; i++)
      vec_safe_push (tset, build_import_decl (d->a[i]));

    this->result_ = build_tree_list_vec (tset);
    tset->truncate (0);
  }

  /* Function aliases are the same as alias symbols.  */
  void visit (FuncAliasDeclaration *d) final override
  {
    FuncDeclaration *fd = d->toAliasFunc ();

    if (fd != NULL)
      fd->accept (this);
  }

  /* Skip over importing templates and tuples.  */
  void visit (TemplateDeclaration *) final override
  {
  }

  void visit (TupleDeclaration *) final override
  {
  }

  /* Import any other kind of declaration.  If the class does not implement
     symbol generation routines, the compiler will throw an error.  */
  void visit (Declaration *d) final override
  {
    this->result_ = this->make_import (get_symbol_decl (d));
  }
};


/* Build a declaration for the symbol D that can be used for the
   debug_hook imported_module_or_decl.  */
tree
build_import_decl (Dsymbol *d)
{
  hash_map_maybe_create<hm_ggc> (imported_decls);

  if (tree *decl = imported_decls->get (d))
    return *decl;

  location_t saved_location = input_location;
  ImportVisitor v = ImportVisitor ();

  input_location = make_location_t (d->loc);
  d->accept (&v);
  input_location = saved_location;

  /* Not all visitors set `result'.  */
  tree isym = v.result ();
  if (isym != NULL_TREE)
    imported_decls->put (d, isym);

  return isym;
}
