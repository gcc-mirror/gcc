/* Process declarations and variables for C compiler.
   Copyright (C) 1988, 92, 93, 94, 95, 1996 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* Process declarations and symbol lookup for C front end.
   Also constructs types; the standard scalar types at initialization,
   and structure, union, array and enum types when they are declared.  */

/* ??? not all decl nodes are given the most useful possible
   line numbers.  For example, the CONST_DECLs for enum values.  */

#include "config.h"
#include <stdio.h>
#include "tree.h"
#include "rtl.h"
#include "flags.h"
#include "cp-tree.h"
#include "decl.h"
#include "lex.h"
#include "output.h"
#include "except.h"
#include "expr.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

static tree get_sentry PROTO((tree));
static void mark_vtable_entries PROTO((tree));
static void import_export_template PROTO((tree));
static void grok_function_init PROTO((tree, tree));
static int finish_vtable_vardecl PROTO((tree, tree));
static int prune_vtable_vardecl PROTO((tree, tree));
static void finish_sigtable_vardecl PROTO((tree, tree));

extern int current_class_depth;

/* A list of virtual function tables we must make sure to write out.  */
tree pending_vtables;

/* A list of static class variables.  This is needed, because a
   static class variable can be declared inside the class without
   an initializer, and then initialized, staticly, outside the class.  */
tree pending_statics;

/* A list of functions which were declared inline, but which we
   may need to emit outline anyway.  */
static tree saved_inlines;

/* Used to help generate temporary names which are unique within
   a function.  Reset to 0 by start_function.  */

int temp_name_counter;

/* Same, but not reset.  Local temp variables and global temp variables
   can have the same name.  */
static int global_temp_name_counter;

/* Flag used when debugging spew.c */

extern int spew_debug;

/* Nonzero if we're done parsing and into end-of-file activities.  */

int at_eof;

/* Functions called along with real static constructors and destructors.  */

tree static_ctors, static_dtors;

/* C (and C++) language-specific option variables.  */

/* Nonzero means allow type mismatches in conditional expressions;
   just make their values `void'.   */

int flag_cond_mismatch;

/* Nonzero means give `double' the same size as `float'.  */

int flag_short_double;

/* Nonzero means don't recognize the keyword `asm'.  */

int flag_no_asm;

/* Nonzero means don't recognize any extension keywords.  */

int flag_no_gnu_keywords;

/* Nonzero means don't recognize the non-ANSI builtin functions.  */

int flag_no_builtin;

/* Nonzero means don't recognize the non-ANSI builtin functions.
   -ansi sets this.  */

int flag_no_nonansi_builtin;

/* Nonzero means do some things the same way PCC does.  Only provided so
   the compiler will link.  */

int flag_traditional;

/* Nonzero means to treat bitfields as unsigned unless they say `signed'.  */

int flag_signed_bitfields = 1;

/* Nonzero means handle `#ident' directives.  0 means ignore them.  */

int flag_no_ident;

/* Nonzero means enable obscure ANSI features and disable GNU extensions
   that might cause ANSI-compliant code to be miscompiled.  */

int flag_ansi;

/* Nonzero means do argument matching for overloading according to the
   ANSI rules, rather than what g++ used to believe to be correct.  */

int flag_ansi_overloading = 1;

/* Nonzero means do emit exported implementations of functions even if
   they can be inlined.  */

int flag_implement_inlines = 1;

/* Nonzero means do emit exported implementations of templates, instead of
   multiple static copies in each file that needs a definition.  */

int flag_external_templates;

/* Nonzero means that the decision to emit or not emit the implementation of a
   template depends on where the template is instantiated, rather than where
   it is defined.  */

int flag_alt_external_templates;

/* Nonzero means that implicit instantiations will be emitted if needed.  */

int flag_implicit_templates = 1;

/* Nonzero means warn about implicit declarations.  */

int warn_implicit = 1;

/* Nonzero means warn when all ctors or dtors are private, and the class
   has no friends.  */

int warn_ctor_dtor_privacy = 1;

/* True if we want to implement vtables using "thunks".
   The default is off.  */

#ifndef DEFAULT_VTABLE_THUNKS
#define DEFAULT_VTABLE_THUNKS 0
#endif
int flag_vtable_thunks = DEFAULT_VTABLE_THUNKS;

/* True if we want to deal with repository information.  */

int flag_use_repository;

/* Nonzero means give string constants the type `const char *'
   to get extra warnings from them.  These warnings will be too numerous
   to be useful, except in thoroughly ANSIfied programs.  */

int warn_write_strings;

/* Nonzero means warn about pointer casts that can drop a type qualifier
   from the pointer target type.  */

int warn_cast_qual;

/* Nonzero means warn that dbx info for template class methods isn't fully
   supported yet.  */

int warn_template_debugging;

/* Nonzero means warn about sizeof(function) or addition/subtraction
   of function pointers.  */

int warn_pointer_arith = 1;

/* Nonzero means warn for any function def without prototype decl.  */

int warn_missing_prototypes;

/* Nonzero means warn about multiple (redundant) decls for the same single
   variable or function.  */

int warn_redundant_decls;

/* Warn if initializer is not completely bracketed.  */

int warn_missing_braces;

/* Warn about comparison of signed and unsigned values.  */

int warn_sign_compare;

/* Warn about *printf or *scanf format/argument anomalies.  */

int warn_format;

/* Warn about a subscript that has type char.  */

int warn_char_subscripts;

/* Warn if a type conversion is done that might have confusing results.  */

int warn_conversion;

/* Warn if adding () is suggested.  */

int warn_parentheses;

/* Non-zero means warn in function declared in derived class has the
   same name as a virtual in the base class, but fails to match the
   type signature of any virtual function in the base class.  */
int warn_overloaded_virtual;

/* Non-zero means warn when declaring a class that has a non virtual
   destructor, when it really ought to have a virtual one.  */
int warn_nonvdtor;

/* Non-zero means warn when a function is declared extern and later inline.  */
int warn_extern_inline;

/* Non-zero means warn when the compiler will reorder code.  */
int warn_reorder;

/* Non-zero means warn when synthesis behavior differs from Cfront's.  */
int warn_synth;

/* Non-zero means warn when we convert a pointer to member function
   into a pointer to (void or function).  */
int warn_pmf2ptr = 1;

/* Nonzero means warn about violation of some Effective C++ style rules.  */

int warn_ecpp;

/* Nonzero means warn where overload resolution chooses a promotion from
   unsigned to signed over a conversion to an unsigned of the same size.  */

int warn_sign_promo;

/* Nonzero means warn when an old-style cast is used.  */

int warn_old_style_cast;

/* Nonzero means `$' can be in an identifier.  */

#ifndef DOLLARS_IN_IDENTIFIERS
#define DOLLARS_IN_IDENTIFIERS 1
#endif
int dollars_in_ident = DOLLARS_IN_IDENTIFIERS;

/* Nonzero for -fno-strict-prototype switch: do not consider empty
   argument prototype to mean function takes no arguments.  */

int flag_strict_prototype = 2;
int strict_prototype = 1;
int strict_prototypes_lang_c, strict_prototypes_lang_cplusplus = 1;

/* Nonzero means that labels can be used as first-class objects */

int flag_labels_ok;

/* Non-zero means to collect statistics which might be expensive
   and to print them when we are done.  */
int flag_detailed_statistics;

/* C++ specific flags.  */   
/* Nonzero for -fall-virtual: make every member function (except
   constructors) lay down in the virtual function table.  Calls
   can then either go through the virtual function table or not,
   depending.  */

int flag_all_virtual;

/* Zero means that `this' is a *const.  This gives nice behavior in the
   2.0 world.  1 gives 1.2-compatible behavior.  2 gives Spring behavior.
   -2 means we're constructing an object and it has fixed type.  */

int flag_this_is_variable;

/* Nonzero means memoize our member lookups.  */

int flag_memoize_lookups; int flag_save_memoized_contexts;

/* 3 means write out only virtuals function tables `defined'
   in this implementation file.
   2 means write out only specific virtual function tables
   and give them (C) public access.
   1 means write out virtual function tables and give them
   (C) public access.
   0 means write out virtual function tables and give them
   (C) static access (default).
   -1 means declare virtual function tables extern.  */

int write_virtuals;

/* Nonzero means we should attempt to elide constructors when possible.
   FIXME: This flag is obsolete, and should be torn out along with the
   old overloading code.  */

int flag_elide_constructors;

/* Nonzero means recognize and handle signature language constructs.  */

int flag_handle_signatures;

/* Nonzero means that member functions defined in class scope are
   inline by default.  */

int flag_default_inline = 1;

/* Controls whether enums and ints freely convert.
   1 means with complete freedom.
   0 means enums can convert to ints, but not vice-versa.  */
int flag_int_enum_equivalence;

/* Controls whether compiler generates 'type descriptor' that give
   run-time type information.  */
int flag_rtti = 1;

/* Nonzero if we wish to output cross-referencing information
   for the GNU class browser.  */
extern int flag_gnu_xref;

/* Nonzero if compiler can make `reasonable' assumptions about
   references and objects.  For example, the compiler must be
   conservative about the following and not assume that `a' is nonnull:

   obj &a = g ();
   a.f (2);

   In general, it is `reasonable' to assume that for many programs,
   and better code can be generated in that case.  */

int flag_assume_nonnull_objects = 1;

/* Nonzero if we want to support huge (> 2^(sizeof(short)*8-1) bytes)
   objects.  */

int flag_huge_objects;

/* Nonzero if we want to conserve space in the .o files.  We do this
   by putting uninitialized data and runtime initialized data into
   .common instead of .data at the expense of not flagging multiple
   definitions.  */

int flag_conserve_space;

/* Nonzero if we want to obey access control semantics.  */

int flag_access_control = 1;

/* Nonzero if we want to understand the operator names, i.e. 'bitand'.  */

int flag_operator_names;

/* Nonzero if we want to check the return value of new and avoid calling
   constructors if it is a null pointer.  */

int flag_check_new;

/* Nonzero if we want the new ANSI rules for pushing a new scope for `for'
   initialization variables.
   0: Old rules, set by -fno-for-scope.
   2: New ANSI rules, set by -ffor-scope.
   1: Try to implement new ANSI rules, but with backup compatibility
   (and warnings).  This is the default, for now.  */

int flag_new_for_scope = 1;

/* Nonzero if we want to emit defined symbols with common-like linkage as
   weak symbols where possible, in order to conform to C++ semantics.
   Otherwise, emit them as local symbols.  */

int flag_weak = 1;

/* Maximum template instantiation depth. Must be at least 17 for ANSI
   compliance. */

int max_tinst_depth = 17;

/* The name-mangling scheme to use.  Must be 1 or greater to support
   template functions with identical types, but different template
   arguments.  */
int name_mangling_version = 1;

/* Nonzero means that guiding declarations are allowed.  */
int flag_guiding_decls;

/* Table of language-dependent -f options.
   STRING is the option name.  VARIABLE is the address of the variable.
   ON_VALUE is the value to store in VARIABLE
    if `-fSTRING' is seen as an option.
   (If `-fno-STRING' is seen as an option, the opposite value is stored.)  */

static struct { char *string; int *variable; int on_value;} lang_f_options[] =
{
  {"signed-char", &flag_signed_char, 1},
  {"unsigned-char", &flag_signed_char, 0},
  {"signed-bitfields", &flag_signed_bitfields, 1},
  {"unsigned-bitfields", &flag_signed_bitfields, 0},
  {"short-enums", &flag_short_enums, 1},
  {"short-double", &flag_short_double, 1},
  {"cond-mismatch", &flag_cond_mismatch, 1},
  {"asm", &flag_no_asm, 0},
  {"builtin", &flag_no_builtin, 0},
  {"ident", &flag_no_ident, 0},
  {"labels-ok", &flag_labels_ok, 1},
  {"stats", &flag_detailed_statistics, 1},
  {"this-is-variable", &flag_this_is_variable, 1},
  {"strict-prototype", &flag_strict_prototype, 1},
  {"all-virtual", &flag_all_virtual, 1},
  {"memoize-lookups", &flag_memoize_lookups, 1},
  {"elide-constructors", &flag_elide_constructors, 1},
  {"handle-exceptions", &flag_exceptions, 1},
  {"handle-signatures", &flag_handle_signatures, 1},
  {"default-inline", &flag_default_inline, 1},
  {"dollars-in-identifiers", &dollars_in_ident, 1},
  {"enum-int-equiv", &flag_int_enum_equivalence, 1},
  {"rtti", &flag_rtti, 1},
  {"xref", &flag_gnu_xref, 1},
  {"nonnull-objects", &flag_assume_nonnull_objects, 1},
  {"implement-inlines", &flag_implement_inlines, 1},
  {"external-templates", &flag_external_templates, 1},
  {"implicit-templates", &flag_implicit_templates, 1},
  {"ansi-overloading", &flag_ansi_overloading, 1},
  {"huge-objects", &flag_huge_objects, 1},
  {"conserve-space", &flag_conserve_space, 1},
  {"vtable-thunks", &flag_vtable_thunks, 1},
  {"access-control", &flag_access_control, 1},
  {"nonansi-builtins", &flag_no_nonansi_builtin, 0},
  {"gnu-keywords", &flag_no_gnu_keywords, 0},
  {"operator-names", &flag_operator_names, 1},
  {"check-new", &flag_check_new, 1},
  {"repo", &flag_use_repository, 1},
  {"for-scope", &flag_new_for_scope, 2},
  {"weak", &flag_weak, 1}
};

/* Decode the string P as a language-specific option.
   Return 1 if it is recognized (and handle it);
   return 0 if not recognized.  */

int   
lang_decode_option (p)
     char *p;
{
  if (!strcmp (p, "-ftraditional") || !strcmp (p, "-traditional"))
    flag_writable_strings = 1,
    flag_this_is_variable = 1, flag_new_for_scope = 0;
  /* The +e options are for cfront compatibility.  They come in as
     `-+eN', to kludge around gcc.c's argument handling.  */
  else if (p[0] == '-' && p[1] == '+' && p[2] == 'e')
    {
      int old_write_virtuals = write_virtuals;
      if (p[3] == '1')
	write_virtuals = 1;
      else if (p[3] == '0')
	write_virtuals = -1;
      else if (p[3] == '2')
	write_virtuals = 2;
      else error ("invalid +e option");
      if (old_write_virtuals != 0
	  && write_virtuals != old_write_virtuals)
	error ("conflicting +e options given");
    }
  else if (p[0] == '-' && p[1] == 'f')
    {
      /* Some kind of -f option.
	 P's value is the option sans `-f'.
	 Search for it in the table of options.  */
      int found = 0, j;

      p += 2;
      /* Try special -f options.  */

      if (!strcmp (p, "handle-exceptions")
	  || !strcmp (p, "no-handle-exceptions"))
	warning ("-fhandle-exceptions has been renamed to -fexceptions (and is now on by default)");

      if (!strcmp (p, "save-memoized"))
	{
	  flag_memoize_lookups = 1;
	  flag_save_memoized_contexts = 1;
	  found = 1;
	}
      else if (!strcmp (p, "no-save-memoized"))
	{
	  flag_memoize_lookups = 0;
	  flag_save_memoized_contexts = 0;
	  found = 1;
	}
      else if (! strcmp (p, "alt-external-templates"))
	{
	  flag_external_templates = 1;
	  flag_alt_external_templates = 1;
	  found = 1;
	}
      else if (! strcmp (p, "no-alt-external-templates"))
	{
	  flag_alt_external_templates = 0;
	  found = 1;
	}
      else if (!strcmp (p, "repo"))
	{
	  flag_use_repository = 1;
	  flag_implicit_templates = 0;
	  found = 1;
	}
      else if (!strcmp (p, "guiding-decls"))
	{
	  flag_guiding_decls = 1;
	  name_mangling_version = 0;
	  found = 1;
	}
      else if (!strcmp (p, "no-guiding-decls"))
	{
	  flag_guiding_decls = 0;
	  found = 1;
	}
      else if (!strncmp (p, "template-depth-", 15))
	{
	  char *endp = p + 15;
	  while (*endp)
	    {
	      if (*endp >= '0' && *endp <= '9')
		endp++;
	      else
		{
		  error ("Invalid option `%s'", p - 2);
		  goto template_depth_lose;
		}
	    }
	  max_tinst_depth = atoi (p + 15);
	template_depth_lose: ;
	}
      else if (!strncmp (p, "name-mangling-version-", 22))
	{
	  char *endp = p + 22;
	  while (*endp)
	    {
	      if (*endp >= '0' && *endp <= '9')
		endp++;
	      else
		{
		  error ("Invalid option `%s'", p - 2);
		  goto mangling_version_lose;
		}
	    }
	  name_mangling_version = atoi (p + 22);
	mangling_version_lose: ;
	}
      else for (j = 0;
		!found && j < sizeof (lang_f_options) / sizeof (lang_f_options[0]);
		j++)
	{
	  if (!strcmp (p, lang_f_options[j].string))
	    {
	      *lang_f_options[j].variable = lang_f_options[j].on_value;
	      /* A goto here would be cleaner,
		 but breaks the vax pcc.  */
	      found = 1;
	    }
	  if (p[0] == 'n' && p[1] == 'o' && p[2] == '-'
	      && ! strcmp (p+3, lang_f_options[j].string))
	    {
	      *lang_f_options[j].variable = ! lang_f_options[j].on_value;
	      found = 1;
	    }
	}
      return found;
    }
  else if (p[0] == '-' && p[1] == 'W')
    {
      int setting = 1;

      /* The -W options control the warning behavior of the compiler.  */
      p += 2;

      if (p[0] == 'n' && p[1] == 'o' && p[2] == '-')
	setting = 0, p += 3;

      if (!strcmp (p, "implicit"))
	warn_implicit = setting;
      else if (!strcmp (p, "return-type"))
	warn_return_type = setting;
      else if (!strcmp (p, "ctor-dtor-privacy"))
	warn_ctor_dtor_privacy = setting;
      else if (!strcmp (p, "write-strings"))
	warn_write_strings = setting;
      else if (!strcmp (p, "cast-qual"))
	warn_cast_qual = setting;
      else if (!strcmp (p, "char-subscripts"))
	warn_char_subscripts = setting;
      else if (!strcmp (p, "pointer-arith"))
	warn_pointer_arith = setting;
      else if (!strcmp (p, "missing-prototypes"))
	warn_missing_prototypes = setting;
      else if (!strcmp (p, "redundant-decls"))
	warn_redundant_decls = setting;
      else if (!strcmp (p, "missing-braces"))
	warn_missing_braces = setting;
      else if (!strcmp (p, "sign-compare"))
	warn_sign_compare = setting;
      else if (!strcmp (p, "format"))
	warn_format = setting;
      else if (!strcmp (p, "conversion"))
	warn_conversion = setting;
      else if (!strcmp (p, "parentheses"))
	warn_parentheses = setting;
      else if (!strcmp (p, "non-virtual-dtor"))
	warn_nonvdtor = setting;
      else if (!strcmp (p, "extern-inline"))
	warn_extern_inline = setting;
      else if (!strcmp (p, "reorder"))
	warn_reorder = setting;
      else if (!strcmp (p, "synth"))
	warn_synth = setting;
      else if (!strcmp (p, "pmf-conversions"))
	warn_pmf2ptr = setting;
      else if (!strcmp (p, "effc++"))
	warn_ecpp = setting;
      else if (!strcmp (p, "sign-promo"))
	warn_sign_promo = setting;
      else if (!strcmp (p, "old-style-cast"))
	warn_old_style_cast = setting;
      else if (!strcmp (p, "comment"))
	;			/* cpp handles this one.  */
      else if (!strcmp (p, "comments"))
	;			/* cpp handles this one.  */
      else if (!strcmp (p, "trigraphs"))
	;			/* cpp handles this one.  */
      else if (!strcmp (p, "import"))
	;			/* cpp handles this one.  */
      else if (!strcmp (p, "all"))
	{
	  warn_return_type = setting;
	  warn_unused = setting;
	  warn_implicit = setting;
	  warn_ctor_dtor_privacy = setting;
	  warn_switch = setting;
	  warn_format = setting;
	  warn_parentheses = setting;
	  warn_missing_braces = setting;
	  warn_sign_compare = setting;
	  warn_extern_inline = setting;
	  warn_nonvdtor = setting;
	  /* We save the value of warn_uninitialized, since if they put
	     -Wuninitialized on the command line, we need to generate a
	     warning about not using it without also specifying -O.  */
	  if (warn_uninitialized != 1)
	    warn_uninitialized = (setting ? 2 : 0);
	  warn_template_debugging = setting;
	  warn_reorder = setting;
	  warn_sign_promo = setting;
	}

      else if (!strcmp (p, "overloaded-virtual"))
	warn_overloaded_virtual = setting;
      else return 0;
    }
  else if (!strcmp (p, "-ansi"))
    flag_no_nonansi_builtin = 1, flag_ansi = 1,
    flag_no_gnu_keywords = 1, flag_operator_names = 1;
#ifdef SPEW_DEBUG
  /* Undocumented, only ever used when you're invoking cc1plus by hand, since
     it's probably safe to assume no sane person would ever want to use this
     under normal circumstances.  */
  else if (!strcmp (p, "-spew-debug"))
    spew_debug = 1;
#endif
  else
    return 0;

  return 1;
}

/* Incorporate `const' and `volatile' qualifiers for member functions.
   FUNCTION is a TYPE_DECL or a FUNCTION_DECL.
   QUALS is a list of qualifiers.  */

tree
grok_method_quals (ctype, function, quals)
     tree ctype, function, quals;
{
  tree fntype = TREE_TYPE (function);
  tree raises = TYPE_RAISES_EXCEPTIONS (fntype);

  do
    {
      extern tree ridpointers[];

      if (TREE_VALUE (quals) == ridpointers[(int)RID_CONST])
	{
	  if (TYPE_READONLY (ctype))
	    error ("duplicate `%s' %s",
		   IDENTIFIER_POINTER (TREE_VALUE (quals)),
		   (TREE_CODE (function) == FUNCTION_DECL
		    ? "for member function" : "in type declaration"));
	  ctype = build_type_variant (ctype, 1, TYPE_VOLATILE (ctype));
	  build_pointer_type (ctype);
	}
      else if (TREE_VALUE (quals) == ridpointers[(int)RID_VOLATILE])
	{
	  if (TYPE_VOLATILE (ctype))
	    error ("duplicate `%s' %s",
		   IDENTIFIER_POINTER (TREE_VALUE (quals)),
		   (TREE_CODE (function) == FUNCTION_DECL
		    ? "for member function" : "in type declaration"));
	  ctype = build_type_variant (ctype, TYPE_READONLY (ctype), 1);
	  build_pointer_type (ctype);
	}
      else
	my_friendly_abort (20);
      quals = TREE_CHAIN (quals);
    }
  while (quals);
  fntype = build_cplus_method_type (ctype, TREE_TYPE (fntype),
				    (TREE_CODE (fntype) == METHOD_TYPE
				     ? TREE_CHAIN (TYPE_ARG_TYPES (fntype))
				     : TYPE_ARG_TYPES (fntype)));
  if (raises)
    fntype = build_exception_variant (fntype, raises);

  TREE_TYPE (function) = fntype;
  return ctype;
}

#if 0				/* Not used.  */
/* This routine replaces cryptic DECL_NAMEs with readable DECL_NAMEs.
   It leaves DECL_ASSEMBLER_NAMEs with the correct value.  */
/* This does not yet work with user defined conversion operators
   It should.  */

static void
substitute_nice_name (decl)
     tree decl;
{
  if (DECL_NAME (decl) && TREE_CODE (DECL_NAME (decl)) == IDENTIFIER_NODE)
    {
      char *n = decl_as_string (DECL_NAME (decl), 1);
      if (n[strlen (n) - 1] == ' ')
	n[strlen (n) - 1] = 0;
      DECL_NAME (decl) = get_identifier (n);
    }
}
#endif

/* Warn when -fexternal-templates is used and #pragma
   interface/implementation is not used all the times it should be,
   inform the user.  */

void
warn_if_unknown_interface (decl)
     tree decl;
{
  static int already_warned = 0;
  if (already_warned++)
    return;

  if (flag_alt_external_templates)
    {
      struct tinst_level *til = tinst_for_decl ();
      int sl = lineno;
      char *sf = input_filename;

      if (til)
	{
	  lineno = til->line;
	  input_filename = til->file;
	}
      cp_warning ("template `%#D' instantiated in file without #pragma interface",
		  decl);
      lineno = sl;
      input_filename = sf;
    }
  else
    cp_warning_at ("template `%#D' defined in file without #pragma interface",
		   decl);
}

/* A subroutine of the parser, to handle a component list.  */

tree
grok_x_components (specs, components)
     tree specs, components;
{
  register tree t, x, tcode;

  /* We just got some friends.  They have been recorded elsewhere.  */
  if (components == void_type_node)
    return NULL_TREE;

  if (components == NULL_TREE)
    {
      t = groktypename (build_decl_list (specs, NULL_TREE));

      if (t == NULL_TREE)
	{
	  error ("error in component specification");
	  return NULL_TREE;
	}

      switch (TREE_CODE (t))
	{
	case VAR_DECL:
	  /* Static anonymous unions come out as VAR_DECLs.  */
	  if (TREE_CODE (TREE_TYPE (t)) == UNION_TYPE
	      && ANON_AGGRNAME_P (TYPE_IDENTIFIER (TREE_TYPE (t))))
	    return t;

	  /* We return SPECS here, because in the parser it was ending
	     up with not doing anything to $$, which is what SPECS
	     represents.  */
	  return specs;
	  break;

	case RECORD_TYPE:
	  /* This code may be needed for UNION_TYPEs as
	     well.  */
	  tcode = record_type_node;
	  if (CLASSTYPE_DECLARED_CLASS (t))
	    tcode = class_type_node;
	  else if (IS_SIGNATURE (t))
	    tcode = signature_type_node;
	  
	  t = xref_tag (tcode, TYPE_IDENTIFIER (t), NULL_TREE, 0);
	  if (TYPE_CONTEXT (t))
	    CLASSTYPE_NO_GLOBALIZE (t) = 1;
	  return NULL_TREE;
	  break;

	case UNION_TYPE:
	case ENUMERAL_TYPE:
	  if (TREE_CODE (t) == UNION_TYPE)
	    tcode = union_type_node;
	  else
	    tcode = enum_type_node;

	  t = xref_tag (tcode, TYPE_IDENTIFIER (t), NULL_TREE, 0);
	  if (TREE_CODE (t) == UNION_TYPE && TYPE_CONTEXT (t))
	    CLASSTYPE_NO_GLOBALIZE (t) = 1;
	  if (TREE_CODE (t) == UNION_TYPE
	      && ANON_AGGRNAME_P (TYPE_IDENTIFIER (t)))
	    {
	      /* See also shadow_tag.  */

	      struct pending_inline **p;
	      tree *q;
	      x = build_lang_field_decl (FIELD_DECL, NULL_TREE, t);

	      /* Wipe out memory of synthesized methods */
	      TYPE_HAS_CONSTRUCTOR (t) = 0;
	      TYPE_HAS_DEFAULT_CONSTRUCTOR (t) = 0;
	      TYPE_HAS_INIT_REF (t) = 0;
	      TYPE_HAS_CONST_INIT_REF (t) = 0;
	      TYPE_HAS_ASSIGN_REF (t) = 0;
	      TYPE_HAS_ASSIGNMENT (t) = 0;
	      TYPE_HAS_CONST_ASSIGN_REF (t) = 0;

	      q = &TYPE_METHODS (t);
	      while (*q)
		{
		  if (DECL_ARTIFICIAL (*q))
		    *q = TREE_CHAIN (*q);
		  else
		    q = &TREE_CHAIN (*q);
		}
	      if (TYPE_METHODS (t))
		error ("an anonymous union cannot have function members");

	      p = &pending_inlines;
	      for (; *p; *p = (*p)->next)
		if (DECL_CONTEXT ((*p)->fndecl) != t)
		  break;
	    }
	  else if (TREE_CODE (t) == ENUMERAL_TYPE)
	    x = grok_enum_decls (t, NULL_TREE);
	  else
	    x = NULL_TREE;
	  return x;
	  break;

	default:
	  if (t != void_type_node)
	    error ("empty component declaration");
	  return NULL_TREE;
	}
    }
  else
    {
      t = TREE_TYPE (components);
      if (TREE_CODE (t) == ENUMERAL_TYPE && TREE_NONLOCAL_FLAG (t))
	return grok_enum_decls (t, components);
      else
	return components;
    }
}

/* Classes overload their constituent function names automatically.
   When a function name is declared in a record structure,
   its name is changed to it overloaded name.  Since names for
   constructors and destructors can conflict, we place a leading
   '$' for destructors.

   CNAME is the name of the class we are grokking for.

   FUNCTION is a FUNCTION_DECL.  It was created by `grokdeclarator'.

   FLAGS contains bits saying what's special about today's
   arguments.  1 == DESTRUCTOR.  2 == OPERATOR.

   If FUNCTION is a destructor, then we must add the `auto-delete' field
   as a second parameter.  There is some hair associated with the fact
   that we must "declare" this variable in the manner consistent with the
   way the rest of the arguments were declared.

   QUALS are the qualifiers for the this pointer.  */

void
grokclassfn (ctype, cname, function, flags, quals)
     tree ctype, cname, function;
     enum overload_flags flags;
     tree quals;
{
  tree fn_name = DECL_NAME (function);
  tree arg_types;
  tree parm;
  tree qualtype;
  tree fntype = TREE_TYPE (function);
  tree raises = TYPE_RAISES_EXCEPTIONS (fntype);

  if (fn_name == NULL_TREE)
    {
      error ("name missing for member function");
      fn_name = get_identifier ("<anonymous>");
      DECL_NAME (function) = fn_name;
    }

  if (quals)
    qualtype = grok_method_quals (ctype, function, quals);
  else
    qualtype = ctype;

  arg_types = TYPE_ARG_TYPES (TREE_TYPE (function));
  if (TREE_CODE (TREE_TYPE (function)) == METHOD_TYPE)
    {
      /* Must add the class instance variable up front.  */
      /* Right now we just make this a pointer.  But later
	 we may wish to make it special.  */
      tree type = TREE_VALUE (arg_types);
      int constp = 1;

      if ((flag_this_is_variable > 0)
	  && (flags == DTOR_FLAG || DECL_CONSTRUCTOR_P (function)))
	constp = 0;

      if (DECL_CONSTRUCTOR_P (function))
	{
	  if (TYPE_USES_VIRTUAL_BASECLASSES (ctype))
	    {
	      DECL_CONSTRUCTOR_FOR_VBASE_P (function) = 1;
	      /* In this case we need "in-charge" flag saying whether
		 this constructor is responsible for initialization
		 of virtual baseclasses or not.  */
	      parm = build_decl (PARM_DECL, in_charge_identifier, integer_type_node);
	      /* Mark the artificial `__in_chrg' parameter as "artificial".  */
	      SET_DECL_ARTIFICIAL (parm);
	      DECL_ARG_TYPE (parm) = integer_type_node;
	      TREE_READONLY (parm) = 1;
	      TREE_CHAIN (parm) = last_function_parms;
	      last_function_parms = parm;
	    }
	}

      parm = build_decl (PARM_DECL, this_identifier, type);
      /* Mark the artificial `this' parameter as "artificial".  */
      SET_DECL_ARTIFICIAL (parm);
      DECL_ARG_TYPE (parm) = type;
      /* We can make this a register, so long as we don't
	 accidentally complain if someone tries to take its address.  */
      DECL_REGISTER (parm) = 1;
      if (constp)
	TREE_READONLY (parm) = 1;
      TREE_CHAIN (parm) = last_function_parms;
      last_function_parms = parm;
    }

  if (flags == DTOR_FLAG)
    {
      char *buf, *dbuf;
      int len = sizeof (DESTRUCTOR_DECL_PREFIX)-1;

      arg_types = hash_tree_chain (integer_type_node, void_list_node);
      TREE_SIDE_EFFECTS (arg_types) = 1;
      /* Build the overload name.  It will look like `7Example'.  */
      if (IDENTIFIER_TYPE_VALUE (cname))
	dbuf = build_overload_name (IDENTIFIER_TYPE_VALUE (cname), 1, 1);
      else if (IDENTIFIER_LOCAL_VALUE (cname))
	dbuf = build_overload_name (TREE_TYPE (IDENTIFIER_LOCAL_VALUE (cname)), 1, 1);
      else
      /* Using ctype fixes the `X::Y::~Y()' crash.  The cname has no type when
	 it's defined out of the class definition, since poplevel_class wipes
	 it out.  This used to be internal error 346.  */
	dbuf = build_overload_name (ctype, 1, 1);
      buf = (char *) alloca (strlen (dbuf) + sizeof (DESTRUCTOR_DECL_PREFIX));
      bcopy (DESTRUCTOR_DECL_PREFIX, buf, len);
      buf[len] = '\0';
      strcat (buf, dbuf);
      DECL_ASSEMBLER_NAME (function) = get_identifier (buf);
      parm = build_decl (PARM_DECL, in_charge_identifier, integer_type_node);
      /* Mark the artificial `__in_chrg' parameter as "artificial".  */
      SET_DECL_ARTIFICIAL (parm);
      TREE_READONLY (parm) = 1;
      DECL_ARG_TYPE (parm) = integer_type_node;
      /* This is the same chain as DECL_ARGUMENTS (...).  */
      TREE_CHAIN (last_function_parms) = parm;

      fntype = build_cplus_method_type (qualtype, void_type_node,
					arg_types);
      if (raises)
	{
	  fntype = build_exception_variant (fntype, raises);
	}
      TREE_TYPE (function) = fntype;
      TYPE_HAS_DESTRUCTOR (ctype) = 1;
    }
  else
    {
      tree these_arg_types;

      if (DECL_CONSTRUCTOR_FOR_VBASE_P (function))
	{
	  arg_types = hash_tree_chain (integer_type_node,
				       TREE_CHAIN (arg_types));
	  fntype = build_cplus_method_type (qualtype,
					    TREE_TYPE (TREE_TYPE (function)),
					    arg_types);
	  if (raises)
	    {
	      fntype = build_exception_variant (fntype, raises);
	    }
	  TREE_TYPE (function) = fntype;
	  arg_types = TYPE_ARG_TYPES (TREE_TYPE (function));
	}

      these_arg_types = arg_types;

      if (TREE_CODE (TREE_TYPE (function)) == FUNCTION_TYPE)
	/* Only true for static member functions.  */
	these_arg_types = hash_tree_chain (build_pointer_type (qualtype),
					   arg_types);

      DECL_ASSEMBLER_NAME (function)
	= build_decl_overload (fn_name, these_arg_types,
			       1 + DECL_CONSTRUCTOR_P (function));

#if 0
      /* This code is going into the compiler, but currently, it makes
	 libg++/src/Integer.cc not compile.  The problem is that the nice name
	 winds up going into the symbol table, and conversion operations look
	 for the manged name.  */
      substitute_nice_name (function);
#endif
    }

  DECL_ARGUMENTS (function) = last_function_parms;
  /* First approximations.  */
  DECL_CONTEXT (function) = ctype;
  DECL_CLASS_CONTEXT (function) = ctype;
}

/* Work on the expr used by alignof (this is only called by the parser).  */

tree
grok_alignof (expr)
     tree expr;
{
  tree best, t;
  int bestalign;

  if (TREE_CODE (expr) == COMPONENT_REF
      && DECL_BIT_FIELD (TREE_OPERAND (expr, 1)))
    error ("`__alignof__' applied to a bit-field");

  if (TREE_CODE (expr) == INDIRECT_REF)
    {
      best = t = TREE_OPERAND (expr, 0);
      bestalign = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (t)));

      while (TREE_CODE (t) == NOP_EXPR
	     && TREE_CODE (TREE_TYPE (TREE_OPERAND (t, 0))) == POINTER_TYPE)
	{
	  int thisalign;
	  t = TREE_OPERAND (t, 0);
	  thisalign = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (t)));
	  if (thisalign > bestalign)
	    best = t, bestalign = thisalign;
	}
      return c_alignof (TREE_TYPE (TREE_TYPE (best)));
    }
  else
    {
      /* ANSI says arrays and fns are converted inside comma.
	 But we can't convert them in build_compound_expr
	 because that would break commas in lvalues.
	 So do the conversion here if operand was a comma.  */
      if (TREE_CODE (expr) == COMPOUND_EXPR
	  && (TREE_CODE (TREE_TYPE (expr)) == ARRAY_TYPE
	      || TREE_CODE (TREE_TYPE (expr)) == FUNCTION_TYPE))
	expr = default_conversion (expr);
      return c_alignof (TREE_TYPE (expr));
    }
}

/* Create an ARRAY_REF, checking for the user doing things backwards
   along the way.  */

tree
grok_array_decl (array_expr, index_exp)
     tree array_expr, index_exp;
{
  tree type = TREE_TYPE (array_expr);
  tree p1, p2, i1, i2;

  if (type == error_mark_node || index_exp == error_mark_node)
    return error_mark_node;
  if (processing_template_decl)
    return build_min (ARRAY_REF, type ? TREE_TYPE (type) : NULL_TREE,
		      array_expr, index_exp);

  if (type == NULL_TREE)
    {
      /* Something has gone very wrong.  Assume we are mistakenly reducing
	 an expression instead of a declaration.  */
      error ("parser may be lost: is there a '{' missing somewhere?");
      return NULL_TREE;
    }

  if (TREE_CODE (type) == OFFSET_TYPE
      || TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  /* If they have an `operator[]', use that.  */
  if (TYPE_LANG_SPECIFIC (type)
      && TYPE_OVERLOADS_ARRAY_REF (complete_type (type)))
    return build_opfncall (ARRAY_REF, LOOKUP_NORMAL,
			   array_expr, index_exp, NULL_TREE);

  /* Otherwise, create an ARRAY_REF for a pointer or array type.  */

  if (TREE_CODE (type) == ARRAY_TYPE)
    p1 = array_expr;
  else
    p1 = build_expr_type_conversion (WANT_POINTER, array_expr, 0);

  if (TREE_CODE (TREE_TYPE (index_exp)) == ARRAY_TYPE)
    p2 = index_exp;
  else
    p2 = build_expr_type_conversion (WANT_POINTER, index_exp, 0);

  i1 = build_expr_type_conversion (WANT_INT | WANT_ENUM, array_expr, 0);
  i2 = build_expr_type_conversion (WANT_INT | WANT_ENUM, index_exp, 0);

  if ((p1 && i2) && (i1 && p2))
    error ("ambiguous conversion for array subscript");

  if (p1 && i2)
    array_expr = p1, index_exp = i2;
  else if (i1 && p2)
    array_expr = p2, index_exp = i1;
  else
    {
      cp_error ("invalid types `%T[%T]' for array subscript",
		type, TREE_TYPE (index_exp));
      return error_mark_node;
    }

  if (array_expr == error_mark_node || index_exp == error_mark_node)
    error ("ambiguous conversion for array subscript");

  return build_array_ref (array_expr, index_exp);
}

/* Given the cast expression EXP, checking out its validity.   Either return
   an error_mark_node if there was an unavoidable error, return a cast to
   void for trying to delete a pointer w/ the value 0, or return the
   call to delete.  If DOING_VEC is 1, we handle things differently
   for doing an array delete.  If DOING_VEC is 2, they gave us the
   array size as an argument to delete.
   Implements ARM $5.3.4.  This is called from the parser.  */

tree
delete_sanity (exp, size, doing_vec, use_global_delete)
     tree exp, size;
     int doing_vec, use_global_delete;
{
  tree t;
  tree type;
  enum tree_code code;
  /* For a regular vector delete (aka, no size argument) we will pass
     this down as a NULL_TREE into build_vec_delete.  */
  tree maxindex = NULL_TREE;

  if (exp == error_mark_node)
    return exp;

  if (processing_template_decl)
    {
      t = build_min (DELETE_EXPR, void_type_node, exp, size);
      DELETE_EXPR_USE_GLOBAL (t) = use_global_delete;
      DELETE_EXPR_USE_VEC (t) = doing_vec;
      return t;
    }

  t = exp;
  if (TREE_CODE (t) == OFFSET_REF)
    t = resolve_offset_ref (t);
  t = stabilize_reference (convert_from_reference (t));
  type = TREE_TYPE (t);
  code = TREE_CODE (type);

  switch (doing_vec)
    {
    case 2:
      maxindex = build_binary_op (MINUS_EXPR, size, integer_one_node, 1);
      pedwarn ("anachronistic use of array size in vector delete");
      /* Fall through.  */
    case 1:
      break;
    default:
      if (code != POINTER_TYPE)
	{
	  cp_error ("type `%#T' argument given to `delete', expected pointer",
		    type);
	  return error_mark_node;
	}

      /* Deleting a pointer with the value zero is valid and has no effect.  */
      if (integer_zerop (t))
	return build1 (NOP_EXPR, void_type_node, t);
    }

  if (code == POINTER_TYPE)
    {
#if 0
      /* As of Valley Forge, you can delete a pointer to const.  */
      if (TREE_READONLY (TREE_TYPE (type)))
	{
	  error ("`const *' cannot be deleted");
	  return error_mark_node;
	}
#endif
      /* You can't delete functions.  */
      if (TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
	{
	  error ("cannot delete a function");
	  return error_mark_node;
	}
    }

#if 0
  /* If the type has no destructor, then we should build a regular
     delete, instead of a vector delete.  Otherwise, we would end
     up passing a bogus offset into __builtin_delete, which is
     not expecting it.  */ 
  if (doing_vec
      && TREE_CODE (type) == POINTER_TYPE
      && !TYPE_HAS_DESTRUCTOR (TREE_TYPE (type)))
    {
      doing_vec = 0;
      use_global_delete = 1;
    }
#endif

  if (doing_vec)
    return build_vec_delete (t, maxindex, integer_one_node,
			     integer_two_node, use_global_delete);
  else
    {
      if (IS_AGGR_TYPE (TREE_TYPE (type))
	  && TYPE_GETS_REG_DELETE (TREE_TYPE (type)))
	{
	  /* Only do access checking here; we'll be calling op delete
	     from the destructor.  */
	  tree tmp = build_op_delete_call (DELETE_EXPR, t,
					   size_zero_node, LOOKUP_NORMAL);
	  if (tmp == error_mark_node)
	    return error_mark_node;
	}

      return build_delete (type, t, integer_three_node,
			   LOOKUP_NORMAL, use_global_delete);
    }
}

/* Report an error if the indicated template declaration is not the
   sort of thing that should be a member template.  */

void
check_member_template (tmpl)
     tree tmpl;
{
  tree decl;

  my_friendly_assert (TREE_CODE (tmpl) == TEMPLATE_DECL, 0);
  decl = DECL_TEMPLATE_RESULT (tmpl);

  if (TREE_CODE (decl) == FUNCTION_DECL) 
    {
      if (current_function_decl)
	/* 14.5.2.2 [temp.mem]
	   
	   A local class shall not have member templates. */
	cp_error ("declaration of of member template `%#D' in local class",
		  decl);
      
      if (DECL_VIRTUAL_P (decl)) 
	{
	  /* 14.5.2.3 [temp.mem]

	     A member function template shall not be virtual.  */
	  cp_error 
	    ("invalid use of `virtual' in template declaration of `%#D'",
	     decl);
	  DECL_VIRTUAL_P (decl) = 0;
	}

      /* The debug-information generating code doesn't know what to do
	 with member templates.  */ 
      DECL_IGNORED_P (tmpl) = 1;
    } 
  else if (TREE_CODE (decl) == TYPE_DECL &&
	   AGGREGATE_TYPE_P (TREE_TYPE (decl)))
    {
      if (current_function_decl)
	/* 14.5.2.2 [temp.mem]

	   A local class shall not have member templates.  */
	cp_error ("declaration of of member template `%#D' in local class",
		  decl);

      /* We don't handle member template classes yet. */
      sorry ("member templates classes");
    }
  else
    cp_error ("template declaration of `%#D'", decl);
}

/* Sanity check: report error if this function FUNCTION is not
   really a member of the class (CTYPE) it is supposed to belong to.
   CNAME is the same here as it is for grokclassfn above.  */

tree
check_classfn (ctype, function)
     tree ctype, function;
{
  tree fn_name = DECL_NAME (function);
  tree fndecl;
  tree method_vec = CLASSTYPE_METHOD_VEC (complete_type (ctype));
  tree *methods = 0;
  tree *end = 0;
  tree templates = NULL_TREE;

  if (method_vec != 0)
    {
      methods = &TREE_VEC_ELT (method_vec, 0);
      end = TREE_VEC_END (method_vec);

      /* First suss out ctors and dtors.  */
      if (*methods && fn_name == DECL_NAME (*methods)
	  && DECL_CONSTRUCTOR_P (function))
	goto got_it;
      if (*++methods && fn_name == DECL_NAME (*methods)
	  && DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (function)))
	goto got_it;

      while (++methods != end)
	{
	  fndecl = *methods;
	  if (fn_name == DECL_NAME (*methods))
	    {
	    got_it:
	      fndecl = *methods;
	      while (fndecl)
		{
		  if (DECL_ASSEMBLER_NAME (function) == DECL_ASSEMBLER_NAME (fndecl))
		    return fndecl;
#if 0
		  /* This doesn't work for static member functions that are
                     pretending to be methods.  */
		  /* We have to do more extensive argument checking here, as
		     the name may have been changed by asm("new_name").  */
		  if (decls_match (function, fndecl))
		    return fndecl;
#else
		  if (DECL_NAME (function) == DECL_NAME (fndecl))
		    {
		      tree p1 = TYPE_ARG_TYPES (TREE_TYPE (function));
		      tree p2 = TYPE_ARG_TYPES (TREE_TYPE (fndecl));

		      /* Get rid of the this parameter on functions that become
			 static.  */
		      if (DECL_STATIC_FUNCTION_P (fndecl)
			  && TREE_CODE (TREE_TYPE (function)) == METHOD_TYPE)
			p1 = TREE_CHAIN (p1);

		      if (comptypes (TREE_TYPE (TREE_TYPE (function)),
				     TREE_TYPE (TREE_TYPE (fndecl)), 1)
			  && compparms (p1, p2, 3)
			  && (DECL_TEMPLATE_SPECIALIZATION (function)
			      == DECL_TEMPLATE_SPECIALIZATION (fndecl))
			  && (!DECL_TEMPLATE_SPECIALIZATION (function)
			      || (DECL_TI_TEMPLATE (function) 
				  == DECL_TI_TEMPLATE (fndecl))))
			return fndecl;

		      if (is_member_template (fndecl))
			/* This function might be an instantiation
			   or specialization of fndecl.  */
			templates = 
			  scratch_tree_cons (NULL_TREE, fndecl, templates);
		    }
#endif
		  fndecl = DECL_CHAIN (fndecl);
		}
	      break;		/* loser */
	    }
	  else if (TREE_CODE (fndecl) == TEMPLATE_DECL 
		   && IDENTIFIER_TYPENAME_P (DECL_NAME (fndecl))
		   && IDENTIFIER_TYPENAME_P (fn_name))
	    /* The method in the class is a member template
	       conversion operator.  We are declaring another
	       conversion operator.  It is possible that even though
	       the names don't match, there is some specialization
	       occurring.  */
	    templates = 
	      scratch_tree_cons (NULL_TREE, fndecl, templates);
	}
    }

  if (templates)
    /* This function might be an instantiation or a specialization.
       We should verify that this is possible.  If it is, we must
       somehow add the new declaration to the method vector for the
       class.  Perhaps we should use add_method?  For now, we simply
       return NULL_TREE, which lets the caller know that this
       function is new, but we don't print an error message.  */
    return NULL_TREE;

  if (methods != end)
    {
      tree fndecl = *methods;
      cp_error ("prototype for `%#D' does not match any in class `%T'",
		function, ctype);
      cp_error_at ("candidate%s: %+#D", DECL_CHAIN (fndecl) ? "s are" : " is",
		   fndecl);
      while (fndecl = DECL_CHAIN (fndecl), fndecl)
	cp_error_at ("                %#D", fndecl);
    }
  else
    {
      methods = 0;
      cp_error ("no `%#D' member function declared in class `%T'",
		function, ctype);
    }

  /* If we did not find the method in the class, add it to avoid
     spurious errors.  */
  add_method (ctype, methods, function);
  return NULL_TREE;
}

/* Process the specs, declarator (NULL if omitted) and width (NULL if omitted)
   of a structure component, returning a FIELD_DECL node.
   QUALS is a list of type qualifiers for this decl (such as for declaring
   const member functions).

   This is done during the parsing of the struct declaration.
   The FIELD_DECL nodes are chained together and the lot of them
   are ultimately passed to `build_struct' to make the RECORD_TYPE node.

   C++:

   If class A defines that certain functions in class B are friends, then
   the way I have set things up, it is B who is interested in permission
   granted by A.  However, it is in A's context that these declarations
   are parsed.  By returning a void_type_node, class A does not attempt
   to incorporate the declarations of the friends within its structure.

   DO NOT MAKE ANY CHANGES TO THIS CODE WITHOUT MAKING CORRESPONDING
   CHANGES TO CODE IN `start_method'.  */

tree
grokfield (declarator, declspecs, init, asmspec_tree, attrlist)
     tree declarator, declspecs, init, asmspec_tree, attrlist;
{
  register tree value;
  char *asmspec = 0;
  int flags = LOOKUP_ONLYCONVERTING;

  /* Convert () initializers to = initializers.  */
  if (init == NULL_TREE && declarator != NULL_TREE
      && TREE_CODE (declarator) == CALL_EXPR
      && TREE_OPERAND (declarator, 0)
      && (TREE_CODE (TREE_OPERAND (declarator, 0)) == IDENTIFIER_NODE
	  || TREE_CODE (TREE_OPERAND (declarator, 0)) == SCOPE_REF)
      && parmlist_is_exprlist (TREE_OPERAND (declarator, 1)))
    {
      init = TREE_OPERAND (declarator, 1);
      declarator = TREE_OPERAND (declarator, 0);
      flags = 0;
    }

  if (declspecs == NULL_TREE
      && TREE_CODE (declarator) == SCOPE_REF
      && TREE_CODE (TREE_OPERAND (declarator, 1)) == IDENTIFIER_NODE)
    {
      /* Access declaration */
      if (! IS_AGGR_TYPE_CODE (TREE_CODE (TREE_OPERAND (declarator, 0))))
	;
      else if (TREE_COMPLEXITY (declarator) == current_class_depth)
	pop_nested_class (1);
      return do_class_using_decl (declarator);
    }

  if (init
      && TREE_CODE (init) == TREE_LIST
      && TREE_VALUE (init) == error_mark_node
      && TREE_CHAIN (init) == NULL_TREE)
    init = NULL_TREE;

  value = grokdeclarator (declarator, declspecs, FIELD, init != 0, NULL_TREE);
  if (! value)
    return value; /* friend or constructor went bad.  */

  /* Pass friendly classes back.  */
  if (TREE_CODE (value) == VOID_TYPE)
    return void_type_node;

  if (DECL_NAME (value) != NULL_TREE
      && IDENTIFIER_POINTER (DECL_NAME (value))[0] == '_'
      && ! strcmp (IDENTIFIER_POINTER (DECL_NAME (value)), "_vptr"))
    cp_error ("member `%D' conflicts with virtual function table field name", value);

  /* Stash away type declarations.  */
  if (TREE_CODE (value) == TYPE_DECL)
    {
      DECL_NONLOCAL (value) = 1;
      DECL_CONTEXT (value) = current_class_type;
      DECL_CLASS_CONTEXT (value) = current_class_type;
      CLASSTYPE_LOCAL_TYPEDECLS (current_class_type) = 1;

      pushdecl_class_level (value);
      return value;
    }

  if (IS_SIGNATURE (current_class_type)
      && TREE_CODE (value) != FUNCTION_DECL)
    {
      error ("field declaration not allowed in signature");
      return void_type_node;
    }

  if (DECL_IN_AGGR_P (value))
    {
      cp_error ("`%D' is already defined in the class %T", value,
		  DECL_CONTEXT (value));
      return void_type_node;
    }

  if (asmspec_tree)
    asmspec = TREE_STRING_POINTER (asmspec_tree);

  if (init)
    {
      if (IS_SIGNATURE (current_class_type)
	  && TREE_CODE (value) == FUNCTION_DECL)
	{
	  error ("function declarations cannot have initializers in signature");
	  init = NULL_TREE;
	}
      else if (TREE_CODE (value) == FUNCTION_DECL)
	{
	  grok_function_init (value, init);
	  init = NULL_TREE;
	}
      else if (pedantic && TREE_CODE (value) != VAR_DECL)
	/* Already complained in grokdeclarator.  */
	init = NULL_TREE;
      else
	{
	  /* We allow initializers to become parameters to base
             initializers.  */
	  if (TREE_CODE (init) == TREE_LIST)
	    {
	      if (TREE_CHAIN (init) == NULL_TREE)
		init = TREE_VALUE (init);
	      else
		init = digest_init (TREE_TYPE (value), init, (tree *)0);
	    }
	  
	  if (TREE_CODE (init) == CONST_DECL)
	    init = DECL_INITIAL (init);
	  else if (TREE_READONLY_DECL_P (init))
	    init = decl_constant_value (init);
	  else if (TREE_CODE (init) == CONSTRUCTOR)
	    init = digest_init (TREE_TYPE (value), init, (tree *)0);
	  my_friendly_assert (TREE_PERMANENT (init), 192);
	  if (init == error_mark_node)
	    /* We must make this look different than `error_mark_node'
	       because `decl_const_value' would mis-interpret it
	       as only meaning that this VAR_DECL is defined.  */
	    init = build1 (NOP_EXPR, TREE_TYPE (value), init);
	  else if (processing_template_decl)
	    ;
	  else if (! TREE_CONSTANT (init))
	    {
	      /* We can allow references to things that are effectively
		 static, since references are initialized with the address.  */
	      if (TREE_CODE (TREE_TYPE (value)) != REFERENCE_TYPE
		  || (TREE_STATIC (init) == 0
		      && (TREE_CODE_CLASS (TREE_CODE (init)) != 'd'
			  || DECL_EXTERNAL (init) == 0)))
		{
		  error ("field initializer is not constant");
		  init = error_mark_node;
		}
	    }
	}
    }

  /* The corresponding pop_obstacks is in cp_finish_decl.  */
  push_obstacks_nochange ();

  if (processing_template_decl && ! current_function_decl
      && (TREE_CODE (value) == VAR_DECL || TREE_CODE (value) == FUNCTION_DECL))
    push_template_decl (value);

  if (attrlist)
    cplus_decl_attributes (value, TREE_PURPOSE (attrlist),
			   TREE_VALUE (attrlist));

  if (TREE_CODE (value) == VAR_DECL)
    {
      /* We cannot call pushdecl here, because that would
	 fill in the value of our TREE_CHAIN.  Instead, we
	 modify cp_finish_decl to do the right thing, namely, to
	 put this decl out straight away.  */
      if (TREE_PUBLIC (value))
	{
	  /* current_class_type can be NULL_TREE in case of error.  */
	  if (asmspec == 0 && current_class_type)
	    {
	      TREE_PUBLIC (value) = 1;
	      DECL_INITIAL (value) = error_mark_node;
	      DECL_ASSEMBLER_NAME (value)
		= build_static_name (current_class_type, DECL_NAME (value));
	    }
	  if (! processing_template_decl)
	    pending_statics = perm_tree_cons (NULL_TREE, value, pending_statics);

	  /* Static consts need not be initialized in the class definition.  */
	  if (init != NULL_TREE && TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (value)))
	    {
	      static int explanation = 0;

	      error ("initializer invalid for static member with constructor");
	      if (explanation++ == 0)
		error ("(you really want to initialize it separately)");
	      init = 0;
	    }
	  /* Force the compiler to know when an uninitialized static
	     const member is being used.  */
	  if (TYPE_READONLY (value) && init == 0)
	    TREE_USED (value) = 1;
	}
      DECL_INITIAL (value) = init;
      DECL_IN_AGGR_P (value) = 1;
      DECL_CONTEXT (value) = current_class_type;
      DECL_CLASS_CONTEXT (value) = current_class_type;

      cp_finish_decl (value, init, asmspec_tree, 1, flags);
      pushdecl_class_level (value);
      return value;
    }
  if (TREE_CODE (value) == FIELD_DECL)
    {
      if (asmspec)
	{
	  /* This must override the asm specifier which was placed
	     by grokclassfn.  Lay this out fresh.  */
	  DECL_RTL (value) = NULL_RTX;
	  DECL_ASSEMBLER_NAME (value) = get_identifier (asmspec);
	}
      if (DECL_INITIAL (value) == error_mark_node)
	init = error_mark_node;
      cp_finish_decl (value, init, asmspec_tree, 1, flags);
      DECL_INITIAL (value) = init;
      DECL_IN_AGGR_P (value) = 1;
      return value;
    }
  if (TREE_CODE (value) == FUNCTION_DECL)
    {
      check_default_args (value);
      if (DECL_CHAIN (value) != NULL_TREE)
	{
	  /* Need a fresh node here so that we don't get circularity
	     when we link these together.  */
	  value = copy_node (value);
	  /* When does this happen?  */
	  my_friendly_assert (init == NULL_TREE, 193);
	}
      if (asmspec)
	{
	  /* This must override the asm specifier which was placed
	     by grokclassfn.  Lay this out fresh.  */
	  DECL_RTL (value) = NULL_RTX;
	  DECL_ASSEMBLER_NAME (value) = get_identifier (asmspec);
	}
      cp_finish_decl (value, init, asmspec_tree, 1, flags);

      /* Pass friends back this way.  */
      if (DECL_FRIEND_P (value))
	return void_type_node;

#if 0 /* Just because a fn is declared doesn't mean we'll try to define it.  */
      if (current_function_decl && ! IS_SIGNATURE (current_class_type))
	cp_error ("method `%#D' of local class must be defined in class body",
		  value);
#endif

      DECL_IN_AGGR_P (value) = 1;
      return value;
    }
  my_friendly_abort (21);
  /* NOTREACHED */
  return NULL_TREE;
}

/* Like `grokfield', but for bitfields.
   WIDTH is non-NULL for bit fields only, and is an INTEGER_CST node.  */

tree
grokbitfield (declarator, declspecs, width)
     tree declarator, declspecs, width;
{
  register tree value = grokdeclarator (declarator, declspecs, BITFIELD,
					0, NULL_TREE);

  if (! value) return NULL_TREE; /* friends went bad.  */

  /* Pass friendly classes back.  */
  if (TREE_CODE (value) == VOID_TYPE)
    return void_type_node;

  if (TREE_CODE (value) == TYPE_DECL)
    {
      cp_error ("cannot declare `%D' to be a bitfield type", value);
      return NULL_TREE;
    }

  if (IS_SIGNATURE (current_class_type))
    {
      error ("field declaration not allowed in signature");
      return void_type_node;
    }

  if (DECL_IN_AGGR_P (value))
    {
      cp_error ("`%D' is already defined in the class %T", value,
		  DECL_CONTEXT (value));
      return void_type_node;
    }

  GNU_xref_member (current_class_name, value);

  if (TREE_STATIC (value))
    {
      cp_error ("static member `%D' cannot be a bitfield", value);
      return NULL_TREE;
    }
  cp_finish_decl (value, NULL_TREE, NULL_TREE, 0, 0);

  if (width != error_mark_node)
    {
      constant_expression_warning (width);
      DECL_INITIAL (value) = width;
      DECL_BIT_FIELD (value) = 1;
    }

  DECL_IN_AGGR_P (value) = 1;
  return value;
}

tree
grokoptypename (declspecs, declarator)
     tree declspecs, declarator;
{
  tree t = grokdeclarator (declarator, declspecs, TYPENAME, 0, NULL_TREE);
  return build_typename_overload (t);
}

/* When a function is declared with an initializer,
   do the right thing.  Currently, there are two possibilities:

   class B
   {
    public:
     // initialization possibility #1.
     virtual void f () = 0;
     int g ();
   };
   
   class D1 : B
   {
    public:
     int d1;
     // error, no f ();
   };
   
   class D2 : B
   {
    public:
     int d2;
     void f ();
   };
   
   class D3 : B
   {
    public:
     int d3;
     // initialization possibility #2
     void f () = B::f;
   };

*/

int
copy_assignment_arg_p (parmtype, virtualp)
     tree parmtype;
     int virtualp;
{
  if (current_class_type == NULL_TREE)
    return 0;

  if (TREE_CODE (parmtype) == REFERENCE_TYPE)
    parmtype = TREE_TYPE (parmtype);

  if ((TYPE_MAIN_VARIANT (parmtype) == current_class_type)
#if 0
      /* Non-standard hack to support old Booch components.  */
      || (! virtualp && DERIVED_FROM_P (parmtype, current_class_type))
#endif
      )
    return 1;

  return 0;
}

static void
grok_function_init (decl, init)
     tree decl;
     tree init;
{
  /* An initializer for a function tells how this function should
     be inherited.  */
  tree type = TREE_TYPE (decl);

  if (TREE_CODE (type) == FUNCTION_TYPE)
    cp_error ("initializer specified for non-member function `%D'", decl);
#if 0
  /* We'll check for this in finish_struct_1.  */
  else if (DECL_VINDEX (decl) == NULL_TREE)
    cp_error ("initializer specified for non-virtual method `%D'", decl);
#endif
  else if (integer_zerop (init))
    {
#if 0
      /* Mark this function as being "defined".  */
      DECL_INITIAL (decl) = error_mark_node;
      /* pure virtual destructors must be defined.  */
      /* pure virtual needs to be defined (as abort) only when put in 
	 vtbl. For wellformed call, it should be itself. pr4737 */
      if (!DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (decl)))
	{
	  extern tree abort_fndecl;
	  /* Give this node rtl from `abort'.  */
	  DECL_RTL (decl) = DECL_RTL (abort_fndecl);
	}
#endif
      DECL_ABSTRACT_VIRTUAL_P (decl) = 1;
      if (DECL_NAME (decl) == ansi_opname [(int) MODIFY_EXPR])
	{
	  tree parmtype
	    = TREE_VALUE (TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (decl))));

	  if (copy_assignment_arg_p (parmtype, 1))
	    TYPE_HAS_ABSTRACT_ASSIGN_REF (current_class_type) = 1;
	}
    }
  else if (TREE_CODE (init) == OFFSET_REF
	   && TREE_OPERAND (init, 0) == NULL_TREE
	   && TREE_CODE (TREE_TYPE (init)) == METHOD_TYPE)
    {
      tree basetype = DECL_CLASS_CONTEXT (init);
      tree basefn = TREE_OPERAND (init, 1);
      if (TREE_CODE (basefn) != FUNCTION_DECL)
	cp_error ("non-method initializer invalid for method `%D'", decl);
      else if (! BINFO_OFFSET_ZEROP (TYPE_BINFO (DECL_CLASS_CONTEXT (basefn))))
	sorry ("base member function from other than first base class");
      else
	{
	  tree binfo = get_binfo (basetype, TYPE_METHOD_BASETYPE (type), 1);
	  if (binfo == error_mark_node)
	    ;
	  else if (binfo == 0)
	    error_not_base_type (TYPE_METHOD_BASETYPE (TREE_TYPE (init)),
				 TYPE_METHOD_BASETYPE (type));
	  else
	    {
	      /* Mark this function as being defined,
		 and give it new rtl.  */
	      DECL_INITIAL (decl) = error_mark_node;
	      DECL_RTL (decl) = DECL_RTL (basefn);
	    }
	}
    }
  else
    cp_error ("invalid initializer for virtual method `%D'", decl);
}

void
cplus_decl_attributes (decl, attributes, prefix_attributes)
     tree decl, attributes, prefix_attributes;
{
  if (decl == NULL_TREE || decl == void_type_node)
    return;

  if (TREE_CODE (decl) == TEMPLATE_DECL)
    decl = DECL_TEMPLATE_RESULT (decl);

  decl_attributes (decl, attributes, prefix_attributes);

  if (TREE_CODE (decl) == TYPE_DECL)
    SET_IDENTIFIER_TYPE_VALUE (DECL_NAME (decl), TREE_TYPE (decl));
}

/* CONSTRUCTOR_NAME:
   Return the name for the constructor (or destructor) for the
   specified class.  Argument can be RECORD_TYPE, TYPE_DECL, or
   IDENTIFIER_NODE.  When given a template, this routine doesn't
   lose the specialization.  */

tree
constructor_name_full (thing)
     tree thing;
{
  if (TREE_CODE (thing) == TEMPLATE_TYPE_PARM)
    thing = TYPE_NAME (thing);
  else if (IS_AGGR_TYPE_CODE (TREE_CODE (thing)))
    {
      if (TYPE_WAS_ANONYMOUS (thing) && TYPE_HAS_CONSTRUCTOR (thing))
	thing = DECL_NAME (TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (thing), 0));
      else
	thing = TYPE_NAME (thing);
    }
  if (TREE_CODE (thing) == TYPE_DECL
      || (TREE_CODE (thing) == TEMPLATE_DECL
	  && TREE_CODE (DECL_TEMPLATE_RESULT (thing)) == TYPE_DECL))
    thing = DECL_NAME (thing);
  my_friendly_assert (TREE_CODE (thing) == IDENTIFIER_NODE, 197);
  return thing;
}

/* CONSTRUCTOR_NAME:
   Return the name for the constructor (or destructor) for the
   specified class.  Argument can be RECORD_TYPE, TYPE_DECL, or
   IDENTIFIER_NODE.  When given a template, return the plain
   unspecialized name.  */

tree
constructor_name (thing)
     tree thing;
{
  tree t;
  thing = constructor_name_full (thing);
  t = IDENTIFIER_TEMPLATE (thing);
  if (!t)
    return thing;
  return t;
}

/* Cache the value of this class's main virtual function table pointer
   in a register variable.  This will save one indirection if a
   more than one virtual function call is made this function.  */

void
setup_vtbl_ptr ()
{
  extern tree base_init_expr;

  if (base_init_expr == 0
      && DECL_CONSTRUCTOR_P (current_function_decl))
    {
      if (processing_template_decl)
	add_tree (build_min_nt
		  (CTOR_INITIALIZER,
		   current_member_init_list, current_base_init_list));
      else
	emit_base_init (current_class_type, 0);
    }
}

/* Record the existence of an addressable inline function.  */

void
mark_inline_for_output (decl)
     tree decl;
{
  decl = DECL_MAIN_VARIANT (decl);
  if (DECL_SAVED_INLINE (decl))
    return;
  my_friendly_assert (TREE_PERMANENT (decl), 363);
  DECL_SAVED_INLINE (decl) = 1;
#if 0
  if (DECL_PENDING_INLINE_INFO (decl) != 0
      && ! DECL_PENDING_INLINE_INFO (decl)->deja_vu)
    {
      struct pending_inline *t = pending_inlines;
      my_friendly_assert (DECL_SAVED_INSNS (decl) == 0, 198);
      while (t)
	{
	  if (t == DECL_PENDING_INLINE_INFO (decl))
	    break;
	  t = t->next;
	}
      if (t == 0)
	{
	  t = DECL_PENDING_INLINE_INFO (decl);
	  t->next = pending_inlines;
	  pending_inlines = t;
	}
      DECL_PENDING_INLINE_INFO (decl) = 0;
    }
#endif
  saved_inlines = perm_tree_cons (NULL_TREE, decl, saved_inlines);
}

void
clear_temp_name ()
{
  temp_name_counter = 0;
}

/* Hand off a unique name which can be used for variable we don't really
   want to know about anyway, for example, the anonymous variables which
   are needed to make references work.  Declare this thing so we can use it.
   The variable created will be of type TYPE.

   STATICP is nonzero if this variable should be static.  */

tree
get_temp_name (type, staticp)
     tree type;
     int staticp;
{
  char buf[sizeof (AUTO_TEMP_FORMAT) + 20];
  tree decl;
  int toplev = toplevel_bindings_p ();

  push_obstacks_nochange ();
  if (toplev || staticp)
    {
      end_temporary_allocation ();
      sprintf (buf, AUTO_TEMP_FORMAT, global_temp_name_counter++);
      decl = pushdecl_top_level (build_decl (VAR_DECL, get_identifier (buf), type));
    }
  else
    {
      sprintf (buf, AUTO_TEMP_FORMAT, temp_name_counter++);
      decl = pushdecl (build_decl (VAR_DECL, get_identifier (buf), type));
    }
  TREE_USED (decl) = 1;
  TREE_STATIC (decl) = staticp;
  DECL_ARTIFICIAL (decl) = 1;

  /* If this is a local variable, then lay out its rtl now.
     Otherwise, callers of this function are responsible for dealing
     with this variable's rtl.  */
  if (! toplev)
    {
      expand_decl (decl);
      expand_decl_init (decl);
    }
  pop_obstacks ();

  return decl;
}

/* Get a variable which we can use for multiple assignments.
   It is not entered into current_binding_level, because
   that breaks things when it comes time to do final cleanups
   (which take place "outside" the binding contour of the function).  */

tree
get_temp_regvar (type, init)
     tree type, init;
{
  static char buf[sizeof (AUTO_TEMP_FORMAT) + 20] = { '_' };
  tree decl;

  sprintf (buf+1, AUTO_TEMP_FORMAT, temp_name_counter++);
  decl = build_decl (VAR_DECL, get_identifier (buf), type);
  TREE_USED (decl) = 1;
  DECL_REGISTER (decl) = 1;

  if (init)
    store_init_value (decl, init);

  /* We can expand these without fear, since they cannot need
     constructors or destructors.  */
  expand_decl (decl);
  expand_decl_init (decl);

  return decl;
}

/* Finish off the processing of a UNION_TYPE structure.
   If there are static members, then all members are
   static, and must be laid out together.  If the
   union is an anonymous union, we arrange for that
   as well.  PUBLIC_P is nonzero if this union is
   not declared static.  */

void
finish_anon_union (anon_union_decl)
     tree anon_union_decl;
{
  tree type = TREE_TYPE (anon_union_decl);
  tree field, main_decl = NULL_TREE;
  tree elems = NULL_TREE;
  int public_p = TREE_PUBLIC (anon_union_decl);
  int static_p = TREE_STATIC (anon_union_decl);
  int external_p = DECL_EXTERNAL (anon_union_decl);

  if ((field = TYPE_FIELDS (type)) == NULL_TREE)
    return;

  if (public_p)
    {
      error ("global anonymous unions must be declared static");
      return;
    }

  for (; field; field = TREE_CHAIN (field))
    {
      tree decl;
      if (TREE_CODE (field) != FIELD_DECL)
	continue;

      if (TREE_PRIVATE (field))
	cp_pedwarn_at ("private member `%#D' in anonymous union", field);
      else if (TREE_PROTECTED (field))
	cp_pedwarn_at ("protected member `%#D' in anonymous union", field);

      decl = build_decl (VAR_DECL, DECL_NAME (field), TREE_TYPE (field));
      /* tell `pushdecl' that this is not tentative.  */
      DECL_INITIAL (decl) = error_mark_node;
      TREE_PUBLIC (decl) = public_p;
      TREE_STATIC (decl) = static_p;
      DECL_EXTERNAL (decl) = external_p;
      decl = pushdecl (decl);

      /* Only write out one anon union element--choose the one that
	 can hold them all.  */
      if (main_decl == NULL_TREE
	  && 1 == simple_cst_equal (DECL_SIZE (decl),
				    DECL_SIZE (anon_union_decl)))
	{
	  main_decl = decl;
	}
      else
	{
	  /* ??? This causes there to be no debug info written out
	     about this decl.  */
	  TREE_ASM_WRITTEN (decl) = 1;
	}

      DECL_INITIAL (decl) = NULL_TREE;
      /* If there's a cleanup to do, it belongs in the
	 TREE_PURPOSE of the following TREE_LIST.  */
      elems = scratch_tree_cons (NULL_TREE, decl, elems);
      TREE_TYPE (elems) = type;
    }
  if (static_p)
    {
      if (main_decl)
	{
	  make_decl_rtl (main_decl, 0, toplevel_bindings_p ());
	  DECL_RTL (anon_union_decl) = DECL_RTL (main_decl);
	}
      else
	{
	  warning ("anonymous union with no members");
	  return;
	}
    }

  /* The following call assumes that there are never any cleanups
     for anonymous unions--a reasonable assumption.  */
  expand_anon_union_decl (anon_union_decl, NULL_TREE, elems);
}

/* Finish and output a table which is generated by the compiler.
   NAME is the name to give the table.
   TYPE is the type of the table entry.
   INIT is all the elements in the table.
   PUBLICP is non-zero if this table should be given external access.  */

tree
finish_table (name, type, init, publicp)
     tree name, type, init;
     int publicp;
{
  tree itype, atype, decl;
  static tree empty_table;
  int is_empty = 0;
  tree asmspec;

  itype = build_index_type (size_int (list_length (init) - 1));
  atype = build_cplus_array_type (type, itype);
  layout_type (atype);

  if (TREE_VALUE (init) == integer_zero_node
      && TREE_CHAIN (init) == NULL_TREE)
    {
#if 0
      if (empty_table == NULL_TREE)
#endif
	{
	  empty_table = get_temp_name (atype, 1);
	  init = build (CONSTRUCTOR, atype, NULL_TREE, init);
	  TREE_CONSTANT (init) = 1;
	  TREE_STATIC (init) = 1;
	  DECL_INITIAL (empty_table) = init;
	  asmspec = build_string (IDENTIFIER_LENGTH (DECL_NAME (empty_table)),
				  IDENTIFIER_POINTER (DECL_NAME (empty_table)));
	  cp_finish_decl (empty_table, NULL_TREE, asmspec, 0, 0);
	}
      is_empty = 1;
    }

  if (name == NULL_TREE)
    {
      if (is_empty)
	return empty_table;
      decl = get_temp_name (atype, 1);
    }
  else
    {
      decl = build_decl (VAR_DECL, name, atype);
      decl = pushdecl (decl);
      TREE_STATIC (decl) = 1;
    }

  if (is_empty == 0)
    {
      TREE_PUBLIC (decl) = publicp;
      init = build (CONSTRUCTOR, atype, NULL_TREE, init);
      TREE_CONSTANT (init) = 1;
      TREE_STATIC (init) = 1;
      DECL_INITIAL (decl) = init;
      asmspec = build_string (IDENTIFIER_LENGTH (DECL_NAME (decl)),
			      IDENTIFIER_POINTER (DECL_NAME (decl)));
    }
  else
    {
      /* This will cause DECL to point to EMPTY_TABLE in rtl-land.  */
      DECL_EXTERNAL (decl) = 1;
      TREE_STATIC (decl) = 0;
      init = 0;
      asmspec = build_string (IDENTIFIER_LENGTH (DECL_NAME (empty_table)),
			      IDENTIFIER_POINTER (DECL_NAME (empty_table)));
    }

  cp_finish_decl (decl, NULL_TREE, asmspec, 0, 0);
  return decl;
}

/* Finish processing a builtin type TYPE.  It's name is NAME,
   its fields are in the array FIELDS.  LEN is the number of elements
   in FIELDS minus one, or put another way, it is the maximum subscript
   used in FIELDS.

   It is given the same alignment as ALIGN_TYPE.  */

void
finish_builtin_type (type, name, fields, len, align_type)
     tree type;
     char *name;
     tree fields[];
     int len;
     tree align_type;
{
  register int i;

  TYPE_FIELDS (type) = fields[0];
  for (i = 0; i < len; i++)
    {
      layout_type (TREE_TYPE (fields[i]));
      DECL_FIELD_CONTEXT (fields[i]) = type;
      TREE_CHAIN (fields[i]) = fields[i+1];
    }
  DECL_FIELD_CONTEXT (fields[i]) = type;
  DECL_CLASS_CONTEXT (fields[i]) = type;
  TYPE_ALIGN (type) = TYPE_ALIGN (align_type);
  layout_type (type);
#if 0 /* not yet, should get fixed properly later */
  TYPE_NAME (type) = make_type_decl (get_identifier (name), type);
#else
  TYPE_NAME (type) = build_decl (TYPE_DECL, get_identifier (name), type);
#endif
  TYPE_STUB_DECL (type) = TYPE_NAME (type);
  layout_decl (TYPE_NAME (type), 0);
}

/* Auxiliary functions to make type signatures for
   `operator new' and `operator delete' correspond to
   what compiler will be expecting.  */

extern tree sizetype;

tree
coerce_new_type (type)
     tree type;
{
  int e1 = 0, e2 = 0;

  if (TREE_CODE (type) == METHOD_TYPE)
    type = build_function_type (TREE_TYPE (type), TREE_CHAIN (TYPE_ARG_TYPES (type)));
  if (TREE_TYPE (type) != ptr_type_node)
    e1 = 1, error ("`operator new' must return type `void *'");

  /* Technically the type must be `size_t', but we may not know
     what that is.  */
  if (TYPE_ARG_TYPES (type) == NULL_TREE)
    e1 = 1, error ("`operator new' takes type `size_t' parameter");
  else if (TREE_CODE (TREE_VALUE (TYPE_ARG_TYPES (type))) != INTEGER_TYPE
	   || TYPE_PRECISION (TREE_VALUE (TYPE_ARG_TYPES (type))) != TYPE_PRECISION (sizetype))
    e2 = 1, error ("`operator new' takes type `size_t' as first parameter");
  if (e2)
    type = build_function_type (ptr_type_node, tree_cons (NULL_TREE, sizetype, TREE_CHAIN (TYPE_ARG_TYPES (type))));
  else if (e1)
    type = build_function_type (ptr_type_node, TYPE_ARG_TYPES (type));
  return type;
}

tree
coerce_delete_type (type)
     tree type;
{
  int e1 = 0, e2 = 0, e3 = 0;
  tree arg_types = TYPE_ARG_TYPES (type);

  if (TREE_CODE (type) == METHOD_TYPE)
    {
      type = build_function_type (TREE_TYPE (type), TREE_CHAIN (arg_types));
      arg_types = TREE_CHAIN (arg_types);
    }

  if (TREE_TYPE (type) != void_type_node)
    e1 = 1, error ("`operator delete' must return type `void'");

  if (arg_types == NULL_TREE
      || TREE_VALUE (arg_types) != ptr_type_node)
    e2 = 1, error ("`operator delete' takes type `void *' as first parameter");

#if 0
  if (arg_types
      && TREE_CHAIN (arg_types)
      && TREE_CHAIN (arg_types) != void_list_node)
    {
      /* Again, technically this argument must be `size_t', but again
	 we may not know what that is.  */
      tree t2 = TREE_VALUE (TREE_CHAIN (arg_types));
      if (TREE_CODE (t2) != INTEGER_TYPE
	  || TYPE_PRECISION (t2) != TYPE_PRECISION (sizetype))
	e3 = 1, error ("second argument to `operator delete' must be of type `size_t'");
      else if (TREE_CHAIN (TREE_CHAIN (arg_types)) != void_list_node)
	{
	  e3 = 1;
	  if (TREE_CHAIN (TREE_CHAIN (arg_types)))
	    error ("too many arguments in declaration of `operator delete'");
	  else
	    error ("`...' invalid in specification of `operator delete'");
	}
    }

  if (e3)
    arg_types = tree_cons (NULL_TREE, ptr_type_node,
			   build_tree_list (NULL_TREE, sizetype));
  else if (e3 |= e2)
    {
      if (arg_types == NULL_TREE)
	arg_types = tree_cons (NULL_TREE, ptr_type_node, void_list_node);
      else
	arg_types = tree_cons (NULL_TREE, ptr_type_node, TREE_CHAIN (arg_types));
    }
  else e3 |= e1;
#endif

  if (e2)
    arg_types = tree_cons (NULL_TREE, ptr_type_node,
			   arg_types ? TREE_CHAIN (arg_types): NULL_TREE);
  if (e2 || e1)
    type = build_function_type (void_type_node, arg_types);

  return type;
}

extern tree abort_fndecl;

static void
mark_vtable_entries (decl)
     tree decl;
{
  tree entries = CONSTRUCTOR_ELTS (DECL_INITIAL (decl));

  if (flag_rtti)
    {
      tree fnaddr = (flag_vtable_thunks ? TREE_VALUE (TREE_CHAIN (entries))
		     : FNADDR_FROM_VTABLE_ENTRY (TREE_VALUE (entries)));
      tree fn = TREE_OPERAND (fnaddr, 0);
      TREE_ADDRESSABLE (fn) = 1;
      mark_used (fn);
    }
  skip_rtti_stuff (&entries);

  for (; entries; entries = TREE_CHAIN (entries))
    {
      tree fnaddr = (flag_vtable_thunks ? TREE_VALUE (entries) 
		     : FNADDR_FROM_VTABLE_ENTRY (TREE_VALUE (entries)));
      tree fn = TREE_OPERAND (fnaddr, 0);
      TREE_ADDRESSABLE (fn) = 1;
      if (DECL_LANG_SPECIFIC (fn) && DECL_ABSTRACT_VIRTUAL_P (fn))
	{
	  TREE_OPERAND (fnaddr, 0) = fn = copy_node (fn);
	  DECL_RTL (fn) = DECL_RTL (abort_fndecl);
	  mark_used (abort_fndecl);
	}
      if (TREE_CODE (fn) == THUNK_DECL && DECL_EXTERNAL (fn))
	{
	  DECL_EXTERNAL (fn) = 0;
	  emit_thunk (fn);
	}
      mark_used (fn);
    }
}

/* Set DECL up to have the closest approximation of "initialized common"
   linkage available.  */

void
comdat_linkage (decl)
     tree decl;
{
  if (flag_weak)
    make_decl_one_only (decl);
  else
    TREE_PUBLIC (decl) = 0;
}

/* Set TREE_PUBLIC and/or DECL_EXTERN on the vtable DECL,
   based on TYPE and other static flags.

   Note that anything public is tagged TREE_PUBLIC, whether
   it's public in this file or in another one.  */

void
import_export_vtable (decl, type, final)
     tree decl, type;
     int final;
{
  if (DECL_INTERFACE_KNOWN (decl))
    return;

  /* +e0 or +e1 */
  if (write_virtuals < 2 && write_virtuals != 0)
    {
      TREE_PUBLIC (decl) = 1;
      if (write_virtuals < 0)
	DECL_EXTERNAL (decl) = 1;
      DECL_INTERFACE_KNOWN (decl) = 1;
    }
  else if (CLASSTYPE_INTERFACE_KNOWN (type))
    {
      TREE_PUBLIC (decl) = 1;
      DECL_EXTERNAL (decl) = ! CLASSTYPE_VTABLE_NEEDS_WRITING (type);
      DECL_INTERFACE_KNOWN (decl) = 1;
    }
  else
    {
      /* We can only wait to decide if we have real non-inline virtual
	 functions in our class, or if we come from a template.  */

      int found = CLASSTYPE_TEMPLATE_INSTANTIATION (type);

#ifndef MULTIPLE_SYMBOL_SPACES
      if (! found && ! final)
	{
	  tree method;
	  for (method = TYPE_METHODS (type); method != NULL_TREE;
	       method = TREE_CHAIN (method))
	    if (DECL_VINDEX (method) != NULL_TREE
		&& ! DECL_THIS_INLINE (method)
		&& ! DECL_ABSTRACT_VIRTUAL_P (method))
	      {
		found = 1;
		break;
	      }
	}
#endif

      if (final || ! found)
	{
	  comdat_linkage (decl);
	  DECL_EXTERNAL (decl) = 0;
	}
      else
	{
	  TREE_PUBLIC (decl) = 1;
	  DECL_EXTERNAL (decl) = 1;
	}
    }
}

static void
import_export_template (type)
     tree type;
{
  if (CLASSTYPE_IMPLICIT_INSTANTIATION (type)
      && ! flag_implicit_templates
      && CLASSTYPE_INTERFACE_UNKNOWN (type))
    {
      SET_CLASSTYPE_INTERFACE_KNOWN (type);
      CLASSTYPE_INTERFACE_ONLY (type) = 1;
      CLASSTYPE_VTABLE_NEEDS_WRITING (type) = 0;
    }
}
    
int
finish_prevtable_vardecl (prev, vars)
     tree prev, vars;
{
  tree ctype = DECL_CONTEXT (vars);
  import_export_template (ctype);

#ifndef MULTIPLE_SYMBOL_SPACES
  if (CLASSTYPE_INTERFACE_UNKNOWN (ctype) && TYPE_VIRTUAL_P (ctype)
      && ! CLASSTYPE_TEMPLATE_INSTANTIATION (ctype))
    {
      tree method;
      for (method = TYPE_METHODS (ctype); method != NULL_TREE;
	   method = TREE_CHAIN (method))
	{
	  if (DECL_VINDEX (method) != NULL_TREE
	      && !DECL_THIS_INLINE (method)
	      && !DECL_ABSTRACT_VIRTUAL_P (method))
	    {
	      SET_CLASSTYPE_INTERFACE_KNOWN (ctype);
	      CLASSTYPE_VTABLE_NEEDS_WRITING (ctype)
		= ! DECL_REALLY_EXTERN (method);
	      CLASSTYPE_INTERFACE_ONLY (ctype) = DECL_REALLY_EXTERN (method);
	      break;
	    }
	}
    }
#endif

  import_export_vtable (vars, ctype, 1);
  return 1;
}
    
static int
finish_vtable_vardecl (prev, vars)
     tree prev, vars;
{
  if (write_virtuals >= 0
      && ! DECL_EXTERNAL (vars)
      && ((TREE_PUBLIC (vars) && ! DECL_WEAK (vars) && ! DECL_ONE_ONLY (vars))
	  || TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (vars))
	  || (hack_decl_function_context (vars) && TREE_USED (vars)))
      && ! TREE_ASM_WRITTEN (vars))
    {
      /* Write it out.  */
      mark_vtable_entries (vars);
      if (TREE_TYPE (DECL_INITIAL (vars)) == 0)
	store_init_value (vars, DECL_INITIAL (vars));

      if (write_symbols == DWARF_DEBUG || write_symbols == DWARF2_DEBUG)
	{
	  /* Mark the VAR_DECL node representing the vtable itself as a
	     "gratuitous" one, thereby forcing dwarfout.c to ignore it.
	     It is rather important that such things be ignored because
	     any effort to actually generate DWARF for them will run
	     into trouble when/if we encounter code like:

		#pragma interface
		struct S { virtual void member (); };

	      because the artificial declaration of the vtable itself (as
	      manufactured by the g++ front end) will say that the vtable
	      is a static member of `S' but only *after* the debug output
	      for the definition of `S' has already been output.  This causes
	      grief because the DWARF entry for the definition of the vtable
	      will try to refer back to an earlier *declaration* of the
	      vtable as a static member of `S' and there won't be one.
	      We might be able to arrange to have the "vtable static member"
	      attached to the member list for `S' before the debug info for
	      `S' get written (which would solve the problem) but that would
	      require more intrusive changes to the g++ front end.  */

	  DECL_IGNORED_P (vars) = 1;
	}

      rest_of_decl_compilation (vars, NULL_PTR, 1, 1);
      return 1;
    }
  else if (! TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (vars)))
    /* We don't know what to do with this one yet.  */
    return 0;

  /* We know that PREV must be non-zero here.  */
  TREE_CHAIN (prev) = TREE_CHAIN (vars);
  return 0;
}

static int
prune_vtable_vardecl (prev, vars)
     tree prev, vars;
{
  /* We know that PREV must be non-zero here.  */
  TREE_CHAIN (prev) = TREE_CHAIN (vars);
  return 1;
}

int
walk_vtables (typedecl_fn, vardecl_fn)
     register void (*typedecl_fn) PROTO ((tree, tree));
     register int (*vardecl_fn) PROTO ((tree, tree));
{
  tree prev, vars;
  int flag = 0;

  for (prev = 0, vars = getdecls (); vars; vars = TREE_CHAIN (vars))
    {
      register tree type = TREE_TYPE (vars);

      if (TREE_CODE (vars) == VAR_DECL && DECL_VIRTUAL_P (vars))
	{
	  if (vardecl_fn)
	    flag |= (*vardecl_fn) (prev, vars);

	  if (prev && TREE_CHAIN (prev) != vars)
	    continue;
	}
      else if (TREE_CODE (vars) == TYPE_DECL
	       && type != error_mark_node
	       && TYPE_LANG_SPECIFIC (type)
	       && CLASSTYPE_VSIZE (type))
	{
	  if (typedecl_fn) (*typedecl_fn) (prev, vars);
	}

      prev = vars;
    }

  return flag;
}

static void
finish_sigtable_vardecl (prev, vars)
     tree prev, vars;
{
  /* We don't need to mark sigtable entries as addressable here as is done
     for vtables.  Since sigtables, unlike vtables, are always written out,
     that was already done in build_signature_table_constructor.  */

  rest_of_decl_compilation (vars, NULL_PTR, 1, 1);

  /* We know that PREV must be non-zero here.  */
  TREE_CHAIN (prev) = TREE_CHAIN (vars);
}

void
walk_sigtables (typedecl_fn, vardecl_fn)
     register void (*typedecl_fn) PROTO((tree, tree));
     register void (*vardecl_fn) PROTO((tree, tree));
{
  tree prev, vars;

  for (prev = 0, vars = getdecls (); vars; vars = TREE_CHAIN (vars))
    {
      register tree type = TREE_TYPE (vars);

      if (TREE_CODE (vars) == TYPE_DECL
	  && type != error_mark_node
	  && IS_SIGNATURE (type))
	{
	  if (typedecl_fn) (*typedecl_fn) (prev, vars);
	}
      else if (TREE_CODE (vars) == VAR_DECL
	       && TREE_TYPE (vars) != error_mark_node
	       && IS_SIGNATURE (TREE_TYPE (vars)))
	{
	  if (vardecl_fn) (*vardecl_fn) (prev, vars);
	}
      else
	prev = vars;
    }
}

/* Determines the proper settings of TREE_PUBLIC and DECL_EXTERNAL for an
   inline function or template instantiation at end-of-file.  */

void
import_export_decl (decl)
     tree decl;
{
  if (DECL_INTERFACE_KNOWN (decl))
    return;

  if (DECL_TEMPLATE_INSTANTIATION (decl))
    {
      DECL_NOT_REALLY_EXTERN (decl) = 1;
      if (DECL_IMPLICIT_INSTANTIATION (decl)
	  && (flag_implicit_templates || DECL_THIS_INLINE (decl)))
	{
	  if (TREE_CODE (decl) == FUNCTION_DECL)
	    comdat_linkage (decl);
	  else
	    DECL_COMDAT (decl) = 1;
	}
      else
	DECL_NOT_REALLY_EXTERN (decl) = 0;
    }
  else if (DECL_FUNCTION_MEMBER_P (decl))
    {
      tree ctype = DECL_CLASS_CONTEXT (decl);
      if (CLASSTYPE_INTERFACE_KNOWN (ctype)
	  && (! DECL_ARTIFICIAL (decl) || DECL_VINDEX (decl)))
	{
	  DECL_NOT_REALLY_EXTERN (decl)
	    = ! (CLASSTYPE_INTERFACE_ONLY (ctype)
		 || (DECL_THIS_INLINE (decl) && ! flag_implement_inlines));
	}
      else
	comdat_linkage (decl);
    }
  /* tinfo function */
  else if (DECL_ARTIFICIAL (decl) && DECL_MUTABLE_P (decl))
    {
      tree ctype = TREE_TYPE (DECL_NAME (decl));
      if (IS_AGGR_TYPE (ctype) && CLASSTYPE_INTERFACE_KNOWN (ctype)
	  && TYPE_VIRTUAL_P (ctype))
	{
	  DECL_NOT_REALLY_EXTERN (decl)
	    = ! (CLASSTYPE_INTERFACE_ONLY (ctype)
		 || (DECL_THIS_INLINE (decl) && ! flag_implement_inlines));
	}
      else if (TYPE_BUILT_IN (ctype) && ctype == TYPE_MAIN_VARIANT (ctype))
	DECL_NOT_REALLY_EXTERN (decl) = 0;
      else
	comdat_linkage (decl);
    } 
  else
    comdat_linkage (decl);

  DECL_INTERFACE_KNOWN (decl) = 1;
}

tree
build_cleanup (decl)
     tree decl;
{
  tree temp;
  tree type = TREE_TYPE (decl);

  if (TREE_CODE (type) == ARRAY_TYPE)
    temp = decl;
  else
    {
      mark_addressable (decl);
      temp = build1 (ADDR_EXPR, build_pointer_type (type), decl);
    }
  temp = build_delete (TREE_TYPE (temp), temp,
		       integer_two_node,
		       LOOKUP_NORMAL|LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR, 0);
  return temp;
}

extern int parse_time, varconst_time;
extern tree pending_templates;
extern tree maybe_templates;

extern struct obstack permanent_obstack;

static tree
get_sentry (base)
     tree base;
{
  tree sname = get_id_2 ("__sn", base);
  tree sentry = IDENTIFIER_GLOBAL_VALUE (sname);
  if (! sentry)
    {
      push_obstacks (&permanent_obstack, &permanent_obstack);
      sentry = build_decl (VAR_DECL, sname, integer_type_node);
      TREE_PUBLIC (sentry) = 1;
      DECL_ARTIFICIAL (sentry) = 1;
      TREE_STATIC (sentry) = 1;
      TREE_USED (sentry) = 1;
      DECL_COMMON (sentry) = 1;
      pushdecl_top_level (sentry);
      cp_finish_decl (sentry, NULL_TREE, NULL_TREE, 0, 0);
      pop_obstacks ();
    }
  return sentry;
}

/* This routine is called from the last rule in yyparse ().
   Its job is to create all the code needed to initialize and
   destroy the global aggregates.  We do the destruction
   first, since that way we only need to reverse the decls once.  */

void
finish_file ()
{
  extern int lineno;
  int start_time, this_time;

  tree fnname;
  tree vars;
  int needs_cleaning = 0, needs_messing_up = 0;

  at_eof = 1;

  /* Bad parse errors.  Just forget about it.  */
  if (! global_bindings_p () || current_class_type)
    return;

  start_time = get_run_time ();

  /* Otherwise, GDB can get confused, because in only knows
     about source for LINENO-1 lines.  */
  lineno -= 1;

  interface_unknown = 1;
  interface_only = 0;

  for (fnname = pending_templates; fnname; fnname = TREE_CHAIN (fnname))
    {
      tree decl = TREE_VALUE (fnname);
      if (TREE_CODE_CLASS (TREE_CODE (decl)) == 't')
	{
	  instantiate_class_template (decl);
	  if (CLASSTYPE_TEMPLATE_INSTANTIATION (decl))
	    for (vars = TYPE_METHODS (decl); vars; vars = TREE_CHAIN (vars))
	      if (! DECL_ARTIFICIAL (vars))
		instantiate_decl (vars);
	}
      else
	instantiate_decl (decl);
    }

  for (fnname = maybe_templates; fnname; fnname = TREE_CHAIN (fnname))
    {
      tree args, fn, decl = TREE_VALUE (fnname);

      if (DECL_INITIAL (decl))
	continue;

      fn = TREE_PURPOSE (fnname);
      args = get_bindings (fn, decl);
      fn = instantiate_template (fn, args);
      instantiate_decl (fn);
    }

  /* Push into C language context, because that's all
     we'll need here.  */
  push_lang_context (lang_name_c);

#if 1
  /* The reason for pushing garbage onto the global_binding_level is to
     ensure that we can slice out _DECLs which pertain to virtual function
     tables.  If the last thing pushed onto the global_binding_level was a
     virtual function table, then slicing it out would slice away all the
     decls (i.e., we lose the head of the chain).

     There are several ways of getting the same effect, from changing the
     way that iterators over the chain treat the elements that pertain to
     virtual function tables, moving the implementation of this code to
     decl.c (where we can manipulate global_binding_level directly),
     popping the garbage after pushing it and slicing away the vtable
     stuff, or just leaving it alone.  */

  /* Make last thing in global scope not be a virtual function table.  */
#if 0 /* not yet, should get fixed properly later */
  vars = make_type_decl (get_identifier (" @%$#@!"), integer_type_node);
#else
  vars = build_decl (TYPE_DECL, get_identifier (" @%$#@!"), integer_type_node);
#endif
  DECL_IGNORED_P (vars) = 1;
  SET_DECL_ARTIFICIAL (vars);
  pushdecl (vars);
#endif

  /* Walk to mark the inline functions we need, then output them so
     that we can pick up any other tdecls that those routines need.  */
  walk_vtables ((void (*) PROTO ((tree, tree))) 0,
		finish_prevtable_vardecl);

  for (vars = pending_statics; vars; vars = TREE_CHAIN (vars))
    {
      tree decl = TREE_VALUE (vars);

      if (DECL_TEMPLATE_INSTANTIATION (decl)
	  && ! DECL_IN_AGGR_P (decl))
	{
	  import_export_decl (decl);
	  DECL_EXTERNAL (decl) = ! DECL_NOT_REALLY_EXTERN (decl);
	}
    }

  for (vars = static_aggregates; vars; vars = TREE_CHAIN (vars))
    if (! TREE_ASM_WRITTEN (TREE_VALUE (vars)))
      rest_of_decl_compilation (TREE_VALUE (vars), 0, 1, 1);
  vars = static_aggregates;

  if (static_ctors || vars || register_exception_table_p ())
    needs_messing_up = 1;
  if (static_dtors)
    needs_cleaning = 1;

  /* See if we really need the hassle.  */
  while (vars && needs_cleaning == 0)
    {
      tree decl = TREE_VALUE (vars);
      tree type = TREE_TYPE (decl);
      if (TYPE_NEEDS_DESTRUCTOR (type) && ! TREE_STATIC (vars))
	{
	  needs_cleaning = 1;
	  break;
	}

      vars = TREE_CHAIN (vars);
    }

  if (needs_cleaning == 0)
    goto mess_up;

  fnname = get_file_function_name ('D');
  start_function (void_list_node,
		  make_call_declarator (fnname, void_list_node, NULL_TREE,
					NULL_TREE),
		  NULL_TREE, 0);
  fnname = DECL_ASSEMBLER_NAME (current_function_decl);
  store_parm_decls ();

  pushlevel (0);
  clear_last_expr ();
  push_momentary ();
  expand_start_bindings (0);

  /* These must be done in backward order to destroy,
     in which they happen to be!  */
  for (vars = static_aggregates; vars; vars = TREE_CHAIN (vars))
    {
      tree decl = TREE_VALUE (vars);
      tree type = TREE_TYPE (decl);
      tree temp = TREE_PURPOSE (vars);

      if (TYPE_NEEDS_DESTRUCTOR (type) && ! TREE_STATIC (vars)
	  && ! DECL_EXTERNAL (decl))
	{
	  int protect = (TREE_PUBLIC (decl) && (DECL_COMMON (decl)
						|| DECL_ONE_ONLY (decl)
						|| DECL_WEAK (decl)));

	  temp = build_cleanup (decl);

	  if (protect)
	    {
	      tree sentry = get_sentry (DECL_ASSEMBLER_NAME (decl));
	      sentry = build_unary_op (PREDECREMENT_EXPR, sentry, 0);
	      sentry = build_binary_op (EQ_EXPR, sentry, integer_zero_node, 1);
	      expand_start_cond (sentry, 0);
	    }

	  expand_expr_stmt (temp);

	  if (protect)
	    expand_end_cond ();
	}
    }

  for (; static_dtors; static_dtors = TREE_CHAIN (static_dtors))
    expand_expr_stmt (build_function_call (TREE_VALUE (static_dtors),
					   NULL_TREE));
      
  expand_end_bindings (getdecls (), 1, 0);
  poplevel (1, 0, 0);
  pop_momentary ();

  finish_function (lineno, 0, 0);

  assemble_destructor (IDENTIFIER_POINTER (fnname));

  /* if it needed cleaning, then it will need messing up: drop through  */

 mess_up:
  /* Must do this while we think we are at the top level.  */
  vars = nreverse (static_aggregates);
  if (needs_messing_up)
    {
      fnname = get_file_function_name ('I');
      start_function (void_list_node,
		      make_call_declarator (fnname, void_list_node, NULL_TREE,
					    NULL_TREE),
		      NULL_TREE, 0);
      fnname = DECL_ASSEMBLER_NAME (current_function_decl);
      store_parm_decls ();

      pushlevel (0);
      clear_last_expr ();
      push_momentary ();
      expand_start_bindings (0);

      if (register_exception_table_p ())
	register_exception_table ();

      while (vars)
	{
	  tree decl = TREE_VALUE (vars);
	  tree init = TREE_PURPOSE (vars);

	  /* If this was a static attribute within some function's scope,
	     then don't initialize it here.  Also, don't bother
	     with initializers that contain errors.  */
	  if (TREE_STATIC (vars)
	      || DECL_EXTERNAL (decl)
	      || (init && TREE_CODE (init) == TREE_LIST
		  && value_member (error_mark_node, init)))
	    goto next_mess;

	  if (TREE_CODE (decl) == VAR_DECL)
	    {
	      int protect = (TREE_PUBLIC (decl) && (DECL_COMMON (decl)
						    || DECL_ONE_ONLY (decl)
						    || DECL_WEAK (decl)));

	      /* Set these global variables so that GDB at least puts
		 us near the declaration which required the initialization.  */
	      input_filename = DECL_SOURCE_FILE (decl);
	      lineno = DECL_SOURCE_LINE (decl);
	      emit_note (input_filename, lineno);

	      /* 9.5p5: The initializer of a static member of a class has
		 the same access rights as a member function.  */
	      DECL_CLASS_CONTEXT (current_function_decl) = DECL_CONTEXT (decl);
	      DECL_STATIC_FUNCTION_P (current_function_decl) = 1;

	      if (protect)
		{
		  tree sentry = get_sentry (DECL_ASSEMBLER_NAME (decl));
		  sentry = build_unary_op (PREINCREMENT_EXPR, sentry, 0);
		  sentry = build_binary_op
		    (EQ_EXPR, sentry, integer_one_node, 1);
		  expand_start_cond (sentry, 0);
		}

	      expand_start_target_temps ();

	      if (IS_AGGR_TYPE (TREE_TYPE (decl))
		  || TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
		expand_aggr_init (decl, init, 0, 0);
	      else if (TREE_CODE (init) == TREE_VEC)
		{
		  expand_expr (expand_vec_init (decl, TREE_VEC_ELT (init, 0),
						TREE_VEC_ELT (init, 1),
						TREE_VEC_ELT (init, 2), 0),
			       const0_rtx, VOIDmode, EXPAND_NORMAL);
		}
	      else
		expand_assignment (decl, init, 0, 0);

	      /* Cleanup any temporaries needed for the initial value.  */
	      expand_end_target_temps ();

	      if (protect)
		expand_end_cond ();

	      DECL_CLASS_CONTEXT (current_function_decl) = NULL_TREE;
	      DECL_STATIC_FUNCTION_P (current_function_decl) = 0;
	    }
	  else if (decl == error_mark_node)
	    ;
	  else my_friendly_abort (22);

	next_mess:
	  vars = TREE_CHAIN (vars);
	}

      for (; static_ctors; static_ctors = TREE_CHAIN (static_ctors))
	expand_expr_stmt (build_function_call (TREE_VALUE (static_ctors),
					       NULL_TREE));
      
      expand_end_bindings (getdecls (), 1, 0);
      poplevel (1, 0, 0);
      pop_momentary ();

      finish_function (lineno, 0, 0);
      assemble_constructor (IDENTIFIER_POINTER (fnname));
    }

  expand_builtin_throw ();

  permanent_allocation (1);

  /* Done with C language context needs.  */
  pop_lang_context ();

  /* Now write out any static class variables (which may have since
     learned how to be initialized).  */
  while (pending_statics)
    {
      tree decl = TREE_VALUE (pending_statics);

      /* Output DWARF debug information.  */
#ifdef DWARF_DEBUGGING_INFO
      if (write_symbols == DWARF_DEBUG)
	dwarfout_file_scope_decl (decl, 1);
#endif
#ifdef DWARF2_DEBUGGING_INFO
      if (write_symbols == DWARF2_DEBUG)
	dwarf2out_decl (decl);
#endif

      DECL_DEFER_OUTPUT (decl) = 0;
      rest_of_decl_compilation
	(decl, IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)), 1, 1);

      pending_statics = TREE_CHAIN (pending_statics);
    }

  this_time = get_run_time ();
  parse_time -= this_time - start_time;
  varconst_time += this_time - start_time;

  start_time = get_run_time ();

  if (flag_handle_signatures)
    walk_sigtables ((void (*) PROTO ((tree, tree))) 0,
		    finish_sigtable_vardecl);

  for (fnname = saved_inlines; fnname; fnname = TREE_CHAIN (fnname))
    {
      tree decl = TREE_VALUE (fnname);
      import_export_decl (decl);
    }

  /* Now write out inline functions which had their addresses taken and
     which were not declared virtual and which were not declared `extern
     inline'.  */
  {
    int reconsider = 1;		/* More may be referenced; check again */

    while (reconsider)
      {
	tree *p = &saved_inlines;
	reconsider = 0;

	/* We need to do this each time so that newly completed template
           types don't wind up at the front of the list.  Sigh.  */
	vars = build_decl (TYPE_DECL, make_anon_name (), integer_type_node);
	DECL_IGNORED_P (vars) = 1;
	SET_DECL_ARTIFICIAL (vars);
	pushdecl (vars);

	reconsider |= walk_vtables ((void (*) PROTO((tree, tree))) 0, 
				    finish_vtable_vardecl);

	while (*p)
	  {
	    tree decl = TREE_VALUE (*p);

	    if (DECL_ARTIFICIAL (decl) && ! DECL_INITIAL (decl)
		&& TREE_USED (decl)
		&& (! DECL_REALLY_EXTERN (decl) || DECL_INLINE (decl)))
	      {
		if (DECL_MUTABLE_P (decl))
		  synthesize_tinfo_fn (decl);
		else
		  synthesize_method (decl);
		reconsider = 1;
	      }

	    /* Catch new template instantiations.  */
	    if (decl != TREE_VALUE (*p))
	      continue;

	    if (TREE_ASM_WRITTEN (decl)
		|| (DECL_SAVED_INSNS (decl) == 0 && ! DECL_ARTIFICIAL (decl)))
	      *p = TREE_CHAIN (*p);
	    else if (DECL_INITIAL (decl) == 0)
	      p = &TREE_CHAIN (*p);
	    else if ((TREE_PUBLIC (decl) && ! DECL_WEAK (decl)
		      && ! DECL_ONE_ONLY (decl))
		     || TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))
		     || flag_keep_inline_functions)
	      {
		if (DECL_NOT_REALLY_EXTERN (decl))
		  {
		    DECL_EXTERNAL (decl) = 0;
		    reconsider = 1;
		    /* We can't inline this function after it's been
                       emitted.  We want a variant of
                       output_inline_function that doesn't prevent
                       subsequent integration...  */
		    DECL_INLINE (decl) = 0;
		    output_inline_function (decl);
		    permanent_allocation (1);
		  }

		*p = TREE_CHAIN (*p);
	      }
	    else
	      p = &TREE_CHAIN (*p);
	  }
      }
  }

  /* Now delete from the chain of variables all virtual function tables.
     We output them all ourselves, because each will be treated specially.  */

  walk_vtables ((void (*) PROTO((tree, tree))) 0,
		prune_vtable_vardecl);

  if (write_virtuals == 2)
    {
      /* Now complain about an virtual function tables promised
	 but not delivered.  */
      while (pending_vtables)
	{
	  if (TREE_PURPOSE (pending_vtables) == NULL_TREE)
	    error ("virtual function table for `%s' not defined",
		   IDENTIFIER_POINTER (TREE_VALUE (pending_vtables)));
	  pending_vtables = TREE_CHAIN (pending_vtables);
	}
    }

  finish_repo ();

  this_time = get_run_time ();
  parse_time -= this_time - start_time;
  varconst_time += this_time - start_time;

  if (flag_detailed_statistics)
    {
      dump_tree_statistics ();
      dump_time_statistics ();
    }
}

/* This is something of the form 'A()()()()()+1' that has turned out to be an
   expr.  Since it was parsed like a type, we need to wade through and fix
   that.  Unfortunately, since operator() is left-associative, we can't use
   tail recursion.  In the above example, TYPE is `A', and DECL is
   `()()()()()'.

   Maybe this shouldn't be recursive, but how often will it actually be
   used?  (jason) */

tree
reparse_absdcl_as_expr (type, decl)
     tree type, decl;
{
  /* do build_functional_cast (type, NULL_TREE) at bottom */
  if (TREE_OPERAND (decl, 0) == NULL_TREE)
    return build_functional_cast (type, NULL_TREE);

  /* recurse */
  decl = reparse_decl_as_expr (type, TREE_OPERAND (decl, 0));

  decl = build_x_function_call (decl, NULL_TREE, current_class_ref);

  if (TREE_CODE (decl) == CALL_EXPR && TREE_TYPE (decl) != void_type_node)
    decl = require_complete_type (decl);

  return decl;
}

/* This is something of the form `int ((int)(int)(int)1)' that has turned
   out to be an expr.  Since it was parsed like a type, we need to wade
   through and fix that.  Since casts are right-associative, we are
   reversing the order, so we don't have to recurse.

   In the above example, DECL is the `(int)(int)(int)', and EXPR is the
   `1'.  */

tree
reparse_absdcl_as_casts (decl, expr)
     tree decl, expr;
{
  tree type;
  
  if (TREE_CODE (expr) == CONSTRUCTOR
      && TREE_TYPE (expr) == 0)
    {
      type = groktypename (TREE_VALUE (TREE_OPERAND (decl, 1)));
      decl = TREE_OPERAND (decl, 0);

      if (IS_SIGNATURE (type))
	{
	  error ("cast specifies signature type");
	  return error_mark_node;
	}

      expr = digest_init (type, expr, (tree *) 0);
      if (TREE_CODE (type) == ARRAY_TYPE && TYPE_SIZE (type) == 0)
	{
	  int failure = complete_array_type (type, expr, 1);
	  if (failure)
	    my_friendly_abort (78);
	}
    }

  while (decl)
    {
      type = groktypename (TREE_VALUE (TREE_OPERAND (decl, 1)));
      decl = TREE_OPERAND (decl, 0);
      expr = build_c_cast (type, expr);
    }

  if (warn_old_style_cast)
    warning ("use of old-style cast");

  return expr;
}

/* Given plain tree nodes for an expression, build up the full semantics.  */

tree
build_expr_from_tree (t)
     tree t;
{
  if (t == NULL_TREE || t == error_mark_node)
    return t;

  switch (TREE_CODE (t))
    {
    case IDENTIFIER_NODE:
      return do_identifier (t, 0);

    case LOOKUP_EXPR:
      if (LOOKUP_EXPR_GLOBAL (t))
	return do_scoped_id (TREE_OPERAND (t, 0), 0);
      else
	return do_identifier (TREE_OPERAND (t, 0), 0);

    case TEMPLATE_ID_EXPR:
      return (lookup_template_function
	      (build_expr_from_tree (TREE_OPERAND (t, 0)),
	       build_expr_from_tree (TREE_OPERAND (t, 1))));

    case INDIRECT_REF:
      return build_x_indirect_ref
	(build_expr_from_tree (TREE_OPERAND (t, 0)), "unary *");

    case CAST_EXPR:
      return build_functional_cast
	(TREE_TYPE (t), build_expr_from_tree (TREE_OPERAND (t, 0)));

    case REINTERPRET_CAST_EXPR:
      return build_reinterpret_cast
	(TREE_TYPE (t), build_expr_from_tree (TREE_OPERAND (t, 0)));

    case CONST_CAST_EXPR:
      return build_const_cast
	(TREE_TYPE (t), build_expr_from_tree (TREE_OPERAND (t, 0)));

    case DYNAMIC_CAST_EXPR:
      return build_dynamic_cast
	(TREE_TYPE (t), build_expr_from_tree (TREE_OPERAND (t, 0)));

    case STATIC_CAST_EXPR:
      return build_static_cast
	(TREE_TYPE (t), build_expr_from_tree (TREE_OPERAND (t, 0)));

    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case ABS_EXPR:
    case TRUTH_NOT_EXPR:
    case ADDR_EXPR:
    case CONVERT_EXPR:      /* Unary + */
      if (TREE_TYPE (t))
	return t;
      return build_x_unary_op (TREE_CODE (t),
			       build_expr_from_tree (TREE_OPERAND (t, 0)));

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case BIT_AND_EXPR:
    case BIT_ANDTC_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case RSHIFT_EXPR:
    case LSHIFT_EXPR:
    case RROTATE_EXPR:
    case LROTATE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
    case LE_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
    case MEMBER_REF:
      return build_x_binary_op
	(TREE_CODE (t), 
	 build_expr_from_tree (TREE_OPERAND (t, 0)),
	 build_expr_from_tree (TREE_OPERAND (t, 1)));

    case DOTSTAR_EXPR:
      return build_m_component_ref
	(build_expr_from_tree (TREE_OPERAND (t, 0)),
	 build_expr_from_tree (TREE_OPERAND (t, 1)));

    case SCOPE_REF:
      return build_offset_ref (TREE_OPERAND (t, 0), TREE_OPERAND (t, 1));

    case ARRAY_REF:
      if (TREE_OPERAND (t, 0) == NULL_TREE)
	/* new-type-id */
	return build_parse_node (ARRAY_REF, NULL_TREE,
				 build_expr_from_tree (TREE_OPERAND (t, 1)));
      return grok_array_decl (build_expr_from_tree (TREE_OPERAND (t, 0)),
			      build_expr_from_tree (TREE_OPERAND (t, 1)));

    case SIZEOF_EXPR:
      {
	tree r = build_expr_from_tree (TREE_OPERAND (t, 0));
	if (TREE_CODE_CLASS (TREE_CODE (r)) != 't')
	  r = TREE_TYPE (r);
	return c_sizeof (r);
      }

    case MODOP_EXPR:
      return build_x_modify_expr
	(build_expr_from_tree (TREE_OPERAND (t, 0)),
	 TREE_CODE (TREE_OPERAND (t, 1)),
	 build_expr_from_tree (TREE_OPERAND (t, 2)));

    case ARROW_EXPR:
      return build_x_arrow
	(build_expr_from_tree (TREE_OPERAND (t, 0)));

    case NEW_EXPR:
      return build_new
	(build_expr_from_tree (TREE_OPERAND (t, 0)),
	 build_expr_from_tree (TREE_OPERAND (t, 1)),
	 build_expr_from_tree (TREE_OPERAND (t, 2)),
	 NEW_EXPR_USE_GLOBAL (t));

    case DELETE_EXPR:
      return delete_sanity
	(build_expr_from_tree (TREE_OPERAND (t, 0)),
	 build_expr_from_tree (TREE_OPERAND (t, 1)),
	 DELETE_EXPR_USE_VEC (t), DELETE_EXPR_USE_GLOBAL (t));

    case COMPOUND_EXPR:
      if (TREE_OPERAND (t, 1) == NULL_TREE)
	return build_x_compound_expr
	  (build_expr_from_tree (TREE_OPERAND (t, 0)));
      else
	my_friendly_abort (42);

    case METHOD_CALL_EXPR:
      if (TREE_CODE (TREE_OPERAND (t, 0)) == SCOPE_REF)
	{
	  tree ref = TREE_OPERAND (t, 0);
	  return build_scoped_method_call
	    (build_expr_from_tree (TREE_OPERAND (t, 1)),
	     build_expr_from_tree (TREE_OPERAND (ref, 0)),
	     TREE_OPERAND (ref, 1),
	     build_expr_from_tree (TREE_OPERAND (t, 2)));
	}
      return build_method_call
	(build_expr_from_tree (TREE_OPERAND (t, 1)),
	 TREE_OPERAND (t, 0),
	 build_expr_from_tree (TREE_OPERAND (t, 2)),
	 NULL_TREE, LOOKUP_NORMAL);

    case CALL_EXPR:
      if (TREE_CODE (TREE_OPERAND (t, 0)) == SCOPE_REF)
	{
	  tree ref = TREE_OPERAND (t, 0);
	  return build_member_call
	    (build_expr_from_tree (TREE_OPERAND (ref, 0)),
	     TREE_OPERAND (ref, 1),
	     build_expr_from_tree (TREE_OPERAND (t, 1)));
	}
      else
	{
	  tree name = TREE_OPERAND (t, 0);
	  if (TREE_CODE (name) == TEMPLATE_ID_EXPR
	      || ! really_overloaded_fn (name))
	    name = build_expr_from_tree (name);
	  return build_x_function_call
	    (name, build_expr_from_tree (TREE_OPERAND (t, 1)),
	     current_class_ref);
	}

    case COND_EXPR:
      return build_x_conditional_expr
	(build_expr_from_tree (TREE_OPERAND (t, 0)),
	 build_expr_from_tree (TREE_OPERAND (t, 1)),
	 build_expr_from_tree (TREE_OPERAND (t, 2)));

    case TREE_LIST:
      {
	tree purpose, value, chain;

	if (t == void_list_node)
	  return t;

	purpose = TREE_PURPOSE (t);
	if (purpose)
	  purpose = build_expr_from_tree (purpose);
	value = TREE_VALUE (t);
	if (value)
	  value = build_expr_from_tree (value);
	chain = TREE_CHAIN (t);
	if (chain && chain != void_type_node)
	  chain = build_expr_from_tree (chain);
	return expr_tree_cons (purpose, value, chain);
      }

    case COMPONENT_REF:
      return build_x_component_ref
	(build_expr_from_tree (TREE_OPERAND (t, 0)),
	 TREE_OPERAND (t, 1), NULL_TREE, 1);

    case THROW_EXPR:
      return build_throw (build_expr_from_tree (TREE_OPERAND (t, 0)));

    case CONSTRUCTOR:
      {
	tree r = build_nt (CONSTRUCTOR, NULL_TREE,
			   build_expr_from_tree (CONSTRUCTOR_ELTS (t)));

	if (TREE_TYPE (t))
	  return digest_init (TREE_TYPE (t), r, 0);
	return r;
      }

    case TYPEID_EXPR:
      if (TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (t, 0))) == 't')
	return get_typeid (TREE_OPERAND (t, 0));
      return build_x_typeid (build_expr_from_tree (TREE_OPERAND (t, 0)));

    case VAR_DECL:
      return convert_from_reference (t);

    default:
      return t;
    }
}

/* This is something of the form `int (*a)++' that has turned out to be an
   expr.  It was only converted into parse nodes, so we need to go through
   and build up the semantics.  Most of the work is done by
   build_expr_from_tree, above.

   In the above example, TYPE is `int' and DECL is `*a'.  */

tree
reparse_decl_as_expr (type, decl)
     tree type, decl;
{
  decl = build_expr_from_tree (decl);
  if (type)
    return build_functional_cast (type, build_expr_list (NULL_TREE, decl));
  else
    return decl;
}

/* This is something of the form `int (*a)' that has turned out to be a
   decl.  It was only converted into parse nodes, so we need to do the
   checking that make_{pointer,reference}_declarator do.  */

tree
finish_decl_parsing (decl)
     tree decl;
{
  extern int current_class_depth;
  
  switch (TREE_CODE (decl))
    {
    case IDENTIFIER_NODE:
      return decl;
    case INDIRECT_REF:
      return make_pointer_declarator
	(NULL_TREE, finish_decl_parsing (TREE_OPERAND (decl, 0)));
    case ADDR_EXPR:
      return make_reference_declarator
	(NULL_TREE, finish_decl_parsing (TREE_OPERAND (decl, 0)));
    case BIT_NOT_EXPR:
      TREE_OPERAND (decl, 0) = finish_decl_parsing (TREE_OPERAND (decl, 0));
      return decl;
    case SCOPE_REF:
      push_nested_class (TREE_TYPE (TREE_OPERAND (decl, 0)), 3);
      TREE_COMPLEXITY (decl) = current_class_depth;
      return decl;
    case ARRAY_REF:
      TREE_OPERAND (decl, 0) = finish_decl_parsing (TREE_OPERAND (decl, 0));
      return decl;
    default:
      my_friendly_abort (5);
      return NULL_TREE;
    }
}

tree
check_cp_case_value (value)
     tree value;
{
  if (value == NULL_TREE)
    return value;

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  STRIP_TYPE_NOPS (value);

  if (TREE_READONLY_DECL_P (value))
    {
      value = decl_constant_value (value);
      STRIP_TYPE_NOPS (value);
    }
  value = fold (value);

  if (TREE_CODE (value) != INTEGER_CST
      && value != error_mark_node)
    {
      cp_error ("case label `%E' does not reduce to an integer constant",
		value);
      value = error_mark_node;
    }
  else
    /* Promote char or short to int.  */
    value = default_conversion (value);

  constant_expression_warning (value);

  return value;
}

tree current_namespace;

/* Get the inner part of a namespace id.  It doesn't have any prefix, nor
   postfix.  Returns 0 if in global namespace.  */

tree
get_namespace_id ()
{
  tree x = current_namespace;
  if (x)
    x = TREE_PURPOSE (x);
  return x;
}

/* Build up a DECL_ASSEMBLER_NAME for NAME in the current namespace.  */

tree
current_namespace_id (name)
     tree name;
{
  tree old_id = get_namespace_id ();
  char *buf;

  /* Global names retain old encoding.  */
  if (! old_id)
    return name;

  buf = (char *) alloca (8 + IDENTIFIER_LENGTH (old_id)
			 + IDENTIFIER_LENGTH (name));
  sprintf (buf, "__ns_%s_%s", IDENTIFIER_POINTER (old_id),
	   IDENTIFIER_POINTER (name));
  return get_identifier (buf);
}

void
do_namespace_alias (alias, namespace)
     tree alias, namespace;
{
  sorry ("namespace alias");
}

void
do_toplevel_using_decl (decl)
     tree decl;
{
#if 1
  if (TREE_CODE (decl) == SCOPE_REF
      && TREE_OPERAND (decl, 0) == std_node)
    return;
  sorry ("using-declaration");
#else
  if (decl == NULL_TREE || decl == error_mark_node)
    return;

  if (TREE_CODE (decl) == SCOPE_REF)
    decl = resolve_scope_to_name (NULL_TREE, decl);

  /* Is this the right way to do an id list? */
  if (TREE_CODE (decl) != TREE_LIST)
    {
      pushdecl (decl);
    }
  else
    while (decl)
      {
	pushdecl (TREE_VALUE (decl));
	decl = TREE_CHAIN (decl);
      }
#endif
}

tree
do_class_using_decl (decl)
     tree decl;
{
  tree name, value;

  if (TREE_CODE (decl) != SCOPE_REF)
    {
      cp_error ("using-declaration for non-member at class scope");
      return NULL_TREE;
    }
  name = TREE_OPERAND (decl, 1);
  if (TREE_CODE (name) == BIT_NOT_EXPR)
    {
      cp_error ("using-declaration for destructor");
      return NULL_TREE;
    }

  value = build_lang_field_decl (USING_DECL, name, void_type_node);
  DECL_INITIAL (value) = TREE_OPERAND (decl, 0);
  return value;
}

void
do_using_directive (namespace)
     tree namespace;
{
  if (namespace == std_node)
    return;
  sorry ("using directive");
}

void
check_default_args (x)
     tree x;
{
  tree arg = TYPE_ARG_TYPES (TREE_TYPE (x));
  int saw_def = 0, i = 0 - (TREE_CODE (TREE_TYPE (x)) == METHOD_TYPE);
  for (; arg && arg != void_list_node; arg = TREE_CHAIN (arg), ++i)
    {
      if (TREE_PURPOSE (arg))
	saw_def = 1;
      else if (saw_def)
	{
	  cp_error ("default argument missing for parameter %P of `%#D'",
		    i, x);
	  break;
	}
    }
}

void
mark_used (decl)
     tree decl;
{
  TREE_USED (decl) = 1;
  if (processing_template_decl)
    return;
  assemble_external (decl);
  /* Is it a synthesized method that needs to be synthesized?  */
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_CLASS_CONTEXT (decl)
      && DECL_ARTIFICIAL (decl) && ! DECL_INITIAL (decl)
      /* Kludge: don't synthesize for default args.  */
      && current_function_decl)
    synthesize_method (decl);
  if (DECL_LANG_SPECIFIC (decl) && DECL_TEMPLATE_INFO (decl))
    instantiate_decl (decl);
}

/* Helper function for named_class_head_sans_basetype nonterminal.  */

tree
handle_class_head (aggr, scope, id)
     tree aggr, scope, id;
{
  if (TREE_CODE (id) == TYPE_DECL)
    return id;

  if (scope)
    cp_error ("`%T' does not have a nested type named `%D'", scope, id);
  else
    cp_error ("no file-scope type named `%D'", id);

  id = xref_tag
    (aggr, make_anon_name (), NULL_TREE, 1);
  return TYPE_MAIN_DECL (id);
}
