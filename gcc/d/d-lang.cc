/* d-lang.cc -- Language-dependent hooks for D.
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

#include "dmd/aggregate.h"
#include "dmd/cond.h"
#include "dmd/declaration.h"
#include "dmd/doc.h"
#include "dmd/dsymbol.h"
#include "dmd/errors.h"
#include "dmd/expression.h"
#include "dmd/hdrgen.h"
#include "dmd/id.h"
#include "dmd/identifier.h"
#include "dmd/json.h"
#include "dmd/mangle.h"
#include "dmd/module.h"
#include "dmd/mtype.h"
#include "dmd/target.h"
#include "dmd/template.h"

#include "opts.h"
#include "alias.h"
#include "tree.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "toplev.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "target.h"
#include "function.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "varasm.h"
#include "output.h"
#include "print-tree.h"
#include "debug.h"
#include "input.h"

#include "d-tree.h"
#include "d-frontend.h"


/* Array of D frontend type/decl nodes.  */
tree d_global_trees[DTI_MAX];

/* True if compilation is currently inside the D frontend semantic passes.  */
bool doing_semantic_analysis_p = false;

/* Options handled by the compiler that are separate from the frontend.  */
struct d_option_data
{
  const char *fonly;		    /* -fonly=<arg>  */
  const char *multilib;		    /* -imultilib <dir>  */
  const char *prefix;		    /* -iprefix <dir>  */

  bool deps;			    /* -M  */
  bool deps_skip_system;	    /* -MM  */
  const char *deps_filename;	    /* -M[M]D  */
  const char *deps_filename_user;   /* -MF <arg>  */
  vec <const char *> deps_target;   /* -M[QT] <arg> */
  bool deps_phony;		    /* -MP  */

  bool stdinc;			    /* -nostdinc  */
}
d_option;

/* List of modules being compiled.  */
static Modules builtin_modules;

/* The current and global binding level in effect.  */
struct binding_level *current_binding_level;
struct binding_level *global_binding_level;

/* The context to be used for global declarations.  */
static GTY(()) tree global_context;

/* Array of all global declarations to pass back to the middle-end.  */
static GTY(()) vec <tree, va_gc> *global_declarations;

/* Support for GCC-style command-line make dependency generation.
   Adds TARGET to the make dependencies target buffer.
   QUOTED is true if the string should be quoted.  */

static void
deps_add_target (const char *target, bool quoted)
{
  obstack buffer;
  gcc_obstack_init (&buffer);

  if (!quoted)
    {
      obstack_grow0 (&buffer, target, strlen (target));
      d_option.deps_target.safe_push ((const char *) obstack_finish (&buffer));
      return;
    }

  /* Quote characters in target which are significant to Make.  */
  unsigned slashes = 0;

  for (const char *p = target; *p != '\0'; p++)
    {
      switch (*p)
	{
	case '\\':
	  slashes++;
	  break;

	case ' ':
	case '\t':
	  while (slashes--)
	    obstack_1grow (&buffer, '\\');
	  obstack_1grow (&buffer, '\\');
	  goto Ldef;

	case '$':
	  obstack_1grow (&buffer, '$');
	  goto Ldef;

	case '#':
	case ':':
	  obstack_1grow (&buffer, '\\');
	  goto Ldef;

	default:
	Ldef:
	  slashes = 0;
	  break;
	}

      obstack_1grow (&buffer, *p);
    }

  obstack_1grow (&buffer, '\0');
  d_option.deps_target.safe_push ((const char *) obstack_finish (&buffer));
}

/* Write STR, with a leading space to BUFFER, updating COLUMN as appropriate.
   COLMAX is the number of columns to word-wrap at (0 means don't wrap).  */

static void
deps_write_string (const char *str, obstack *buffer, unsigned &column,
		   unsigned colmax = 72)
{
  unsigned size = strlen (str);

  if (column != 0)
    {
      if (colmax && column + size > colmax)
	{
	  obstack_grow (buffer, " \\\n ", 4);
	  column = 1;
	}
      else
	{
	  obstack_1grow (buffer, ' ');
	  column++;
	}
    }

  column += size;
  obstack_grow (buffer, str, size);
}

/* Write out all dependencies of a given MODULE to the specified BUFFER.  */

static void
deps_write (Module *module, obstack *buffer)
{
  hash_set <const char *> seen_modules;
  vec <const char *> dependencies = vNULL;

  Modules modlist;
  modlist.push (module);

  vec <const char *> phonylist = vNULL;
  unsigned column = 0;

  /* Write out make target module name.  */
  if (d_option.deps_target.length ())
    {
      for (unsigned i = 0; i < d_option.deps_target.length (); i++)
	deps_write_string (d_option.deps_target[i], buffer, column);
    }
  else
    deps_write_string (module->objfile.toChars (), buffer, column);

  obstack_1grow (buffer, ':');
  column++;

  /* Search all modules for file dependencies.  */
  while (modlist.length > 0)
    {
      Module *depmod = modlist.pop ();

      const char *modstr = depmod->srcfile.toChars ();

      /* Skip modules that have already been looked at.  */
      if (seen_modules.add (modstr))
	continue;

      dependencies.safe_push (modstr);

      /* Add to list of phony targets if is not being compile.  */
      if (d_option.deps_phony && !depmod->isRoot ())
	phonylist.safe_push (modstr);

      /* Add imported files to dependency list.  */
      for (size_t i = 0; i < depmod->contentImportedFiles.length; i++)
	{
	  const char *impstr = depmod->contentImportedFiles[i];
	  dependencies.safe_push (impstr);
	  phonylist.safe_push (impstr);
	}

      /* Search all imports of the module.  */
      for (size_t i = 0; i < depmod->aimports.length; i++)
	{
	  Module *m = depmod->aimports[i];

	  /* Ignore compiler-generated modules.  */
	  if (m->ident == Identifier::idPool ("__main") && m->parent == NULL)
	    continue;

	  /* Don't search system installed modules, this includes
	     object, core.*, std.*, and gcc.* packages.  */
	  if (d_option.deps_skip_system)
	    {
	      if (m->ident == Identifier::idPool ("object")
		  && m->parent == NULL)
		continue;

	      if (m->md && m->md->packages.length)
		{
		  Identifier *package = m->md->packages.ptr[0];

		  if (package == Identifier::idPool ("core")
		      || package == Identifier::idPool ("std")
		      || package == Identifier::idPool ("gcc"))
		    continue;
		}
	    }

	  modlist.push (m);
	}
    }

  /* Write out all make dependencies.  */
  for (size_t i = 0; i < dependencies.length (); i++)
    deps_write_string (dependencies[i], buffer, column);

  obstack_1grow (buffer, '\n');

  /* Write out all phony targets.  */
  for (size_t i = 0; i < phonylist.length (); i++)
    {
      const char *str = phonylist[i];
      obstack_1grow (buffer, '\n');
      obstack_grow (buffer, str, strlen (str));
      obstack_grow (buffer, ":\n", 2);
    }

  obstack_1grow (buffer, '\0');
}

/* Implements the lang_hooks.init_options routine for language D.
   This initializes the global state for the D frontend before calling
   the option handlers.  */

static void
d_init_options (unsigned int, cl_decoded_option *decoded_options)
{
  /* Initialize the D runtime.  */
  rt_init ();
  gc_disable ();

  /* Set default values.  */
  global._init ();

  global.compileEnv.vendor = lang_hooks.name;
  global.params.argv0 = xstrdup (decoded_options[0].arg);

  /* Default extern(C++) mangling to C++17.  */
  global.params.cplusplus = CppStdRevisionCpp17;

  /* Warnings and deprecations are disabled by default.  */
  global.params.useDeprecated = DIAGNOSTICinform;
  global.params.useWarnings = DIAGNOSTICoff;
  global.params.v.errorLimit = flag_max_errors;
  global.params.v.messageStyle = MessageStyle::gnu;

  /* Extra GDC-specific options.  */
  d_option.fonly = NULL;
  d_option.multilib = NULL;
  d_option.prefix = NULL;
  d_option.deps = false;
  d_option.deps_skip_system = false;
  d_option.deps_filename = NULL;
  d_option.deps_filename_user = NULL;
  d_option.deps_target = vNULL;
  d_option.deps_phony = false;
  d_option.stdinc = true;
}

/* Implements the lang_hooks.init_options_struct routine for language D.
   Initializes the options structure OPTS.  */

static void
d_init_options_struct (gcc_options *opts)
{
  /* GCC options.  */
  opts->x_flag_exceptions = 1;

  /* Unlike C, there is no global `errno' variable.  */
  opts->x_flag_errno_math = 0;
  opts->frontend_set_flag_errno_math = true;

  /* D says that signed overflow is precisely defined.  */
  opts->x_flag_wrapv = 1;
}

/* Implements the lang_hooks.lang_mask routine for language D.
   Returns language mask for option parsing.  */

static unsigned int
d_option_lang_mask (void)
{
  return CL_D;
}

/* Implements input charset and BOM skipping configuration for
   diagnostics.  */
static const char *d_input_charset_callback (const char * /*filename*/)
{
  /* TODO: The input charset is automatically determined by code in
     dmd/dmodule.c based on the contents of the file.  If this detection
     logic were factored out and could be reused here, then we would be able
     to return UTF-16 or UTF-32 as needed here.  For now, we return always
     NULL, which means no conversion is necessary, i.e. the input is assumed
     to be UTF-8 when diagnostics read this file.  */
  return nullptr;
}

/* Implements the lang_hooks.init routine for language D.  */

static bool
d_init (void)
{
  Type::_init ();
  Id::initialize ();
  Module::_init ();
  Expression::_init ();
  Objc::_init ();

  /* Diagnostics input init, to enable BOM skipping and
     input charset conversion.  */
  diagnostic_initialize_input_context (global_dc,
				       d_input_charset_callback, true);

  /* Back-end init.  */
  global_binding_level = ggc_cleared_alloc <binding_level> ();
  current_binding_level = global_binding_level;

  /* This allows the code in d-builtins.cc to not have to worry about
     converting (C signed char *) to (D char *) for string arguments of
     built-in functions.  The parameter (signed_char = false) specifies
     whether char is signed.  */
  build_common_tree_nodes (false);

  d_init_builtins ();

  if (flag_exceptions)
    using_eh_for_cleanups ();

  if (!supports_one_only ())
    flag_weak_templates = 0;

  /* This is the C main, not the D main.  */
  main_identifier_node = get_identifier ("main");

  target._init (global.params);
  d_init_versions ();

  /* Insert all library-configured identifiers and import paths.  */
  add_import_paths (d_option.prefix, d_option.multilib, d_option.stdinc);

  return 1;
}

/* Implements the lang_hooks.init_ts routine for language D.  */

static void
d_init_ts (void)
{
  MARK_TS_TYPED (FLOAT_MOD_EXPR);
  MARK_TS_TYPED (UNSIGNED_RSHIFT_EXPR);
}

/* Implements the lang_hooks.handle_option routine for language D.
   Handles D specific options.  Return false if we didn't do anything.  */

static bool
d_handle_option (size_t scode, const char *arg, HOST_WIDE_INT value,
		 int kind ATTRIBUTE_UNUSED,
		 location_t loc ATTRIBUTE_UNUSED,
		 const cl_option_handlers *handlers ATTRIBUTE_UNUSED)
{
  opt_code code = (opt_code) scode;
  bool result = true;

  switch (code)
    {
    case OPT_fall_instantiations:
      global.params.allInst = value;
      break;

    case OPT_fassert:
      global.params.useAssert = value ? CHECKENABLEon : CHECKENABLEoff;
      break;

    case OPT_fbounds_check:
      global.params.useArrayBounds = value ? CHECKENABLEon : CHECKENABLEoff;
      break;

    case OPT_fbounds_check_:
      global.params.useArrayBounds = (value == 2) ? CHECKENABLEon
	: (value == 1) ? CHECKENABLEsafeonly : CHECKENABLEoff;
      break;

    case OPT_fcheckaction_:
      global.params.checkAction = (value == 0) ? CHECKACTION_D
	: (value == 1) ? CHECKACTION_halt : CHECKACTION_context;
      break;

    case OPT_fdebug:
      global.params.debugEnabled = value ? true : false;
      break;

    case OPT_fdebug_:
      if (Identifier::isValidIdentifier (CONST_CAST (char *, arg)))
	{
	  DebugCondition::addGlobalIdent (arg);
	  break;
	}

      error ("bad argument for %<-fdebug=%>: %qs", arg);
      break;

    case OPT_fdoc:
      global.params.ddoc.doOutput = value;
      break;

    case OPT_fdoc_dir_:
      global.params.ddoc.doOutput = true;
      global.params.ddoc.dir = arg;
      break;

    case OPT_fdoc_file_:
      global.params.ddoc.doOutput = true;
      global.params.ddoc.name = arg;
      break;

    case OPT_fdoc_inc_:
      global.params.ddoc.files.push (arg);
      break;

    case OPT_fdruntime:
      global.params.betterC = !value;
      break;

    case OPT_fdump_c___spec_:
      global.params.cxxhdr.doOutput = true;
      global.params.cxxhdr.name = arg;
      break;

    case OPT_fdump_c___spec_verbose:
      global.params.cxxhdr.fullOutput = true;
      break;

    case OPT_fdump_d_original:
      global.params.vcg_ast = value;
      break;

    case OPT_fexceptions:
      global.params.useExceptions = value;
      break;

    case OPT_fextern_std_:
      switch (value)
	{
	case CppStdRevisionCpp98:
	case CppStdRevisionCpp11:
	case CppStdRevisionCpp14:
	case CppStdRevisionCpp17:
	case CppStdRevisionCpp20:
	case CppStdRevisionCpp23:
	  global.params.cplusplus = (CppStdRevision) value;
	  break;

	default:
	  error ("bad argument for %<-fextern-std%>: %qs", arg);
	}
      break;

    case OPT_fignore_unknown_pragmas:
      global.params.ignoreUnsupportedPragmas = value;
      break;

    case OPT_finclude_imports:
      includeImports = true;
      break;

    case OPT_finvariants:
      global.params.useInvariants = value ? CHECKENABLEon : CHECKENABLEoff;
      break;

    case OPT_fmain:
      global.params.addMain = value;
      break;

    case OPT_fmodule_file_:
      global.params.modFileAliasStrings.push (arg);
      if (!strchr (arg, '='))
	error ("bad argument for %<-fmodule-file=%>: %qs", arg);
      break;

    case OPT_fmoduleinfo:
      global.params.useModuleInfo = value;
      break;

    case OPT_fonly_:
      d_option.fonly = arg;
      break;

    case OPT_fpostconditions:
      global.params.useOut = value ? CHECKENABLEon : CHECKENABLEoff;
      break;

    case OPT_fpreconditions:
      global.params.useIn = value ? CHECKENABLEon : CHECKENABLEoff;
      break;

    case OPT_fpreview_all:
      global.params.ehnogc = value;
      global.params.useDIP1000 = FeatureState::enabled;
      global.params.useDIP1021 = value;
      global.params.bitfields = value;
      global.params.dtorFields = FeatureState::enabled;
      global.params.fieldwise = FeatureState::enabled;
      global.params.fixAliasThis = value;
      global.params.previewIn = value;
      global.params.fix16997 = value;
      global.params.noSharedAccess = FeatureState::enabled;
      global.params.safer = FeatureState::enabled;
      global.params.rvalueRefParam = FeatureState::enabled;
      global.params.inclusiveInContracts = value;
      global.params.systemVariables = FeatureState::enabled;
      global.params.fixImmutableConv = value;
      break;

    case OPT_fpreview_bitfields:
      global.params.bitfields = value;
      break;

    case OPT_fpreview_dip1000:
      global.params.useDIP1000 = FeatureState::enabled;
      break;

    case OPT_fpreview_dip1008:
      global.params.ehnogc = value;
      break;

    case OPT_fpreview_dip1021:
      global.params.useDIP1021 = value;
      break;

    case OPT_fpreview_dtorfields:
      global.params.dtorFields = FeatureState::enabled;
      break;

    case OPT_fpreview_fieldwise:
      global.params.fieldwise = FeatureState::enabled;
      break;

    case OPT_fpreview_fixaliasthis:
      global.params.fixAliasThis = value;
      break;

    case OPT_fpreview_fiximmutableconv:
      global.params.fixImmutableConv = value;
      break;

    case OPT_fpreview_in:
      global.params.previewIn = value;
      break;

    case OPT_fpreview_inclusiveincontracts:
      global.params.inclusiveInContracts = value;
      break;

    case OPT_fpreview_nosharedaccess:
      global.params.noSharedAccess = FeatureState::enabled;
      break;

    case OPT_fpreview_safer:
      global.params.safer = FeatureState::enabled;
      break;

    case OPT_fpreview_rvaluerefparam:
      global.params.rvalueRefParam = FeatureState::enabled;
      break;

    case OPT_fpreview_systemvariables:
      global.params.systemVariables = FeatureState::enabled;
      break;

    case OPT_frelease:
      global.params.release = value;
      break;

    case OPT_frevert_all:
      global.params.useDIP1000 = FeatureState::disabled;
      global.params.dtorFields = FeatureState::disabled;
      global.params.fix16997 = !value;
      break;

    case OPT_frevert_dip1000:
      global.params.useDIP1000 = FeatureState::disabled;
      break;

    case OPT_frevert_dtorfields:
      global.params.dtorFields = FeatureState::disabled;
      break;

    case OPT_frevert_intpromote:
      global.params.fix16997 = !value;
      break;

    case OPT_frtti:
      global.params.useTypeInfo = value;
      break;

    case OPT_fsave_mixins_:
      global.params.mixinOut.doOutput = true;
      global.params.mixinOut.name = arg;
      global.params.mixinOut.buffer = d_gc_malloc<OutBuffer> ();
      break;

    case OPT_fswitch_errors:
      global.params.useSwitchError = value ? CHECKENABLEon : CHECKENABLEoff;
      break;

    case OPT_ftransition_all:
      global.params.v.field = value;
      global.params.v.gc = value;
      global.params.v.vin = value;
      global.params.v.tls = value;
      break;

    case OPT_ftransition_field:
      global.params.v.field = value;
      break;

    case OPT_ftransition_in:
      global.params.v.vin = value;
      break;

    case OPT_ftransition_nogc:
      global.params.v.gc = value;
      break;

    case OPT_ftransition_templates:
      global.params.v.templates = value;
      break;

    case OPT_ftransition_tls:
      global.params.v.tls = value;
      break;

    case OPT_funittest:
      global.params.useUnitTests = value;
      break;

    case OPT_fversion_:
      if (Identifier::isValidIdentifier (CONST_CAST (char *, arg)))
	{
	  VersionCondition::addGlobalIdent (arg);
	  break;
	}

      error ("bad argument for %<-fversion=%>: %qs", arg);
      break;

    case OPT_H:
      global.params.dihdr.doOutput = true;
      break;

    case OPT_Hd:
      global.params.dihdr.doOutput = true;
      global.params.dihdr.dir = arg;
      break;

    case OPT_Hf:
      global.params.dihdr.doOutput = true;
      global.params.dihdr.name = arg;
      break;

    case OPT_imultilib:
      d_option.multilib = arg;
      break;

    case OPT_iprefix:
      d_option.prefix = arg;
      break;

    case OPT_I:
      global.params.imppath.push (arg);
      break;

    case OPT_J:
      global.params.fileImppath.push (arg);
      break;

    case OPT_MM:
      d_option.deps_skip_system = true;
      /* Fall through.  */

    case OPT_M:
      d_option.deps = true;
      break;

    case OPT_MMD:
      d_option.deps_skip_system = true;
      /* Fall through.  */

    case OPT_MD:
      d_option.deps = true;
      d_option.deps_filename = arg;
      break;

    case OPT_MF:
      /* If specified multiple times, last one wins.  */
      d_option.deps_filename_user = arg;
      break;

    case OPT_MP:
      d_option.deps_phony = true;
      break;

    case OPT_MQ:
      deps_add_target (arg, true);
      break;

    case OPT_MT:
      deps_add_target (arg, false);
      break;

    case OPT_nostdinc:
      d_option.stdinc = false;
      break;

    case OPT_v:
      global.params.v.verbose = value;
      break;

    case OPT_Wall:
      if (value)
	global.params.useWarnings = DIAGNOSTICinform;
      break;

    case OPT_Wdeprecated:
      global.params.useDeprecated = value ? DIAGNOSTICinform : DIAGNOSTICoff;
      break;

    case OPT_Werror:
      if (value)
	global.params.useWarnings = DIAGNOSTICerror;
      break;

    case OPT_Wspeculative:
      if (value)
	global.params.v.showGaggedErrors = 1;
      break;

    case OPT_Xf:
      global.params.json.name = arg;
      /* Fall through.  */

    case OPT_X:
      global.params.json.doOutput = true;
      break;

    default:
      break;
    }

  D_handle_option_auto (&global_options, &global_options_set,
			scode, arg, value,
			d_option_lang_mask (), kind,
			loc, handlers, global_dc);

  return result;
}

/* Implements the lang_hooks.post_options routine for language D.
   Deal with any options that imply the turning on/off of features.
   FN is the main input filename passed on the command line.  */

static bool
d_post_options (const char ** fn)
{
  /* Verify the input file name.  */
  const char *filename = *fn;
  if (!filename || strcmp (filename, "-") == 0)
    filename = "";

  /* The front end considers the first input file to be the main one.  */
  *fn = filename;

  /* Release mode doesn't turn off bounds checking for safe functions.  */
  if (global.params.useArrayBounds == CHECKENABLEdefault)
    {
      global.params.useArrayBounds = global.params.release
	? CHECKENABLEsafeonly : CHECKENABLEon;
    }

  /* Assert code is generated if unittests are being compiled also, even if
     release mode is turned on.  */
  if (global.params.useAssert == CHECKENABLEdefault)
    {
      if (global.params.useUnitTests || !global.params.release)
	global.params.useAssert = CHECKENABLEon;
      else
	global.params.useAssert = CHECKENABLEoff;
    }

  /* Checks for switches without a default are turned off in release mode.  */
  if (global.params.useSwitchError == CHECKENABLEdefault)
    {
      global.params.useSwitchError = global.params.release
	? CHECKENABLEoff : CHECKENABLEon;
    }

  /* Contracts are turned off in release mode.  */
  if (global.params.useInvariants == CHECKENABLEdefault)
    {
      global.params.useInvariants = global.params.release
	? CHECKENABLEoff : CHECKENABLEon;
    }

  if (global.params.useIn == CHECKENABLEdefault)
    {
      global.params.useIn = global.params.release
	? CHECKENABLEoff : CHECKENABLEon;
    }

  if (global.params.useOut == CHECKENABLEdefault)
    {
      global.params.useOut = global.params.release
	? CHECKENABLEoff : CHECKENABLEon;
    }

  /* When not linking against D runtime, turn off all code generation that
     would otherwise reference it.  */
  if (global.params.betterC)
    {
      if (!OPTION_SET_P (flag_moduleinfo))
	global.params.useModuleInfo = false;

      /* Ensure that the front-end options are in sync with the `-frtti' and
	 `-fexceptions' flags.  */
      if (!OPTION_SET_P (flag_rtti))
	{
	  global.params.useTypeInfo = false;
	  flag_rtti = false;
	}

      if (!OPTION_SET_P (flag_exceptions))
	{
	  global.params.useExceptions = false;
	  flag_exceptions = false;
	}

      global.params.useGC = false;
      global.params.checkAction = CHECKACTION_C;
    }

  /* Enabling DIP1021 implies DIP1000.  */
  if (global.params.useDIP1021)
    global.params.useDIP1000 = FeatureState::enabled;

  /* Keep in sync with existing -fbounds-check flag.  */
  flag_bounds_check = (global.params.useArrayBounds == CHECKENABLEon);

  /* Turn off partitioning unless it was explicitly requested, as it doesn't
     work with D exception chaining, where EH handler uses LSDA to determine
     whether two thrown exception are in the same context.  */
  if (!OPTION_SET_P (flag_reorder_blocks_and_partition))
    global_options.x_flag_reorder_blocks_and_partition = 0;

  /* Error about use of deprecated features.  */
  if (global.params.useDeprecated == DIAGNOSTICinform
      && global.params.useWarnings == DIAGNOSTICerror)
    global.params.useDeprecated = DIAGNOSTICerror;

  if (flag_excess_precision == EXCESS_PRECISION_DEFAULT)
    flag_excess_precision = EXCESS_PRECISION_STANDARD;

  global.params.useInline = flag_inline_functions;

  /* Make -fmax-errors visible to frontend's diagnostic machinery.  */
  if (OPTION_SET_P (flag_max_errors))
    global.params.v.errorLimit = flag_max_errors;

  global.params.v.showColumns = flag_show_column;
  global.params.v.errorPrintMode = flag_diagnostics_show_caret
    ? ErrorPrintMode::printErrorContext : ErrorPrintMode::simpleError;

  /* Keep the front-end location type in sync with params.  */
  Loc::set (global.params.v.showColumns, global.params.v.messageStyle);

  if (global.params.useInline)
    global.params.dihdr.fullOutput = true;

  global.params.obj = !flag_syntax_only;

  /* The front-end parser only has access to `compileEnv', synchronize its
     fields with params.  */
  global.compileEnv.previewIn = global.params.previewIn;
  global.compileEnv.transitionIn = global.params.v.vin;
  global.compileEnv.ddocOutput = global.params.ddoc.doOutput;
  global.compileEnv.cCharLookupTable =
    IdentifierCharLookup::forTable (IdentifierTable::C11);
  global.compileEnv.dCharLookupTable =
    IdentifierCharLookup::forTable (IdentifierTable::LR);

  if (warn_return_type == -1)
    warn_return_type = 0;

  return false;
}

/* Add the module M to the list of modules that may declare GCC builtins.
   These are scanned after first semantic and before codegen passes.
   See d_maybe_set_builtin() for the implementation.  */

void
d_add_builtin_module (Module *m)
{
  builtin_modules.push (m);
}

/* Writes to FILENAME.  DATA is the full content of the file to be written.  */

static void
d_write_file (const char *filename, const char *data)
{
  FILE *stream;

  if (filename && (filename[0] != '-' || filename[1] != '\0'))
    stream = fopen (filename, "w");
  else
    stream = stdout;

  if (!stream)
    {
      error ("unable to open %s for writing: %m", filename);
      return;
    }

  fprintf (stream, "%s", data);

  if (stream != stdout && (ferror (stream) || fclose (stream)))
    error ("writing output file %s: %m", filename);
}

/* Read ddoc macro files named by the DDOCFILES, then write the concatenated
   the contents into DDOCBUF.  */

static void
d_read_ddoc_files (Strings &ddocfiles, OutBuffer &ddocbuf)
{
  if (ddocbuf.length ())
    return;

  for (size_t i = 0; i < ddocfiles.length; i++)
    {
      int fd = open (ddocfiles[i], O_RDONLY);
      bool ok = false;
      struct stat buf;

      if (fd == -1 || fstat (fd, &buf))
	{
	  error ("unable to open %s for reading: %m", ddocfiles[i]);
	  continue;
	}

      /* Check we've not been given a directory, or a file bigger than 4GB.  */
      if (S_ISDIR (buf.st_mode))
	errno = ENOENT;
      else if (buf.st_size != unsigned (buf.st_size))
	errno = EMFILE;
      else
	{
	  unsigned size = unsigned (buf.st_size);
	  char *buffer = (char *) xmalloc (size);

	  if (read (fd, buffer, size) == ssize_t (size))
	    {
	      ddocbuf.write (buffer, size);
	      ok = true;
	    }

	  free (buffer);
	}

      close (fd);
      if (!ok)
	fatal_error (input_location, "reading ddoc file %s: %m", ddocfiles[i]);
    }
}

static void
d_generate_ddoc_file (Module *m, OutBuffer &ddocbuf)
{
  input_location = make_location_t (m->loc);

  d_read_ddoc_files (global.params.ddoc.files, ddocbuf);

  OutBuffer ddocbuf_out;
  dmd::gendocfile (m, ddocbuf.peekChars (), ddocbuf.length (), global.datetime,
		   global.errorSink, ddocbuf_out);

  d_write_file (m->docfile.toChars (), ddocbuf_out.peekChars ());
}

/* Implements the lang_hooks.parse_file routine for language D.  */

static void
d_parse_file (void)
{
  if (global.params.v.verbose)
    {
      /* Dump information about the D compiler and language version.  */
      message ("binary    %s", global.params.argv0.ptr);
      message ("version   %s", global.versionChars ());

      /* Dump all predefined version identifiers.  */
      obstack buffer;
      gcc_obstack_init (&buffer);
      obstack_grow (&buffer, "predefs  ", 9);
      for (size_t i = 0; i < global.versionids.length; i++)
	{
	  Identifier *id = global.versionids[i];
	  const char *str = id->toChars ();
	  obstack_1grow (&buffer, ' ');
	  obstack_grow (&buffer, str, strlen (str));
	}

      obstack_1grow (&buffer, '\0');
      message ("%s", (char *) obstack_finish (&buffer));
    }

  /* Start the main input file, if the debug writer wants it.  */
  if (debug_hooks->start_end_main_source_file)
    debug_hooks->start_source_file (0, main_input_filename);

  /* Create Module's for all sources we will load.  */
  Modules modules;
  modules.reserve (num_in_fnames);

  /* Buffer for contents of .ddoc files.  */
  OutBuffer ddocbuf;

  /* In this mode, the main input file is supposed to be the same as the one
     given by -fonly=.  */
  if (d_option.fonly && !endswith (main_input_filename, d_option.fonly))
    error ("%<-fonly=%> argument is different from first input file name");

  for (size_t i = 0; i < num_in_fnames; i++)
    {
      if (strcmp (in_fnames[i], "-") == 0)
	{
	  /* Load the entire contents of stdin into memory.  8 kilobytes should
	     be a good enough initial size, but double on each iteration.
	     16 bytes are added for the final '\n' and 15 bytes of padding.  */
	  ssize_t size = 8 * 1024;
	  uchar *buffer = XNEWVEC (uchar, size + 16);
	  ssize_t len = 0;
	  ssize_t count;

	  while ((count = read (STDIN_FILENO, buffer + len, size - len)) > 0)
	    {
	      len += count;
	      if (len == size)
		{
		  size *= 2;
		  buffer = XRESIZEVEC (uchar, buffer, size + 16);
		}
	    }

	  if (count < 0)
	    {
	      error (Loc::singleFilename ("stdin"), "%s", xstrerror (errno));
	      free (buffer);
	      continue;
	    }

	  /* Handling stdin, generate a unique name for the module.  */
	  Module *m = Module::create (in_fnames[i],
				      Identifier::idPool ("__stdin"),
				      global.params.ddoc.doOutput,
				      global.params.dihdr.doOutput);
	  modules.push (m);

	  /* Zero the padding past the end of the buffer so the D lexer has a
	     sentinel.  The lexer only reads up to 4 bytes at a time.  */
	  memset (buffer + len, '\0', 16);

	  /* Overwrite the source file for the module, the one created by
	     Module::create would have a forced a `.d' suffix.  */
	  m->src.length = len;
	  m->src.ptr = buffer;
	}
      else
	{
	  /* Handling a D source file, strip off the path and extension.  */
	  const char *basename = FileName::name (in_fnames[i]);
	  const char *name = FileName::removeExt (basename);

	  Module *m = Module::create (in_fnames[i], Identifier::idPool (name),
				      global.params.ddoc.doOutput,
				      global.params.dihdr.doOutput);
	  modules.push (m);
	  FileName::free (name);
	}
    }

  /* Read all D source files.  */
  for (size_t i = 0; i < modules.length; i++)
    {
      Module *m = modules[i];
      m->read (Loc ());
    }

  /* Parse all D source files.  */
  for (size_t i = 0; i < modules.length; i++)
    {
      Module *m = modules[i];

      if (global.params.v.verbose)
	message ("parse     %s", m->toChars ());

      if (!Module::rootModule)
	Module::rootModule = m;

      m->importedFrom = m;
      m->parse ();

      if (m->filetype == FileType::ddoc)
	{
	  d_generate_ddoc_file (m, ddocbuf);

	  /* Remove M from list of modules.  */
	  modules.remove (i);
	  i--;
	}
    }

  /* Load the module containing D main.  */
  Module *main_module = NULL;
  if (global.params.addMain)
    {
      unsigned errors = global.startGagging ();
      main_module = Module::load (Loc (), NULL, Identifier::idPool ("__main"));

      if (!global.endGagging (errors))
	{
	  main_module->importedFrom = main_module;
	  modules.push (main_module);
	}
    }

  /* If an error occurs later during compilation, remember that we generated
     the headers, so that they can be removed before exit.  */
  bool dump_headers = false;

  if (global.errors)
    goto had_errors;

  if (global.params.dihdr.doOutput)
    {
      /* Generate 'header' import files.  Since 'header' import files must be
	 independent of command line switches and what else is imported, they
	 are generated before any semantic analysis.  */
      for (size_t i = 0; i < modules.length; i++)
	{
	  Module *m = modules[i];
	  if (m->filetype == FileType::dhdr
	      || (d_option.fonly && m != Module::rootModule))
	    continue;

	  if (global.params.v.verbose)
	    message ("import    %s", m->toChars ());

	  OutBuffer buf;
	  dmd::genhdrfile (m, global.params.dihdr.fullOutput, buf);
	  d_write_file (m->hdrfile.toChars (), buf.peekChars ());
	}

      dump_headers = true;
    }

  if (global.errors)
    goto had_errors;

  /* Load all unconditional imports for better symbol resolving.  */
  for (size_t i = 0; i < modules.length; i++)
    {
      Module *m = modules[i];

      if (global.params.v.verbose)
	message ("importall %s", m->toChars ());

      dmd::importAll (m, NULL);
    }

  if (global.errors)
    goto had_errors;

  /* Do semantic analysis.  */
  doing_semantic_analysis_p = true;

  for (size_t i = 0; i < modules.length; i++)
    {
      Module *m = modules[i];

      /* If this is the `__main` module, check that `D main` hasn't already
	 been declared in user code before running semantic on it.  */
      if (m == main_module && global.hasMainFunction)
	{
	  modules.remove (i);
	  continue;
	}

      if (global.params.v.verbose)
	message ("semantic  %s", m->toChars ());

      dmd::dsymbolSemantic (m, NULL);
    }

  /* Do deferred semantic analysis.  */
  Module::runDeferredSemantic ();

  if (Module::deferred.length)
    {
      for (size_t i = 0; i < Module::deferred.length; i++)
	{
	  Dsymbol *sd = Module::deferred[i];
	  error_at (make_location_t (sd->loc),
		    "unable to resolve forward reference in definition");
	}
    }

  /* Process all built-in modules or functions now for CTFE.  */
  while (builtin_modules.length != 0)
    {
      Module *m = builtin_modules.pop ();
      d_maybe_set_builtin (m);
    }

  /* Do pass 2 semantic analysis.  */
  for (size_t i = 0; i < modules.length; i++)
    {
      Module *m = modules[i];

      if (global.params.v.verbose)
	message ("semantic2 %s", m->toChars ());

      dmd::semantic2 (m, NULL);
    }

  Module::runDeferredSemantic2 ();

  if (global.errors)
    goto had_errors;

  /* Do pass 3 semantic analysis.  */
  for (size_t i = 0; i < modules.length; i++)
    {
      Module *m = modules[i];

      if (global.params.v.verbose)
	message ("semantic3 %s", m->toChars ());

      dmd::semantic3 (m, NULL);
    }

  if (includeImports)
    {
      for (size_t i = 0; i < compiledImports.length; i++)
	{
	  Module *m = compiledImports[i];
	  gcc_assert (m->isRoot ());

	  if (global.params.v.verbose)
	    message ("semantic3 %s", m->toChars ());

	  dmd::semantic3 (m, NULL);
	  modules.push (m);
	}
    }

  Module::runDeferredSemantic3 ();

  /* Check again, incase semantic3 pass loaded any more modules.  */
  while (builtin_modules.length != 0)
    {
      Module *m = builtin_modules.pop ();
      d_maybe_set_builtin (m);
    }

  /* Do not attempt to generate output files if errors or warnings occurred.  */
  if (global.errors || global.warnings)
    goto had_errors;

  /* Generate output files.  */
  doing_semantic_analysis_p = false;

  if (Module::rootModule)
    {
      /* Declare the name of the root module as the first global name in order
	 to make the middle-end fully deterministic.  */
      OutBuffer buf;
      dmd::mangleToBuffer (Module::rootModule, buf);
      first_global_object_name = buf.extractChars ();
    }

  /* Make dependencies.  */
  if (d_option.deps)
    {
      obstack buffer;
      gcc_obstack_init (&buffer);

      for (size_t i = 0; i < modules.length; i++)
	deps_write (modules[i], &buffer);

      /* -MF <arg> overrides -M[M]D.  */
      if (d_option.deps_filename_user)
	d_option.deps_filename = d_option.deps_filename_user;

      d_write_file (d_option.deps_filename,
		    (char *) obstack_finish (&buffer));
    }

  if (global.params.v.templates)
    {
      dmd::printTemplateStats (global.params.v.templatesListInstances,
			       global.errorSink);
    }

  /* Generate JSON files.  */
  if (global.params.json.doOutput)
    {
      OutBuffer buf;
      dmd::json_generate (modules, buf);
      d_write_file (global.params.json.name.ptr, buf.peekChars ());
    }

  /* Generate Ddoc files.  */
  if (global.params.ddoc.doOutput && !global.errors && !errorcount)
    {
      for (size_t i = 0; i < modules.length; i++)
	{
	  Module *m = modules[i];
	  d_generate_ddoc_file (m, ddocbuf);
	}
    }

  /* Handle -fdump-d-original.  */
  if (global.params.vcg_ast)
    {
      for (size_t i = 0; i < modules.length; i++)
	{
	  Module *m = modules[i];
	  OutBuffer buf;
	  buf.doindent = 1;

	  dmd::moduleToBuffer (buf, true, m);
	  message ("%s", buf.peekChars ());
	}
    }

  /* Generate C++ header files.  */
  if (global.params.cxxhdr.doOutput)
    dmd::genCppHdrFiles (modules);

  if (global.errors)
    goto had_errors;

  for (size_t i = 0; i < modules.length; i++)
    {
      Module *m = modules[i];

      /* Skip generating code for header files, or when the module wasn't
	 specified by `-fonly=`.  */
      if ((m->filetype == FileType::dhdr && m != main_module)
	  || (d_option.fonly && m != Module::rootModule))
	continue;

      if (global.params.v.verbose)
	message ("code      %s", m->toChars ());

      if (!flag_syntax_only)
	build_decl_tree (m);
    }

  /* And end the main input file, if the debug writer wants it.  */
  if (debug_hooks->start_end_main_source_file)
    debug_hooks->end_source_file (0);

 had_errors:
  /* Add the D frontend error count to the GCC error count to correctly
     exit with an error status.  */
  errorcount += (global.errors + global.warnings);

  /* We want to write the mixin expansion file also on error.  */
  if (global.params.mixinOut.doOutput)
    {
      d_write_file (global.params.mixinOut.name.ptr,
		    global.params.mixinOut.buffer->peekChars ());
    }

  /* Remove generated .di files on error.  */
  if (errorcount && dump_headers)
    {
      for (size_t i = 0; i < modules.length; i++)
	{
	  Module *m = modules[i];
	  if (m->filetype == FileType::dhdr
	      || (d_option.fonly && m != Module::rootModule))
	    continue;

	  remove (m->hdrfile.toChars ());
	}
    }

  /* Write out globals.  */
  d_finish_compilation (vec_safe_address (global_declarations),
			vec_safe_length (global_declarations));
}

/* Implements the lang_hooks.types.type_for_mode routine for language D.  */

static tree
d_type_for_mode (machine_mode mode, int unsignedp)
{
  if (mode == QImode)
    return unsignedp ? d_ubyte_type : d_byte_type;

  if (mode == HImode)
    return unsignedp ? d_ushort_type : d_short_type;

  if (mode == SImode)
    return unsignedp ? d_uint_type : d_int_type;

  if (mode == DImode)
    return unsignedp ? d_ulong_type : d_long_type;

  if (mode == TYPE_MODE (d_cent_type))
    return unsignedp ? d_ucent_type : d_cent_type;

  if (mode == TYPE_MODE (float_type_node))
    return float_type_node;

  if (mode == TYPE_MODE (double_type_node))
    return double_type_node;

  if (mode == TYPE_MODE (long_double_type_node))
    return long_double_type_node;

  if (mode == TYPE_MODE (build_pointer_type (char8_type_node)))
    return build_pointer_type (char8_type_node);

  if (mode == TYPE_MODE (build_pointer_type (d_int_type)))
    return build_pointer_type (d_int_type);

  for (int i = 0; i < NUM_INT_N_ENTS; i ++)
    {
      if (int_n_enabled_p[i] && mode == int_n_data[i].m)
	{
	  if (unsignedp)
	    return int_n_trees[i].unsigned_type;
	  else
	    return int_n_trees[i].signed_type;
	}
    }

  if (COMPLEX_MODE_P (mode))
    {
      machine_mode inner_mode;
      tree inner_type;

      if (mode == TYPE_MODE (complex_float_type_node))
	return complex_float_type_node;
      if (mode == TYPE_MODE (complex_double_type_node))
	return complex_double_type_node;
      if (mode == TYPE_MODE (complex_long_double_type_node))
	return complex_long_double_type_node;

      inner_mode = (machine_mode) GET_MODE_INNER (mode);
      inner_type = d_type_for_mode (inner_mode, unsignedp);
      if (inner_type != NULL_TREE)
	return build_complex_type (inner_type);
    }
  else if (VECTOR_MODE_P (mode))
    {
      machine_mode inner_mode = (machine_mode) GET_MODE_INNER (mode);
      tree inner_type = d_type_for_mode (inner_mode, unsignedp);
      if (inner_type != NULL_TREE)
	return build_vector_type_for_mode (inner_type, mode);
    }

  return 0;
}

/* Implements the lang_hooks.types.type_for_size routine for language D.  */

static tree
d_type_for_size (unsigned bits, int unsignedp)
{
  if (bits <= TYPE_PRECISION (d_byte_type))
    return unsignedp ? d_ubyte_type : d_byte_type;

  if (bits <= TYPE_PRECISION (d_short_type))
    return unsignedp ? d_ushort_type : d_short_type;

  if (bits <= TYPE_PRECISION (d_int_type))
    return unsignedp ? d_uint_type : d_int_type;

  if (bits <= TYPE_PRECISION (d_long_type))
    return unsignedp ? d_ulong_type : d_long_type;

  if (bits <= TYPE_PRECISION (d_cent_type))
    return unsignedp ? d_ucent_type : d_cent_type;

  for (int i = 0; i < NUM_INT_N_ENTS; i ++)
    {
      if (int_n_enabled_p[i] && bits == int_n_data[i].bitsize)
	{
	  if (unsignedp)
	    return int_n_trees[i].unsigned_type;
	  else
	    return int_n_trees[i].signed_type;
	}
    }

  return 0;
}

/* Implements the lang_hooks.types.type_promotes_to routine for language D.  */

static tree
d_type_promotes_to (tree type)
{
  /* Promotions are only applied on unnamed function arguments for declarations
     with `extern(C)' or `extern(C++)' linkage.  */
  if (cfun && DECL_LANG_FRONTEND (cfun->decl)
      && DECL_LANG_FRONTEND (cfun->decl)->resolvedLinkage () != LINK::d)
    {
      /* In [type/integer-promotions], integer promotions are conversions of the
	 following types:

		bool	int
		byte	int
		ubyte	int
		short	int
		ushort	int
		char	int
		wchar	int
		dchar	uint

	 If an enum has as a base type one of the types in the left column, it
	 is converted to the type in the right column.  */
      if (TREE_CODE (type) == ENUMERAL_TYPE && ENUM_IS_SCOPED (type))
	type = TREE_TYPE (type);

      type = TYPE_MAIN_VARIANT (type);

      /* Check for promotions of target-defined types first.  */
      tree promoted_type = targetm.promoted_type (type);
      if (promoted_type)
	return promoted_type;

      if (TREE_CODE (type) == BOOLEAN_TYPE)
	return d_int_type;

      if (INTEGRAL_TYPE_P (type))
	{
	  if (type == d_byte_type || type == d_ubyte_type
	      || type == d_short_type || type == d_ushort_type
	      || type == char8_type_node || type == char16_type_node)
	    return d_int_type;

	  if (type == char32_type_node)
	    return d_uint_type;

	  if (TYPE_PRECISION (type) < TYPE_PRECISION (d_int_type))
	    return d_int_type;
	}

      /* Float arguments are converted to doubles.  */
      if (type == float_type_node)
	return double_type_node;

      if (type == ifloat_type_node)
	return idouble_type_node;
    }

  return type;
}

/* Implements the lang_hooks.decls.global_bindings_p routine for language D.
   Return true if we are in the global binding level.  */

static bool
d_global_bindings_p (void)
{
  return (current_binding_level == global_binding_level);
}

/* Return global_context, but create it first if need be.  */

static tree
get_global_context (void)
{
  if (!global_context)
    {
      global_context = build_translation_unit_decl (NULL_TREE);
      debug_hooks->register_main_translation_unit (global_context);
    }

  return global_context;
}

/* Implements the lang_hooks.decls.pushdecl routine for language D.
   Record DECL as belonging to the current lexical scope.  */

tree
d_pushdecl (tree decl)
{
  /* Set the context of the decl.  If current_function_decl did not help in
     determining the context, use global scope.  */
  if (!DECL_CONTEXT (decl))
    {
      if (current_function_decl)
	DECL_CONTEXT (decl) = current_function_decl;
      else
	DECL_CONTEXT (decl) = get_global_context ();
    }

  /* Put decls on list in reverse order.  */
  if (TREE_STATIC (decl) || d_global_bindings_p ())
    vec_safe_push (global_declarations, decl);
  else
    {
      TREE_CHAIN (decl) = current_binding_level->names;
      current_binding_level->names = decl;
    }

  return decl;
}

/* Implements the lang_hooks.decls.getdecls routine for language D.
   Return the list of declarations of the current level.  */

static tree
d_getdecls (void)
{
  if (current_binding_level)
    return current_binding_level->names;

  return NULL_TREE;
}


/* Implements the lang_hooks.get_alias_set routine for language D.
   Get the alias set corresponding to type or expression T.
   Return -1 if we don't do anything special.  */

static alias_set_type
d_get_alias_set (tree)
{
  /* For now in D, assume everything aliases everything else, until we define
     some solid rules backed by a specification.  There are also some parts
     of code generation routines that don't adhere to C alias rules, such as
     build_vconvert.  In any case, a lot of user code already assumes there
     is no strict aliasing and will break if we were to change that.  */
  return 0;
}

/* Implements the lang_hooks.types_compatible_p routine for language D.
   Compares two types for equivalence in the D programming language.
   This routine should only return 1 if it is sure, even though the frontend
   should have already ensured that all types are compatible before handing
   over the parsed ASTs to the code generator.  */

static int
d_types_compatible_p (tree x, tree y)
{
  Type *tx = TYPE_LANG_FRONTEND (x);
  Type *ty = TYPE_LANG_FRONTEND (y);

  /* Try validating the types in the frontend.  */
  if (tx != NULL && ty != NULL)
    {
      /* Types are equivalent.  */
      if (same_type_p (tx, ty))
	return true;

      /* Type system allows implicit conversion between.  */
      if (dmd::implicitConvTo (tx, ty) != MATCH::nomatch
	  || dmd::implicitConvTo (ty, tx) != MATCH::nomatch)
	return true;
    }

  /* Fallback on using type flags for comparison.  E.g: all dynamic arrays
     are distinct types in D, but are VIEW_CONVERT compatible.  */
  if (TREE_CODE (x) == RECORD_TYPE && TREE_CODE (y) == RECORD_TYPE)
    {
      if (TYPE_DYNAMIC_ARRAY (x) && TYPE_DYNAMIC_ARRAY (y))
	return true;

      if (TYPE_DELEGATE (x) && TYPE_DELEGATE (y))
	return true;

      if (TYPE_ASSOCIATIVE_ARRAY (x) && TYPE_ASSOCIATIVE_ARRAY (y))
	return true;
    }

  return false;
}

/* Implements the lang_hooks.finish_incomplete_decl routine for language D.  */

static void
d_finish_incomplete_decl (tree decl)
{
  if (VAR_P (decl))
    {
      /* D allows zero-length declarations.  Such a declaration ends up with
	 DECL_SIZE (t) == NULL_TREE which is what the back-end function
	 assembler_variable checks.  This could change in later versions, or
	 maybe all of these variables should be aliased to one symbol.  */
      if (DECL_SIZE (decl) == 0)
	{
	  DECL_SIZE (decl) = bitsize_zero_node;
	  DECL_SIZE_UNIT (decl) = size_zero_node;
	}
    }
}

/* Implements the lang_hooks.types.classify_record routine for language D.
   Return the true debug type for TYPE.  */

static classify_record
d_classify_record (tree type)
{
  Type *t = TYPE_LANG_FRONTEND (type);
  TypeClass *tc = t ? t->isTypeClass () : NULL;

  if (tc != NULL)
    {
      /* extern(C++) interfaces get emitted as classes.  */
      if (tc->sym->isInterfaceDeclaration ()
	  && !tc->sym->isCPPinterface ())
	return RECORD_IS_INTERFACE;

      return RECORD_IS_CLASS;
    }

  return RECORD_IS_STRUCT;
}

/* Implements the lang_hooks.tree_size routine for language D.
   Determine the size of our tcc_constant or tcc_exceptional nodes.  */

static size_t
d_tree_size (tree_code code)
{
  switch (code)
    {
    case FUNCFRAME_INFO:
      return sizeof (tree_frame_info);

    default:
      gcc_unreachable ();
    }
}

/* Implements the lang_hooks.print_xnode routine for language D.  */

static void
d_print_xnode (FILE *file, tree node, int indent)
{
  switch (TREE_CODE (node))
    {
    case FUNCFRAME_INFO:
      print_node (file, "frame_type", FRAMEINFO_TYPE (node), indent + 4);
      break;

    default:
      break;
    }
}

/* Return which tree structure is used by NODE, or TS_D_GENERIC if NODE
   is one of the language-independent trees.  */

d_tree_node_structure_enum
d_tree_node_structure (lang_tree_node *t)
{
  switch (TREE_CODE (&t->generic))
    {
    case IDENTIFIER_NODE:
      return TS_D_IDENTIFIER;

    case FUNCFRAME_INFO:
      return TS_D_FRAMEINFO;

    default:
      return TS_D_GENERIC;
    }
}

/* Allocate and return a lang specific structure for the frontend type.  */

struct lang_type *
build_lang_type (Type *t)
{
  struct lang_type *lt = ggc_cleared_alloc <struct lang_type> ();
  lt->type = t;
  return lt;
}

/* Allocate and return a lang specific structure for the frontend decl.  */

struct lang_decl *
build_lang_decl (Declaration *d)
{
  /* For compiler generated run-time typeinfo, a lang_decl is allocated even if
     there's no associated frontend symbol to refer to (yet).  If the symbol
     appears later in the compilation, then the slot will be re-used.  */
  if (d == NULL)
    return ggc_cleared_alloc <struct lang_decl> ();

  struct lang_decl *ld = (d->csym) ? DECL_LANG_SPECIFIC (d->csym) : NULL;
  if (ld == NULL)
    ld = ggc_cleared_alloc <struct lang_decl> ();

  if (ld->decl == NULL)
    ld->decl = d;

  return ld;
}

/* Implements the lang_hooks.dup_lang_specific_decl routine for language D.
   Replace the DECL_LANG_SPECIFIC field of NODE with a copy.  */

static void
d_dup_lang_specific_decl (tree node)
{
  if (!DECL_LANG_SPECIFIC (node))
    return;

  struct lang_decl *ld = ggc_alloc <struct lang_decl> ();
  memcpy (ld, DECL_LANG_SPECIFIC (node), sizeof (struct lang_decl));
  DECL_LANG_SPECIFIC (node) = ld;
}

/* This preserves trees we create from the garbage collector.  */

static GTY(()) tree d_keep_list = NULL_TREE;

void
d_keep (tree t)
{
  d_keep_list = tree_cons (NULL_TREE, t, d_keep_list);
}

/* Implements the lang_hooks.eh_personality routine for language D.
   Return the GDC personality function decl.  */

static GTY(()) tree d_eh_personality_decl;

static tree
d_eh_personality (void)
{
  if (!d_eh_personality_decl)
    d_eh_personality_decl = build_personality_function ("gdc");

  return d_eh_personality_decl;
}

/* Implements the lang_hooks.eh_runtime_type routine for language D.  */

static tree
d_build_eh_runtime_type (tree type)
{
  Type *t = TYPE_LANG_FRONTEND (type);
  gcc_assert (t != NULL);
  t = t->toBasetype ();

  ClassDeclaration *cd = t->isTypeClass ()->sym;
  tree decl;

  if (cd->isCPPclass ())
    decl = get_cpp_typeinfo_decl (cd);
  else
    decl = get_classinfo_decl (cd);

  return convert (ptr_type_node, build_address (decl));
}

/* Implements the lang_hooks.enum_underlying_base_type routine for language D.
   Returns the underlying type of the given enumeration TYPE.  */

static tree
d_enum_underlying_base_type (const_tree type)
{
  gcc_assert (TREE_CODE (type) == ENUMERAL_TYPE);
  return TREE_TYPE (type);
}

/* Get a value for the SARIF v2.1.0 "artifact.sourceLanguage" property,
   based on the list in SARIF v2.1.0 Appendix J.  */

static const char *
d_get_sarif_source_language (const char *)
{
  return "d";
}

const scoped_attribute_specs *const d_langhook_attribute_table[] =
{
  &d_langhook_gnu_attribute_table,
  &d_langhook_common_attribute_table,
};

/* Definitions for our language-specific hooks.  */

#undef LANG_HOOKS_NAME
#undef LANG_HOOKS_INIT
#undef LANG_HOOKS_INIT_TS
#undef LANG_HOOKS_INIT_OPTIONS
#undef LANG_HOOKS_INIT_OPTIONS_STRUCT
#undef LANG_HOOKS_OPTION_LANG_MASK
#undef LANG_HOOKS_HANDLE_OPTION
#undef LANG_HOOKS_POST_OPTIONS
#undef LANG_HOOKS_PARSE_FILE
#undef LANG_HOOKS_ATTRIBUTE_TABLE
#undef LANG_HOOKS_GET_ALIAS_SET
#undef LANG_HOOKS_TYPES_COMPATIBLE_P
#undef LANG_HOOKS_BUILTIN_FUNCTION
#undef LANG_HOOKS_BUILTIN_FUNCTION_EXT_SCOPE
#undef LANG_HOOKS_REGISTER_BUILTIN_TYPE
#undef LANG_HOOKS_FINISH_INCOMPLETE_DECL
#undef LANG_HOOKS_GIMPLIFY_EXPR
#undef LANG_HOOKS_CLASSIFY_RECORD
#undef LANG_HOOKS_TREE_SIZE
#undef LANG_HOOKS_PRINT_XNODE
#undef LANG_HOOKS_DUP_LANG_SPECIFIC_DECL
#undef LANG_HOOKS_EH_PERSONALITY
#undef LANG_HOOKS_EH_RUNTIME_TYPE
#undef LANG_HOOKS_ENUM_UNDERLYING_BASE_TYPE
#undef LANG_HOOKS_PUSHDECL
#undef LANG_HOOKS_GETDECLS
#undef LANG_HOOKS_GLOBAL_BINDINGS_P
#undef LANG_HOOKS_TYPE_FOR_MODE
#undef LANG_HOOKS_TYPE_FOR_SIZE
#undef LANG_HOOKS_TYPE_PROMOTES_TO
#undef LANG_HOOKS_GET_SARIF_SOURCE_LANGUAGE

#define LANG_HOOKS_NAME			    "GNU D"
#define LANG_HOOKS_INIT			    d_init
#define LANG_HOOKS_INIT_TS		    d_init_ts
#define LANG_HOOKS_INIT_OPTIONS		    d_init_options
#define LANG_HOOKS_INIT_OPTIONS_STRUCT	    d_init_options_struct
#define LANG_HOOKS_OPTION_LANG_MASK	    d_option_lang_mask
#define LANG_HOOKS_HANDLE_OPTION	    d_handle_option
#define LANG_HOOKS_POST_OPTIONS		    d_post_options
#define LANG_HOOKS_PARSE_FILE		    d_parse_file
#define LANG_HOOKS_ATTRIBUTE_TABLE	    d_langhook_attribute_table
#define LANG_HOOKS_GET_ALIAS_SET	    d_get_alias_set
#define LANG_HOOKS_TYPES_COMPATIBLE_P	    d_types_compatible_p
#define LANG_HOOKS_BUILTIN_FUNCTION	    d_builtin_function
#define LANG_HOOKS_BUILTIN_FUNCTION_EXT_SCOPE d_builtin_function_ext_scope
#define LANG_HOOKS_REGISTER_BUILTIN_TYPE    d_register_builtin_type
#define LANG_HOOKS_FINISH_INCOMPLETE_DECL   d_finish_incomplete_decl
#define LANG_HOOKS_GIMPLIFY_EXPR	    d_gimplify_expr
#define LANG_HOOKS_CLASSIFY_RECORD	    d_classify_record
#define LANG_HOOKS_TREE_SIZE		    d_tree_size
#define LANG_HOOKS_PRINT_XNODE		    d_print_xnode
#define LANG_HOOKS_DUP_LANG_SPECIFIC_DECL   d_dup_lang_specific_decl
#define LANG_HOOKS_EH_PERSONALITY	    d_eh_personality
#define LANG_HOOKS_EH_RUNTIME_TYPE	    d_build_eh_runtime_type
#define LANG_HOOKS_ENUM_UNDERLYING_BASE_TYPE d_enum_underlying_base_type
#define LANG_HOOKS_PUSHDECL		    d_pushdecl
#define LANG_HOOKS_GETDECLS		    d_getdecls
#define LANG_HOOKS_GLOBAL_BINDINGS_P	    d_global_bindings_p
#define LANG_HOOKS_TYPE_FOR_MODE	    d_type_for_mode
#define LANG_HOOKS_TYPE_FOR_SIZE	    d_type_for_size
#define LANG_HOOKS_TYPE_PROMOTES_TO	    d_type_promotes_to
#define LANG_HOOKS_GET_SARIF_SOURCE_LANGUAGE d_get_sarif_source_language

struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#include "gt-d-d-lang.h"
#include "gtype-d.h"
