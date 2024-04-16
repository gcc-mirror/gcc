/* gm2-lang.cc language-dependent hooks for GNU Modula-2.

Copyright (C) 2002-2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING.  If not, write to the
Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#define INCLUDE_VECTOR
#include "gm2-gcc/gcc-consolidation.h"

#include "langhooks-def.h" /* FIXME: for lhd_set_decl_assembler_name.  */
#include "tree-pass.h"     /* FIXME: only for PROP_gimple_any.  */
#include "toplev.h"
#include "debug.h"

#include "opts.h"

#define GM2_LANG_C
#include "gm2-lang.h"
#include "m2block.h"
#include "dynamicstrings.h"
#include "m2options.h"
#include "m2convert.h"
#include "m2linemap.h"
#include "init.h"
#include "m2-tree.h"
#include "convert.h"
#include "rtegraph.h"

#undef ENABLE_M2DUMP_ALL

static void write_globals (void);

static int insideCppArgs = FALSE;

/* We default to pim in the absence of fiso.  */
static bool iso = false;

typedef struct named_path_s {
  std::vector<const char*>path;
  const char *name;
} named_path;


/* The language include paths are based on the libraries in use.  */
static bool allow_libraries = true;
static const char *flibs = nullptr;
static const char *iprefix = nullptr;
static const char *imultilib = nullptr;
static std::vector<named_path>Ipaths;
static std::vector<const char*>isystem;
static std::vector<const char*>iquote;

#define EXPR_STMT_EXPR(NODE) TREE_OPERAND (EXPR_STMT_CHECK (NODE), 0)

/* start of new stuff.  */

/* Language-dependent contents of a type.  */

struct GTY (()) lang_type
{
  char dummy;
};

/* Language-dependent contents of a decl.  */

struct GTY (()) lang_decl
{
  char dummy;
};

/* Language-dependent contents of an identifier.  This must include a
   tree_identifier.  */

struct GTY (()) lang_identifier
{
  struct tree_identifier common;
};

/* The resulting tree type.  */

union GTY ((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
            chain_next ("CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), "
                        "TS_COMMON) ? ((union lang_tree_node *) TREE_CHAIN "
                        "(&%h.generic)) : NULL"))) lang_tree_node
{
  union tree_node GTY ((tag ("0"),
                        desc ("tree_node_structure (&%h)"))) generic;
  struct lang_identifier GTY ((tag ("1"))) identifier;
};

struct GTY (()) language_function
{

  /* While we are parsing the function, this contains information about
  the statement-tree that we are building.  */
  /* struct stmt_tree_s stmt_tree;  */
  tree stmt_tree;
};

/* Language hooks.  */

static void gm2_langhook_parse_file (void);

bool
gm2_langhook_init (void)
{
  build_common_tree_nodes (false);
  build_common_builtin_nodes ();

  /* The default precision for floating point numbers.  This is used
     for floating point constants with abstract type.  This may eventually
     be controllable by a command line option.  */
  mpfr_set_default_prec (256);

  /* GNU Modula-2 uses exceptions.  */
  using_eh_for_cleanups ();

  if (M2Options_GetPPOnly ())
    {
      /* Preprocess the file here.  */
      gm2_langhook_parse_file ();
      return false; /* Finish now, no further compilation.  */
    }
  return true;
}

/* The option mask.  */

static unsigned int
gm2_langhook_option_lang_mask (void)
{
  return CL_ModulaX2;
}

/* Initialize the options structure.  */

static void
gm2_langhook_init_options_struct (struct gcc_options *opts)
{
  /* Default to avoiding range issues for complex multiply and divide.  */
  opts->x_flag_complex_method = 2;

  /* The builtin math functions should not set errno.  */
  opts->x_flag_errno_math = 0;
  opts->frontend_set_flag_errno_math = true;

  /* Exceptions are used.  */
  opts->x_flag_exceptions = 1;
  init_FrontEndInit ();
}

/* Infrastructure for a VEC of bool values.  */

/* This array determines whether the filename is associated with the
   C preprocessor.  */

static vec<bool> filename_cpp;

/* Build the C preprocessor command line here, since we need to include
   options that are not passed to the handle_option function.  */

void
gm2_langhook_init_options (unsigned int decoded_options_count,
                           struct cl_decoded_option *decoded_options)
{
  unsigned int i;
  bool in_cpp_args = false;
  bool building_cpp_command = false;

  for (i = 1; i < decoded_options_count; i++)
    {
      enum opt_code code = (enum opt_code)decoded_options[i].opt_index;
      const struct cl_option *option = &cl_options[code];
      const char *opt = (const char *)option->opt_text;
      const char *arg = decoded_options[i].arg;
      HOST_WIDE_INT value = decoded_options[i].value;
      switch (code)
	{
	case OPT_fcpp:
	  if (value)
	    gcc_checking_assert (building_cpp_command);
	  break;
	case OPT_fcpp_begin:
	  in_cpp_args = true;
	  building_cpp_command = true;
	  break;
	case OPT_fcpp_end:
	  in_cpp_args = false;
	  break;
	case OPT_SPECIAL_input_file:
	  filename_cpp.safe_push (in_cpp_args);
	  break;

	/* C and driver opts that are not passed to the preprocessor for
	   modula-2, but that we use internally for building preprocesor
	   command lines.  */
	case OPT_B:
	  M2Options_SetB (arg);
	  break;
	case OPT_c:
	  M2Options_Setc (value);
	  break;
	case OPT_dumpdir:
	  M2Options_SetDumpDir (arg);
	  break;
	case OPT_save_temps:
	  if (building_cpp_command)
	    M2Options_SetSaveTemps (value);
	  break;
	case OPT_save_temps_:
	  if (building_cpp_command)
	    /* Also sets SaveTemps. */
	    M2Options_SetSaveTempsDir (arg);
	  break;

	case OPT_E:
	  if (!in_cpp_args)
	    {
	      M2Options_SetPPOnly (value);
	      building_cpp_command = true;
	    }
	  M2Options_CppArg (opt, arg, (option->flags & CL_JOINED)
			    && !(option->flags & CL_SEPARATE));
	  break;

	case OPT_M:
	  /* Output a rule suitable for make describing the dependencies of the
	     main source file.  */
	  if (in_cpp_args)
	    {
	      gcc_checking_assert (building_cpp_command);
	      /* This is a preprocessor command.  */
	      M2Options_CppArg (opt, arg, (option->flags & CL_JOINED)
				&& !(option->flags & CL_SEPARATE));
	    }
	  M2Options_SetPPOnly (value);
	  M2Options_SetM (value);
	  break;

	case OPT_MM:
	  if (in_cpp_args)
	    {
	      gcc_checking_assert (building_cpp_command);
	      /* This is a preprocessor command.  */
	      M2Options_CppArg (opt, arg, (option->flags & CL_JOINED)
				&& !(option->flags & CL_SEPARATE));
	    }
	  M2Options_SetPPOnly (value);
	  M2Options_SetMM (value);
	  break;

	case OPT_MF:
	  if (!in_cpp_args)
	    M2Options_SetMF (arg);
	  break;

	case OPT_MP:
	  M2Options_SetMP (value);
	  break;

	/* We can only use MQ and MT when the command line is either PP-only, or
	   when there is a MD/MMD on it.  */
	case OPT_MQ:
	  M2Options_SetMQ (arg);
	  break;

	case OPT_MT:
	  M2Options_SetMT (arg);
	  break;

	case OPT_o:
	  M2Options_SetObj (arg);
	  break;

	/* C and driver options that we ignore for the preprocessor lines.  */
	case OPT_fpch_deps:
	case OPT_fpch_preprocess:
	  break;

	case OPT_fplugin_:
	  /* FIXME: We might need to handle this specially, since the modula-2
	     plugin is not usable here, but others might be.
	     For now skip all plugins to avoid fails with the m2 one.  */
	  break;

	/* Preprocessor arguments with a following filename.  */
	case OPT_MD:
	  M2Options_SetMD (value);
	  if (value)
	    {
	      M2Options_SetM (true);
	      M2Options_SetMF (arg);
	    }
	  break;

	case OPT_MMD:
	  M2Options_SetMMD (value);
	  if (value)
	    {
	      M2Options_SetMM (true);
	      M2Options_SetMF (arg);
	    }
	  break;

	/* Modula 2 claimed options we pass to the preprocessor.  */
	case OPT_ansi:
	case OPT_traditional_cpp:
	  if (building_cpp_command)
	    M2Options_CppArg (opt, arg, (option->flags & CL_JOINED)
			      && !(option->flags & CL_SEPARATE));
	  break;

	/* Options we act on and also pass to the preprocessor.  */
	case OPT_O:
	  M2Options_SetOptimizing (value);
	  if (building_cpp_command)
	    M2Options_CppArg (opt, arg, (option->flags & CL_JOINED)
			      && !(option->flags & CL_SEPARATE));
	  break;
	case OPT_quiet:
	  M2Options_SetQuiet (value);
	  if (building_cpp_command)
	    M2Options_CppArg (opt, arg, (option->flags & CL_JOINED)
			      && !(option->flags & CL_SEPARATE));
	  break;
	case OPT_v:
	  M2Options_SetVerbose (value);
	  /* FALLTHROUGH */
	default:
	  /* We handled input files above.  */
	  if (code >= N_OPTS)
	    break;
	  /* Do not pass Modula-2 args to the preprocessor, any that we care
	     about here should already have been handled above.  */
	  if (option->flags & CL_ModulaX2)
	    break;
	  /* Otherwise, add this to the CPP command line.  */
	  if (building_cpp_command)
	    M2Options_CppArg (opt, arg, (option->flags & CL_JOINED)
			      && !(option->flags & CL_SEPARATE));
	  break;
	}
    }
  filename_cpp.safe_push (false);
}

static bool
is_cpp_filename (unsigned int i)
{
  gcc_assert (i < filename_cpp.length ());
  return filename_cpp[i];
}

static void
push_back_Ipath (const char *arg)
{
  if (Ipaths.empty ())
    {
      named_path np;
      np.path.push_back (arg);
      np.name = xstrdup (M2Options_GetM2PathName ());
      Ipaths.push_back (np);
    }
  else
    {
      if (strcmp (Ipaths.back ().name,
		  M2Options_GetM2PathName ()) == 0)
	Ipaths.back ().path.push_back (arg);
      else
	{
	  named_path np;
	  np.path.push_back (arg);
	  np.name = xstrdup (M2Options_GetM2PathName ());
	  Ipaths.push_back (np);
	}
    }
}

/* Handle gm2 specific options.  Return 0 if we didn't do anything.  */

bool
gm2_langhook_handle_option (
    size_t scode, const char *arg, HOST_WIDE_INT value, int kind ATTRIBUTE_UNUSED,
    location_t loc ATTRIBUTE_UNUSED,
    const struct cl_option_handlers *handlers ATTRIBUTE_UNUSED)
{
  enum opt_code code = (enum opt_code)scode;

  const struct cl_option *option = &cl_options[scode];
  /* ignore file names.  */
  if (code == N_OPTS)
    return 1;

  switch (code)
    {
    case OPT_dumpdir:
      M2Options_SetDumpDir (arg);
      return 1;
    case OPT_I:
      push_back_Ipath (arg);
      return 1;
    case OPT_fiso:
      M2Options_SetISO (value);
      iso = value;
      return 1;
    case OPT_fpim:
      M2Options_SetPIM (value);
      iso = value ? false : iso;
      return 1;
    case OPT_fpim2:
      M2Options_SetPIM2 (value);
      iso = value ? false : iso;
      return 1;
    case OPT_fpim3:
      M2Options_SetPIM3 (value);
      iso = value ? false : iso;
      return 1;
    case OPT_fpim4:
      M2Options_SetPIM4 (value);
      iso = value ? false : iso;
      return 1;
    case OPT_fpositive_mod_floor_div:
      M2Options_SetPositiveModFloor (value);
      return 1;
    case OPT_flibs_:
      allow_libraries = value;
      flibs = arg;
      return 1;
    case OPT_fgen_module_list_:
      M2Options_SetGenModuleList (value, arg);
      return 1;
    case OPT_fnil:
      M2Options_SetNilCheck (value);
      return 1;
    case OPT_fwholediv:
      M2Options_SetWholeDiv (value);
      return 1;
    case OPT_findex:
      M2Options_SetIndex (value);
      return 1;
    case OPT_frange:
      M2Options_SetRange (value);
      return 1;
    case OPT_ffloatvalue:
      M2Options_SetFloatValueCheck (value);
      return 1;
    case OPT_fwholevalue:
      M2Options_SetWholeValueCheck (value);
      return 1;
    case OPT_freturn:
      M2Options_SetReturnCheck (value);
      return 1;
    case OPT_fcase:
      M2Options_SetCaseCheck (value);
      return 1;
    case OPT_fd:
      M2Options_SetCompilerDebugging (value);
      return 1;
    case OPT_fdebug_builtins:
      M2Options_SetDebugBuiltins (value);
      return 1;
    case OPT_fdebug_function_line_numbers:
      M2Options_SetDebugFunctionLineNumbers (value);
      return 1;
    case OPT_fauto_init:
      M2Options_SetAutoInit (value);
      return 1;
    case OPT_fsoft_check_all:
      M2Options_SetCheckAll (value);
      return 1;
    case OPT_fexceptions:
      M2Options_SetExceptions (value);
      return 1;
    case OPT_Wstyle:
      M2Options_SetStyle (value);
      return 1;
    case OPT_Wpedantic:
      M2Options_SetPedantic (value);
      return 1;
    case OPT_Wpedantic_param_names:
      M2Options_SetPedanticParamNames (value);
      return 1;
    case OPT_Wpedantic_cast:
      M2Options_SetPedanticCast (value);
      return 1;
    case OPT_fextended_opaque:
      M2Options_SetExtendedOpaque (value);
      return 1;
    case OPT_Wverbose_unbounded:
      M2Options_SetVerboseUnbounded (value);
      return 1;
    case OPT_Wunused_variable:
      M2Options_SetUnusedVariableChecking (value);
      return 1;
    case OPT_Wunused_parameter:
      M2Options_SetUnusedParameterChecking (value);
      return 1;
    case OPT_Wuninit_variable_checking:
      return M2Options_SetUninitVariableChecking (value, "known");
    case OPT_Wuninit_variable_checking_:
      return M2Options_SetUninitVariableChecking (value, arg);
    case OPT_fm2_strict_type:
      M2Options_SetStrictTypeChecking (value);
      return 1;
    case OPT_fm2_debug_trace_:
      M2Options_SetM2DebugTraceFilter (value, arg);
      return 1;
#ifdef ENABLE_M2DUMP_ALL
    case OPT_fm2_dump_:
      return M2Options_SetM2Dump (value, arg);
    case OPT_fm2_dump_decl_:
      M2Options_SetDumpDeclFilename (value, arg);
      return 1;
    case OPT_fm2_dump_gimple_:
      M2Options_SetDumpGimpleFilename (value, arg);
      return 1;
    case OPT_fm2_dump_quad_:
      M2Options_SetDumpQuadFilename (value, arg);
      return 1;
    case OPT_fm2_dump_filter_:
      M2Options_SetM2DumpFilter (value, arg);
      return 1;
#endif
    case OPT_Wall:
      M2Options_SetWall (value);
      return 1;
    case OPT_Wcase_enum:
      M2Options_SetCaseEnumChecking (value);
      return 1;
#if 0
    /* Not yet implemented.  */
    case OPT_fxcode:
      M2Options_SetXCode (value);
      return 1;
#endif
    case OPT_fm2_lower_case:
      M2Options_SetLowerCaseKeywords (value);
      return 1;
    case OPT_fuse_list_:
      M2Options_SetUselist (value, arg);
      return 1;
    case OPT_fruntime_modules_:
      M2Options_SetRuntimeModuleOverride (arg);
      return 1;
    case OPT_fpthread:
      /* Handled in the driver.  */
      return 1;
    case OPT_fm2_plugin:
      /* Handled in the driver.  */
      return 1;
    case OPT_fscaffold_dynamic:
      M2Options_SetScaffoldDynamic (value);
      return 1;
    case OPT_fscaffold_static:
      M2Options_SetScaffoldStatic (value);
      return 1;
    case OPT_fscaffold_main:
      M2Options_SetScaffoldMain (value);
      return 1;
    case OPT_fcpp:
      M2Options_SetCpp (value);
      return 1;
    case OPT_fpreprocessed:
      /* Provided for compatibility; ignore for now.  */
      return 1;
    case OPT_fcpp_begin:
      insideCppArgs = TRUE;
      return 1;
    case OPT_fcpp_end:
      insideCppArgs = FALSE;
      return 1;
    case OPT_fq:
      M2Options_SetQuadDebugging (value);
      return 1;
    case OPT_fsources:
      M2Options_SetSources (value);
      return 1;
    case OPT_funbounded_by_reference:
      M2Options_SetUnboundedByReference (value);
      return 1;
    case OPT_fdef_:
      M2Options_setdefextension (arg);
      return 1;
    case OPT_fmod_:
      M2Options_setmodextension (arg);
      return 1;
    case OPT_fdump_system_exports:
      M2Options_SetDumpSystemExports (value);
      return 1;
    case OPT_fswig:
      M2Options_SetSwig (value);
      return 1;
    case OPT_fshared:
      M2Options_SetShared (value);
      return 1;
    case OPT_fm2_statistics:
      M2Options_SetStatistics (value);
      return 1;
    case OPT_fm2_g:
      M2Options_SetM2g (value);
      return 1;
      break;
    case OPT_fm2_pathname_:
      if (strcmp (arg, "-") == 0)
	M2Options_SetM2PathName ("");
      else
	M2Options_SetM2PathName (arg);
      return 1;
      break;
    case OPT_fm2_pathnameI:
      push_back_Ipath (arg);
      return 1;
      break;
    case OPT_fm2_prefix_:
      if (strcmp (arg, "-") == 0)
	M2Options_SetM2Prefix ("");
      else
	M2Options_SetM2Prefix (arg);
      return 1;
      break;
    case OPT_iprefix:
      iprefix = arg;
      return 1;
      break;
    case OPT_imultilib:
      imultilib = arg;
      return 1;
      break;
    case OPT_isystem:
      isystem.push_back (arg);
      return 1;
      break;
    case OPT_iquote:
      iquote.push_back (arg);
      return 1;
      break;
    case OPT_isysroot:
      /* Otherwise, ignored, at least for now. */
      return 1;
      break;
    case OPT_fm2_whole_program:
      M2Options_SetWholeProgram (value);
      return 1;
#ifdef OPT_mabi_ibmlongdouble
    case OPT_mabi_ibmlongdouble:
      M2Options_SetIBMLongDouble (value);
      return 1;
#endif
#ifdef OPT_mabi_ieeelongdouble
    case OPT_mabi_ieeelongdouble:
      M2Options_SetIEEELongDouble (value);
      return 1;
#endif
    case OPT_flocation_:
      if (strcmp (arg, "builtins") == 0)
        {
          M2Options_SetForcedLocation (BUILTINS_LOCATION);
          return 1;
        }
      else if (strcmp (arg, "unknown") == 0)
        {
          M2Options_SetForcedLocation (UNKNOWN_LOCATION);
          return 1;
        }
      else if ((arg != NULL) && (ISDIGIT (arg[0])))
        {
          M2Options_SetForcedLocation (atoi (arg));
          return 1;
        }
      else
        return 0;
    default:
      if (insideCppArgs)
	/* Handled in gm2_langhook_init_options ().  */
	return 1;
      else if (option->flags & CL_DRIVER)
	/* Driver options (unless specifically claimed above) should be handled
	   in gm2_langhook_init_options ().  */
	return 1;
      else if (option->flags & CL_C)
	/* C options (unless specifically claimed above) should be handled
	   in gm2_langhook_init_options ().  */
	return 1;
      break;
    }
  return 0;
}

/* This prefixes LIBNAME with the current compiler prefix (if it has been
   relocated) or the LIBSUBDIR, if not.  */
static void
add_one_import_path (const char *libname)
{
  const char *libpath = iprefix ? iprefix : LIBSUBDIR;
  const char dir_sep[] = {DIR_SEPARATOR, (char)0};
  size_t dir_sep_size = strlen (dir_sep);
  unsigned int mlib_len = 0;

  if (imultilib)
    {
      mlib_len = strlen (imultilib);
      mlib_len += strlen (dir_sep);
    }

  char *lib = (char *)alloca (strlen (libpath) + dir_sep_size
			      + strlen ("m2") + dir_sep_size
			      + strlen (libname) + 1
			      + mlib_len + 1);
  strcpy (lib, libpath);
  /* iprefix has a trailing dir separator, LIBSUBDIR does not.  */
  if (!iprefix)
    strcat (lib, dir_sep);

  if (imultilib)
    {
      strcat (lib, imultilib);
      strcat (lib, dir_sep);
    }
  strcat (lib, "m2");
  strcat (lib, dir_sep);
  strcat (lib, libname);
  M2Options_SetM2PathName (libname);
  M2Options_SetSearchPath (lib);
}

/* For each comma-separated standard library name in LIBLIST, add the
   corresponding include path.  */
static void
add_m2_import_paths (const char *liblist)
{
  while (*liblist != 0 && *liblist != '-')
    {
      const char *comma = strstr (liblist, ",");
      size_t len;
      if (comma)
	len = comma - liblist;
      else
	len = strlen (liblist);
      char *libname = (char *) alloca (len+1);
      strncpy (libname, liblist, len);
      libname[len] = 0;
      add_one_import_path (libname);
      liblist += len;
      if (*liblist == ',')
	liblist++;
    }
}

/* Run after parsing options.  */

static bool
gm2_langhook_post_options (const char **pfilename)
{
  const char *filename = *pfilename;
  flag_excess_precision = EXCESS_PRECISION_FAST;
  M2Options_SetCC1Quiet (quiet_flag);
  M2Options_FinaliseOptions ();
  main_input_filename = filename;

  /* Add the include paths as per the libraries specified.
     NOTE: This assumes that the driver has validated the input and makes
     no attempt to be defensive of nonsense input in flibs=.  */
  if (allow_libraries)
    {
      if (!flibs)
	{
	  if (iso)
	    flibs = "m2iso,m2cor,m2pim,m2log";
	  else
	    flibs = "m2pim,m2iso,m2cor,m2log";
	}
    }

  /* Add search paths.
     We are not handling all of the cases yet (e.g idirafter).
     This (barring the missing cases) is intended to follow the directory
     search rules used for c-family.  It would be less confusing if the
     presence of absence of these search paths was not dependent on the
     flibs= option. */

  for (auto *s : iquote)
    M2Options_SetSearchPath (s);
  iquote.clear();
  for (auto np : Ipaths)
    {
      M2Options_SetM2PathName (np.name);
      for (auto *s : np.path)
	M2Options_SetSearchPath (s);
    }
  Ipaths.clear();
  for (auto *s : isystem)
    M2Options_SetSearchPath (s);
  isystem.clear();
  /* FIXME: this is not a good way to suppress the addition of the import
     paths.  */
  if (allow_libraries)
    add_m2_import_paths (flibs);

  /* Returning false means that the backend should be used.  */
  return M2Options_GetPPOnly ();
}

/* Call the compiler for every source filename on the command line.  */

static void
gm2_parse_input_files (const char **filenames, unsigned int filename_count)
{
  unsigned int i;
  gcc_assert (filename_count > 0);

  for (i = 0; i < filename_count; i++)
    if (!is_cpp_filename (i))
      {
	main_input_filename = filenames[i];
	init_PerCompilationInit (filenames[i]);
      }
}

static void
gm2_langhook_parse_file (void)
{
  gm2_parse_input_files (in_fnames, num_in_fnames);
  if (!M2Options_GetPPOnly ())
    write_globals ();
}

static tree
gm2_langhook_type_for_size (unsigned int bits, int unsignedp)
{
  return gm2_type_for_size (bits, unsignedp);
}

static tree
gm2_langhook_type_for_mode (machine_mode mode, int unsignedp)
{
  tree type;

  for (int i = 0; i < NUM_INT_N_ENTS; i ++)
    if (int_n_enabled_p[i]
	&& mode == int_n_data[i].m)
      return (unsignedp ? int_n_trees[i].unsigned_type
	      : int_n_trees[i].signed_type);

  if (VECTOR_MODE_P (mode))
    {
      tree inner;

      inner = gm2_langhook_type_for_mode (GET_MODE_INNER (mode), unsignedp);
      if (inner != NULL_TREE)
        return build_vector_type_for_mode (inner, mode);
      return NULL_TREE;
    }

  scalar_int_mode imode;
  if (is_int_mode (mode, &imode))
    return gm2_langhook_type_for_size (GET_MODE_BITSIZE (imode), unsignedp);

  if (mode == TYPE_MODE (float_type_node))
    return float_type_node;

  if (mode == TYPE_MODE (double_type_node))
    return double_type_node;

  if (mode == TYPE_MODE (long_double_type_node))
    return long_double_type_node;

  if ((float128_type_node != NULL) && (mode == TYPE_MODE (float128_type_node)))
    return float128_type_node;

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

      inner_mode = GET_MODE_INNER (mode);
      inner_type = gm2_langhook_type_for_mode (inner_mode, unsignedp);
      if (inner_type != NULL_TREE)
	return build_complex_type (inner_type);
    }

#if HOST_BITS_PER_WIDE_INT >= 64
  /* The middle-end and some backends rely on TImode being supported
  for 64-bit HWI.  */
  if (mode == TImode)
    {
      type = build_nonstandard_integer_type (GET_MODE_BITSIZE (TImode),
                                             unsignedp);
      if (type && TYPE_MODE (type) == TImode)
        return type;
    }
#endif
  return NULL_TREE;
}

/* Record a builtin function.  We just ignore builtin functions.  */

static tree
gm2_langhook_builtin_function (tree decl)
{
  return decl;
}

/* Return true if we are in the global binding level.  */

static bool
gm2_langhook_global_bindings_p (void)
{
  return current_function_decl == NULL_TREE;
}

/* Unused langhook.  */

static tree
gm2_langhook_pushdecl (tree decl ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* This hook is used to get the current list of declarations as trees.
   We don't support that; instead we use write_globals.  This can't
   simply crash because it is called by -gstabs.  */

static tree
gm2_langhook_getdecls (void)
{
  return NULL;
}

/* m2_write_global_declarations writes out globals creating an array
   of the declarations and calling wrapup_global_declarations.  */

static void
m2_write_global_declarations (tree globals)
{
  auto_vec<tree> global_decls;
  tree decl = globals;
  int n = 0;

  while (decl != NULL)
    {
      global_decls.safe_push (decl);
      decl = TREE_CHAIN (decl);
      n++;
    }
  wrapup_global_declarations (global_decls.address (), n);
}

/* Write out globals.  */

static void
write_globals (void)
{
  tree t;
  unsigned i;

  m2block_finishGlobals ();

  /* Process all file scopes in this compilation, and the
     external_scope, through wrapup_global_declarations and
     check_global_declarations.  */
  FOR_EACH_VEC_ELT (*all_translation_units, i, t)
  m2_write_global_declarations (BLOCK_VARS (DECL_INITIAL (t)));
}


/* Gimplify an EXPR_STMT node.  */

static void
gimplify_expr_stmt (tree *stmt_p)
{
  gcc_assert (EXPR_STMT_EXPR (*stmt_p) != NULL_TREE);
  *stmt_p = EXPR_STMT_EXPR (*stmt_p);
}

/* Genericize a TRY_BLOCK.  */

static void
genericize_try_block (tree *stmt_p)
{
  tree body = TRY_STMTS (*stmt_p);
  tree cleanup = TRY_HANDLERS (*stmt_p);

  *stmt_p = build2 (TRY_CATCH_EXPR, void_type_node, body, cleanup);
}

/* Genericize a HANDLER by converting to a CATCH_EXPR.  */

static void
genericize_catch_block (tree *stmt_p)
{
  tree type = HANDLER_TYPE (*stmt_p);
  tree body = HANDLER_BODY (*stmt_p);

  /* FIXME should the caught type go in TREE_TYPE?  */
  *stmt_p = build2 (CATCH_EXPR, void_type_node, type, body);
}

/* Convert the tree representation of FNDECL from m2 frontend trees
   to GENERIC.  */

extern void pf (tree);

void
gm2_genericize (tree fndecl)
{
  tree t;
  struct cgraph_node *cgn;

#if 0
  pf (fndecl);
#endif
  /* Fix up the types of parms passed by invisible reference.  */
  for (t = DECL_ARGUMENTS (fndecl); t; t = DECL_CHAIN (t))
    if (TREE_ADDRESSABLE (TREE_TYPE (t)))
      {

        /* If a function's arguments are copied to create a thunk, then
	   DECL_BY_REFERENCE will be set -- but the type of the argument will be
	   a pointer type, so we will never get here.  */
        gcc_assert (!DECL_BY_REFERENCE (t));
        gcc_assert (DECL_ARG_TYPE (t) != TREE_TYPE (t));
        TREE_TYPE (t) = DECL_ARG_TYPE (t);
        DECL_BY_REFERENCE (t) = 1;
        TREE_ADDRESSABLE (t) = 0;
        relayout_decl (t);
      }

  /* Dump all nested functions now.  */
  cgn = cgraph_node::get_create (fndecl);
  for (cgn = first_nested_function (cgn);
       cgn != NULL; cgn = next_nested_function (cgn))
    gm2_genericize (cgn->decl);
}

/* gm2 gimplify expression, currently just change THROW in the same
   way as C++ */

static int
gm2_langhook_gimplify_expr (tree *expr_p, gimple_seq *pre_p ATTRIBUTE_UNUSED,
                            gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  enum tree_code code = TREE_CODE (*expr_p);

  switch (code)
    {
    case THROW_EXPR:

      /* FIXME communicate throw type to back end, probably by moving
      THROW_EXPR into ../tree.def.  */
      *expr_p = TREE_OPERAND (*expr_p, 0);
      return GS_OK;

    case EXPR_STMT:
      gimplify_expr_stmt (expr_p);
      return GS_OK;

    case TRY_BLOCK:
      genericize_try_block (expr_p);
      return GS_OK;

    case HANDLER:
      genericize_catch_block (expr_p);
      return GS_OK;

    default:
      return GS_UNHANDLED;
    }
}

static GTY(()) tree gm2_eh_personality_decl;

static tree
gm2_langhook_eh_personality (void)
{
  if (!gm2_eh_personality_decl)
    gm2_eh_personality_decl = build_personality_function ("gxx");

  return gm2_eh_personality_decl;
}

/* Functions called directly by the generic backend.  */

tree
convert_loc (location_t location, tree type, tree expr)
{
  if (type == error_mark_node || expr == error_mark_node
      || TREE_TYPE (expr) == error_mark_node)
    return error_mark_node;

  if (type == TREE_TYPE (expr))
    return expr;

  gcc_assert (TYPE_MAIN_VARIANT (type) != NULL);
  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (expr)))
    return fold_convert (type, expr);

  expr = m2convert_GenericToType (location, type, expr);
  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
    case BOOLEAN_TYPE:
      return fold (convert_to_integer (type, expr));
    case INTEGER_TYPE:
      return fold (convert_to_integer (type, expr));
    case POINTER_TYPE:
      return fold (convert_to_pointer (type, expr));
    case REAL_TYPE:
      return fold (convert_to_real (type, expr));
    case COMPLEX_TYPE:
      return fold (convert_to_complex (type, expr));
    case ENUMERAL_TYPE:
      return fold (convert_to_integer (type, expr));
    default:
      error_at (location, "cannot convert expression, only base types can be converted");
      break;
    }
  return error_mark_node;
}

/* Functions called directly by the generic backend.  */

tree
convert (tree type, tree expr)
{
  return convert_loc (m2linemap_UnknownLocation (), type, expr);
}

/* Mark EXP saying that we need to be able to take the address of it;
   it should not be allocated in a register.  Returns true if
   successful.  */

bool
gm2_mark_addressable (tree exp)
{
  tree x = exp;

  while (TRUE)
    switch (TREE_CODE (x))
      {
      case COMPONENT_REF:
	if (DECL_PACKED (TREE_OPERAND (x, 1)))
	  return false;
	x = TREE_OPERAND (x, 0);
	break;

      case ADDR_EXPR:
      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
        x = TREE_OPERAND (x, 0);
        break;

      case COMPOUND_LITERAL_EXPR:
      case CONSTRUCTOR:
      case STRING_CST:
      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
      case FUNCTION_DECL:
        TREE_ADDRESSABLE (x) = 1;
        return true;
      default:
        return true;
      }
  /* Never reach here.  */
  gcc_unreachable ();
}

/* Return an integer type with BITS bits of precision, that is
   unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

tree
gm2_type_for_size (unsigned int bits, int unsignedp)
{
  if (unsignedp)
    {
      if (bits == INT_TYPE_SIZE)
        return unsigned_type_node;
      else if (bits == CHAR_TYPE_SIZE)
        return unsigned_char_type_node;
      else if (bits == SHORT_TYPE_SIZE)
        return short_unsigned_type_node;
      else if (bits == LONG_TYPE_SIZE)
        return long_unsigned_type_node;
      else if (bits == LONG_LONG_TYPE_SIZE)
        return long_long_unsigned_type_node;
      else
	return build_nonstandard_integer_type (bits,
					       unsignedp);
    }
  else
    {
      if (bits == INT_TYPE_SIZE)
        return integer_type_node;
      else if (bits == CHAR_TYPE_SIZE)
        return signed_char_type_node;
      else if (bits == SHORT_TYPE_SIZE)
        return short_integer_type_node;
      else if (bits == LONG_TYPE_SIZE)
        return long_integer_type_node;
      else if (bits == LONG_LONG_TYPE_SIZE)
        return long_long_integer_type_node;
      else
	return build_nonstandard_integer_type (bits,
					       unsignedp);
    }
  /* Never reach here.  */
  gcc_unreachable ();
}

/* Allow the analyzer to understand Storage ALLOCATE/DEALLOCATE.  */

bool
gm2_langhook_new_dispose_storage_substitution (void)
{
  return true;
}

#undef LANG_HOOKS_NAME
#undef LANG_HOOKS_INIT
#undef LANG_HOOKS_INIT_OPTIONS
#undef LANG_HOOKS_OPTION_LANG_MASK
#undef LANG_HOOKS_INIT_OPTIONS_STRUCT
#undef LANG_HOOKS_HANDLE_OPTION
#undef LANG_HOOKS_POST_OPTIONS
#undef LANG_HOOKS_PARSE_FILE
#undef LANG_HOOKS_TYPE_FOR_MODE
#undef LANG_HOOKS_TYPE_FOR_SIZE
#undef LANG_HOOKS_BUILTIN_FUNCTION
#undef LANG_HOOKS_GLOBAL_BINDINGS_P
#undef LANG_HOOKS_PUSHDECL
#undef LANG_HOOKS_GETDECLS
#undef LANG_HOOKS_GIMPLIFY_EXPR
#undef LANG_HOOKS_EH_PERSONALITY
#undef LANG_HOOKS_NEW_DISPOSE_STORAGE_SUBSTITUTION

#define LANG_HOOKS_NAME "GNU Modula-2"
#define LANG_HOOKS_INIT gm2_langhook_init
#define LANG_HOOKS_INIT_OPTIONS gm2_langhook_init_options
#define LANG_HOOKS_OPTION_LANG_MASK gm2_langhook_option_lang_mask
#define LANG_HOOKS_INIT_OPTIONS_STRUCT gm2_langhook_init_options_struct
#define LANG_HOOKS_HANDLE_OPTION gm2_langhook_handle_option
#define LANG_HOOKS_POST_OPTIONS gm2_langhook_post_options
#define LANG_HOOKS_PARSE_FILE gm2_langhook_parse_file
#define LANG_HOOKS_TYPE_FOR_MODE gm2_langhook_type_for_mode
#define LANG_HOOKS_TYPE_FOR_SIZE gm2_langhook_type_for_size
#define LANG_HOOKS_BUILTIN_FUNCTION gm2_langhook_builtin_function
#define LANG_HOOKS_GLOBAL_BINDINGS_P gm2_langhook_global_bindings_p
#define LANG_HOOKS_PUSHDECL gm2_langhook_pushdecl
#define LANG_HOOKS_GETDECLS gm2_langhook_getdecls
#define LANG_HOOKS_GIMPLIFY_EXPR gm2_langhook_gimplify_expr
#define LANG_HOOKS_EH_PERSONALITY gm2_langhook_eh_personality
#define LANG_HOOKS_NEW_DISPOSE_STORAGE_SUBSTITUTION \
  gm2_langhook_new_dispose_storage_substitution

struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#include "gt-m2-gm2-lang.h"
#include "gtype-m2.h"
