// Copyright (C) 2020-2022 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.
// #include "rust-session-manager.h"

#include "rust-session-manager.h"
#include "rust-diagnostics.h"
#include "rust-lex.h"
#include "rust-parse.h"
#include "rust-macro-expand.h"
#include "rust-ast-resolve.h"
#include "rust-ast-lower.h"
#include "rust-hir-type-check.h"
#include "rust-privacy-check.h"
#include "rust-tycheck-dump.h"
#include "rust-compile.h"
#include "rust-cfg-parser.h"
#include "rust-lint-scan-deadcode.h"
#include "rust-lint-unused-var.h"
#include "rust-hir-dump.h"
#include "rust-ast-dump.h"

#include "diagnostic.h"
#include "input.h"
#include "rust-target.h"
#include "selftest.h"

extern bool
saw_errors (void);

extern Linemap *
rust_get_linemap ();

extern Backend *
rust_get_backend ();

namespace Rust {

const char *kLexDumpFile = "gccrs.lex.dump";
const char *kASTDumpFile = "gccrs.ast.dump";
const char *kASTPrettyDumpFile = "gccrs.ast-pretty.dump";
const char *kASTExpandedDumpFile = "gccrs.ast-expanded.dump";
const char *kHIRDumpFile = "gccrs.hir.dump";
const char *kHIRPrettyDumpFile = "gccrs.hir-pretty.dump";
const char *kHIRTypeResolutionDumpFile = "gccrs.type-resolution.dump";
const char *kTargetOptionsDumpFile = "gccrs.target-options.dump";

const std::string kDefaultCrateName = "rust_out";
const size_t kMaxNameLength = 64;

static std::string
infer_crate_name (const std::string &filename)
{
  if (filename == "-")
    return kDefaultCrateName;

  std::string crate = std::string (filename);
  size_t path_sep = crate.find_last_of (file_separator);

  // find the base filename
  if (path_sep != std::string::npos)
    crate.erase (0, path_sep + 1);

  // find the file stem name (remove file extension)
  size_t ext_position = crate.find_last_of ('.');
  if (ext_position != std::string::npos)
    crate.erase (ext_position);

  // Replace all the '-' symbols with '_' per Rust rules
  for (auto &c : crate)
    {
      if (c == '-')
	c = '_';
    }
  return crate;
}

/* Validate the crate name using the ASCII rules
   TODO: Support Unicode version of the rules */

static bool
validate_crate_name (const std::string &crate_name, Error &error)
{
  if (crate_name.empty ())
    {
      error = Error (Location (), "crate name cannot be empty");
      return false;
    }
  if (crate_name.length () > kMaxNameLength)
    {
      error = Error (Location (), "crate name cannot exceed %lu characters",
		     (unsigned long) kMaxNameLength);
      return false;
    }
  for (auto &c : crate_name)
    {
      if (!(ISALNUM (c) || c == '_'))
	{
	  error = Error (Location (),
			 "invalid character %<%c%> in crate name: %<%s%>", c,
			 crate_name.c_str ());
	  return false;
	}
    }
  return true;
}

// Implicitly enable a target_feature (and recursively enable dependencies).
void
Session::implicitly_enable_feature (std::string feature_name)
{
  // TODO: is this really required since features added would be complete via
  // target spec?

  if (!options.target_data.has_key_value_pair ("target_feature", feature_name))
    {
      // if feature has dependencies, enable them
      if (feature_name == "aes")
	{
	  implicitly_enable_feature ("sse2");
	}
      else if (feature_name == "avx")
	{
	  implicitly_enable_feature ("sse4.2");
	}
      else if (feature_name == "avx2")
	{
	  implicitly_enable_feature ("avx");
	}
      else if (feature_name == "fma")
	{
	  implicitly_enable_feature ("avx");
	}
      else if (feature_name == "pclmulqdq")
	{
	  implicitly_enable_feature ("sse2");
	}
      else if (feature_name == "sha")
	{
	  implicitly_enable_feature ("sse2");
	}
      else if (feature_name == "sse2")
	{
	  implicitly_enable_feature ("sse");
	}
      else if (feature_name == "sse3")
	{
	  implicitly_enable_feature ("sse2");
	}
      else if (feature_name == "sse4.1")
	{
	  implicitly_enable_feature ("sse3");
	}
      else if (feature_name == "sse4.2")
	{
	  implicitly_enable_feature ("sse4.1");
	}
      else if (feature_name == "ssse3")
	{
	  implicitly_enable_feature ("sse3");
	}

      options.target_data.insert_key_value_pair ("target_feature",
						 std::move (feature_name));
    }
}

// Meant to enable all target features. As this will be done by target hook,
// this method's deprecated.
void
Session::enable_features ()
{
  bool has_target_crt_static = false;

  rust_debug (
    "ERROR: Somewhere in call chain Session::enable_features is called.");

  if (has_target_crt_static)
    {
      // enable "crt-static" attribute
    }

  /* TODO: do this via target hook. have one for each target that implicitly
   * enables the
   * features for that platform. Would probably have to make custom target hook.
   */

  /*
  if (target == "x86" || target == "x86_64") {
      if (TARGET_AES) {
	  // enable aes, implicitly enable sse2
	  implicitly_enable_feature("aes");
      }

      if (TARGET_AVX) {
	  // enable avx, implicitly enable sse4.2
	  implicitly_enable_feature("sse4.2");
      }

      if (TARGET_AVX2) {
	  // enable avx2, implicitly enable avx
	  implicitly_enable_feature("avx");
      }

      if (TARGET_BMI) {
	  // enable bmi1
	  implicitly_enable_feature("bmi1");
      }

      if (TARGET_BMI2) {
	  // enable bmi2
	  implicitly_enable_feature("bmi2");
      }

      if (TARGET_FMA) {
	  // enable fma, implicitly enable avx
	  implicitly_enable_feature("fma");
      }

      if (TARGET_FXSR) {
	  // enable fxsr
	  implicitly_enable_feature("fxsr");
      }

      if (TARGET_LZCNT) {
	  // enable lzcnt
	  implicitly_enable_feature("lzcnt");
      }

      if (TARGET_VPCLMULQDQ) {
	  // enable pclmulqdq, implicitly enable sse2
	  implicitly_enable_feature("pclmulqdq");
      }

      if (TARGET_POPCNT) {
	  // enable popcnt
	  implicitly_enable_feature("popcnt");
      }

      if (TARGET_RDRND) {
	  // enable rdrand
	  implicitly_enable_feature("rdrand");
      }

      if (TARGET_RDSEED) {
	  // enable rdseed
	  implicitly_enable_feature("rdseed");
      }

      if (TARGET_SHA) {
	  // enable sha, implicitly enable sse2
	  implicitly_enable_feature("sha");
      }

      if (TARGET_SSE) {
	  // enable sse
	  implicitly_enable_feature("sse");
      }

      if (TARGET_SSE2) {
	  // enable sse2, implicitly enable sse
	  implicitly_enable_feature("sse2");
      }

      if (TARGET_SSE3) {
	  // enable sse3, implicitly enable sse2
	  implicitly_enable_feature("sse3");
      }

      if (TARGET_SSE4_1) {
	  // enable sse4.1, implicitly enable sse3
	  implicitly_enable_feature("sse4.1");
      }

      if (TARGET_SSE4_2) {
	  // enable sse4.2, implicitly enable sse4.1
	  implicitly_enable_feature("sse4.2");
      }

      if (TARGET_SSSE3) {
	  // enable ssse3, implicitly enable sse3
	  implicitly_enable_feature("ssse3");
      }

      if (TARGET_XSAVE) {
	  // enable xsave
	  implicitly_enable_feature("xsave");
      }

      if (TARGET_XSAVEC) {
	  // enable xsavec
	  implicitly_enable_feature("xsavec");
      }

      if (TARGET_XSAVEOPT) {
	  // enable xsaveopt
	  implicitly_enable_feature("xsaveopt");
      }

      if (TARGET_XSAVES) {
	  // enable xsaves
	  implicitly_enable_feature("xsaves");
      }
  }
  options.target_data.features.shrink_to_fit();
  std::sort(options.target_data.features.begin(),
  options.target_data.features.end());*/
}

void
Session::init ()
{
#ifndef TARGET_RUST_OS_INFO
#define TARGET_RUST_OS_INFO()
#endif
//#define builtin_rust_info(KEY, VALUE) rust_add_target_info (KEY, VALUE)
// might as well use c++ stuff
#define builtin_rust_info(KEY, VALUE)                                          \
  options.target_data.insert_key_value_pair (KEY, VALUE)

  // initialise target hooks
  // targetrustm.rust_cpu_info();
  // targetrustm.rust_os_info();
  // ok, that's not working too well TODO - see if can salvage old
  // implementation
  TARGET_RUST_CPU_INFO ();
  TARGET_RUST_OS_INFO ();

  /* note that due to issues with gcc targets, some implementations of those two
   * macros above (TARGET_RUST_CPU_INFO and TARGET_RUST_OS_INFO) are not
   * function calls, but actually inline substitutions. As such, they can't be
   * stored with a function pointer in a "real" target hook.
   * At least, that's my current understanding of it. */

#undef builtin_rust_info

  // target-independent values that should exist in all targets
  options.target_data.insert_key_value_pair ("target_pointer_width",
					     std::to_string (POINTER_SIZE));
  options.target_data.insert_key_value_pair ("target_endian", BYTES_BIG_ENDIAN
								? "big"
								: "little");

  // TODO: find min atomic width and max atomic width
  // from it, add atomic-related stuff for sizes 8, 16, 32, 64, and 128 (if
  // inside bounds) in rustc, min atomic width is a known quantity (or 8 if not
  // known), and max is also a known quantity (or is pointer size if not known)
  // TODO: add atomic pointer if some criteria is satisfied

  // TODO: find whether target has "atomic cas"

  // add debug_assertions if enabled and proc_macro if crate type has it or
  // whatever

  // derived values from hook
  options.target_data.init_derived_values ();

  // setup singleton linemap
  linemap = rust_get_linemap ();

  // setup backend to GCC GIMPLE
  backend = rust_get_backend ();
}

/* Initialise default options. Actually called before handle_option, unlike init
 * itself. */
void
Session::init_options ()
{}

// Handle option selection.
bool
Session::handle_option (
  enum opt_code code, const char *arg, HOST_WIDE_INT value ATTRIBUTE_UNUSED,
  int kind ATTRIBUTE_UNUSED, location_t loc ATTRIBUTE_UNUSED,
  const struct cl_option_handlers *handlers ATTRIBUTE_UNUSED)
{
  // used to store whether results of various stuff are successful
  bool ret = true;

  // Handles options as listed in lang.opt.
  switch (code)
    {
    case OPT_I:
      // TODO: add search path
      break;

    case OPT_L:
      // TODO: add library link path or something
      break;

    case OPT_frust_crate_:
      // set the crate name
      if (arg != nullptr)
	{
	  auto error = Error (Location (), std::string ());
	  if ((ret = validate_crate_name (arg, error)))
	    {
	      options.set_crate_name (arg);
	      options.crate_name_set_manually = true;
	    }
	  else
	    {
	      rust_assert (!error.message.empty ());
	      error.emit_error ();
	    }
	}
      else
	ret = false;
      break;

    case OPT_frust_dump_:
      // enable dump and return whether this was successful
      if (arg != nullptr)
	{
	  ret = enable_dump (std::string (arg));
	}
      else
	{
	  ret = false;
	}
      break;

    case OPT_frust_mangling_:
      Compile::Mangler::set_mangling (flag_rust_mangling);
      break;

      case OPT_frust_cfg_: {
	auto string_arg = std::string (arg);
	ret = handle_cfg_option (string_arg);
	break;
      }

    case OPT_frust_edition_:
      options.set_edition (flag_rust_edition);
      break;

    default:
      break;
    }

  return ret;
}

bool
Session::handle_cfg_option (std::string &input)
{
  std::string key;
  std::string value;

  // Refactor this if needed
  if (!parse_cfg_option (input, key, value))
    {
      rust_error_at (
	Location (),
	"invalid argument to %<-frust-cfg%>: Accepted formats are "
	"%<-frust-cfg=key%> or %<-frust-cfg=key=\"value\"%> (quoted)");
      return false;
    }

  if (value.empty ())
    // rustc does not seem to error on dup key
    options.target_data.insert_key (key);
  else
    options.target_data.insert_key_value_pair (key, value);

  return true;
}

/* Enables a certain dump depending on the name passed in. Returns true if
 * name is valid, false otherwise. */
bool
Session::enable_dump (std::string arg)
{
  if (arg.empty ())
    {
      rust_error_at (
	Location (),
	"dump option was not given a name. choose %<lex%>, %<parse%>, "
	"%<register_plugins%>, %<injection%>, %<expansion%>, %<resolution%>,"
	" %<target_options%>, %<hir%>, or %<all%>");
      return false;
    }

  if (arg == "all")
    {
      options.enable_all_dump_options ();
    }
  else if (arg == "lex")
    {
      options.enable_dump_option (CompileOptions::LEXER_DUMP);
    }
  else if (arg == "parse")
    {
      options.enable_dump_option (CompileOptions::PARSER_AST_DUMP);
    }
  else if (arg == "ast-pretty")
    {
      options.enable_dump_option (CompileOptions::AST_DUMP_PRETTY);
    }
  else if (arg == "register_plugins")
    {
      options.enable_dump_option (CompileOptions::REGISTER_PLUGINS_DUMP);
    }
  else if (arg == "injection")
    {
      options.enable_dump_option (CompileOptions::INJECTION_DUMP);
    }
  else if (arg == "expansion")
    {
      options.enable_dump_option (CompileOptions::EXPANSION_DUMP);
    }
  else if (arg == "resolution")
    {
      options.enable_dump_option (CompileOptions::RESOLUTION_DUMP);
    }
  else if (arg == "target_options")
    {
      options.enable_dump_option (CompileOptions::TARGET_OPTION_DUMP);
    }
  else if (arg == "hir")
    {
      options.enable_dump_option (CompileOptions::HIR_DUMP);
    }
  else if (arg == "hir-pretty")
    {
      options.enable_dump_option (CompileOptions::HIR_DUMP_PRETTY);
    }
  else
    {
      rust_error_at (
	Location (),
	"dump option %qs was unrecognised. choose %<lex%>, %<parse%>, "
	"%<register_plugins%>, %<injection%>, %<expansion%>, %<resolution%>,"
	" %<target_options%>, or %<hir%>",
	arg.c_str ());
      return false;
    }
  return true;
}

/* Actual main entry point for front-end. Called from langhook to parse files.
 */
void
Session::parse_files (int num_files, const char **files)
{
  if (options.crate_name.empty ())
    {
      /* HACK: We use the first file to infer the crate name, which might be
       * incorrect: since rustc only allows one file to be supplied in the
       * command-line */
      auto filename = "-";
      if (num_files > 0)
	filename = files[0];

      auto crate_name = infer_crate_name (filename);
      rust_debug ("inferred crate name: %s", crate_name.c_str ());
      // set the preliminary crate name here
      // we will figure out the real crate name in `handle_crate_name`
      options.set_crate_name (crate_name);
    }

  auto mappings = Analysis::Mappings::get ();
  CrateNum crate_num = mappings->setup_crate_mappings (options.crate_name);
  mappings->set_current_crate (crate_num);

  for (int i = 0; i < num_files; i++)
    {
      rust_debug ("Attempting to parse file: %s", files[i]);
      parse_file (files[i]);
    }
  /* TODO: should semantic analysis be dealed with here? or per file? for now,
   * per-file. */
}

void
Session::handle_crate_name (AST::Crate parsed_crate)
{
  auto mappings = Analysis::Mappings::get ();
  auto crate_name_changed = false;
  auto error = Error (Location (), std::string ());

  for (const auto &attr : parsed_crate.inner_attrs)
    {
      if (attr.get_path () != "crate_name")
	continue;
      if (!attr.has_attr_input ())
	{
	  rust_error_at (attr.get_locus (),
			 "%<crate_name%> accepts one argument");
	  continue;
	}

      auto &literal
	= static_cast<AST::AttrInputLiteral &> (attr.get_attr_input ());
      const auto &msg_str = literal.get_literal ().as_string ();
      if (!validate_crate_name (msg_str, error))
	{
	  error.locus = attr.get_locus ();
	  error.emit_error ();
	  continue;
	}

      auto options = Session::get_instance ().options;
      if (options.crate_name_set_manually && (options.crate_name != msg_str))
	{
	  rust_error_at (attr.get_locus (),
			 "%<-frust-crate-name%> and %<#[crate_name]%> are "
			 "required to match, but %qs does not match %qs",
			 options.crate_name.c_str (), msg_str.c_str ());
	}
      crate_name_changed = true;
      options.set_crate_name (msg_str);
      mappings->set_crate_name (mappings->get_current_crate (), msg_str);
    }

  options.crate_name_set_manually |= crate_name_changed;
  if (!options.crate_name_set_manually
      && !validate_crate_name (options.crate_name, error))
    {
      error.emit_error ();
      rust_inform (linemap->get_location (0),
		   "crate name inferred from this file");
    }
}

// Parses a single file with filename filename.
void
Session::parse_file (const char *filename)
{
  RAIIFile file_wrap (filename);

  if (file_wrap.get_raw () == nullptr)
    {
      rust_fatal_error (Location (), "cannot open filename %s: %m", filename);
    }

  // parse file here
  /* create lexer and parser - these are file-specific and so aren't instance
   * variables */
  Lexer lex (filename, std::move (file_wrap), linemap);
  Parser<Lexer> parser (std::move (lex));

  // generate crate from parser
  auto parsed_crate = parser.parse_crate ();

  // setup the mappings for this AST
  auto mappings = Analysis::Mappings::get ();
  mappings->insert_ast_crate (&parsed_crate);

  // handle crate name
  handle_crate_name (parsed_crate);

  if (options.dump_option_enabled (CompileOptions::LEXER_DUMP))
    {
      dump_lex (parser);
    }
  if (options.dump_option_enabled (CompileOptions::PARSER_AST_DUMP))
    {
      dump_ast (parser, parsed_crate);
    }
  if (options.dump_option_enabled (CompileOptions::AST_DUMP_PRETTY))
    {
      dump_ast_pretty (parsed_crate);
    }
  if (options.dump_option_enabled (CompileOptions::TARGET_OPTION_DUMP))
    {
      options.target_data.dump_target_options ();
    }

  if (saw_errors ())
    return;

  /* basic pipeline:
   *  - lex
   *  - parse
   *  - register plugins (dummy stage for now) - attribute injection? what is
   * this? (attribute injection is injecting attributes specified in command
   * line into crate root)
   *  - injection (some lint checks or dummy, register builtin macros, crate
   * injection)
   *  - expansion (expands all macros, maybe build test harness, AST
   * validation, maybe macro crate)
   *  - resolution (name resolution, type resolution, maybe feature checking,
   * maybe buffered lints)
   *  TODO not done */

  rust_debug ("\033[0;31mSUCCESSFULLY PARSED CRATE \033[0m");

  // If -fsyntax-only was passed, we can just skip the remaining passes.
  // Parsing errors are already emitted in `parse_crate()`
  if (flag_syntax_only)
    return;

  // register plugins pipeline stage
  register_plugins (parsed_crate);
  rust_debug ("\033[0;31mSUCCESSFULLY REGISTERED PLUGINS \033[0m");
  if (options.dump_option_enabled (CompileOptions::REGISTER_PLUGINS_DUMP))
    {
      // TODO: what do I dump here?
    }

  // injection pipeline stage
  injection (parsed_crate);
  rust_debug ("\033[0;31mSUCCESSFULLY FINISHED INJECTION \033[0m");
  if (options.dump_option_enabled (CompileOptions::INJECTION_DUMP))
    {
      // TODO: what do I dump here? injected crate names?
    }

  // expansion pipeline stage
  expansion (parsed_crate);
  rust_debug ("\033[0;31mSUCCESSFULLY FINISHED EXPANSION \033[0m");
  if (options.dump_option_enabled (CompileOptions::EXPANSION_DUMP))
    {
      // dump AST with expanded stuff
      rust_debug ("BEGIN POST-EXPANSION AST DUMP");
      dump_ast_expanded (parser, parsed_crate);
      rust_debug ("END POST-EXPANSION AST DUMP");
    }

  // resolution pipeline stage
  Resolver::NameResolution::Resolve (parsed_crate);
  if (options.dump_option_enabled (CompileOptions::RESOLUTION_DUMP))
    {
      // TODO: what do I dump here? resolved names? AST with resolved names?
    }

  if (saw_errors ())
    return;

  // lower AST to HIR
  HIR::Crate hir = HIR::ASTLowering::Resolve (parsed_crate);
  if (options.dump_option_enabled (CompileOptions::HIR_DUMP))
    {
      dump_hir (hir);
    }

  if (options.dump_option_enabled (CompileOptions::HIR_DUMP_PRETTY))
    {
      dump_hir_pretty (hir);
    }

  if (saw_errors ())
    return;

  // add the mappings to it
  mappings->insert_hir_crate (&hir);

  // type resolve
  Resolver::TypeResolution::Resolve (hir);
  if (options.dump_option_enabled (CompileOptions::TYPE_RESOLUTION_DUMP))
    {
      dump_type_resolution (hir);
    }

  if (saw_errors ())
    return;

  // privacy pass
  Privacy::Resolver::resolve (hir);

  // do compile to gcc generic
  Compile::Context ctx (backend);
  Compile::CompileCrate::Compile (hir, &ctx);

  // we can't do static analysis if there are errors to worry about
  if (!saw_errors ())
    {
      Analysis::ScanDeadcode::Scan (hir);
      Analysis::UnusedVariables::Lint (ctx);
    }

  // pass to GCC middle-end
  ctx.write_to_backend ();
}

// TODO: actually implement method
void
load_extern_crate (std::string crate_name ATTRIBUTE_UNUSED)
{}
// TODO: deprecated - don't use

// Parses up to the "load (external) crates" part of the frontend.
// TODO: lots of this code is probably actually useful outside of dumping, so
// maybe split off function
void
Session::debug_dump_load_crates (Parser<Lexer> &parser)
{
  // parse crate as AST
  AST::Crate crate = parser.parse_crate ();

  /* TODO: search through inner attrs and see whether any of those attr paths
   * contain "no_core", "no_std", "compiler_builtins". If so/not, save certain
   * crate names. In these names, insert items at beginning of crate items.
   * This is crate injection. Also, inject prelude use decl at beginning
   * (first name is assumed to be prelude - prelude is a use decl
   * automatically generated to enable using Option and Copy without
   * qualifying it or importing it via 'use' manually) */

  std::vector<std::string> crate_names;
  for (const auto &item : crate.items)
    {
      // if item is extern crate, add name? to list of stuff ONLY IF config is
      // checked if item is module, iterate this loop inside it as well
      // (recursive?) ONLY IF config is checked

      // TODO: actually do the checks somewhere - probably in the items

      item->add_crate_name (crate_names);
    }

  /* loop through list of crate names/paths/whatever, attempting to load each
   * one. save loaded crates to a Session variable? Or save to current
   * AST::Crate? */
  for (const auto &name : crate_names)
    {
      load_extern_crate (name /*, basename = ""?*/);
    }
  //  for each loaded crate, load dependencies of it as well
}
// TODO: deprecated - don't use

void
Session::register_plugins (AST::Crate &crate ATTRIBUTE_UNUSED)
{
  rust_debug ("ran register_plugins (with no body)");
}

// TODO: move somewhere else
bool
contains_name (const AST::AttrVec &attrs, std::string name)
{
  for (const auto &attr : attrs)
    {
      if (attr.get_path () == name)
	return true;
    }

  return false;
}

void
Session::injection (AST::Crate &crate)
{
  rust_debug ("started injection");

  // lint checks in future maybe?

  // register builtin macros
  /* In rustc, builtin macros are divided into 3 categories depending on use -
   * "bang" macros, "attr" macros, and "derive" macros. I think the meanings
   * of these categories should be fairly obvious to anyone who has used rust.
   * Builtin macro list by category: Bang
   *      - asm
   *      - assert
   *      - cfg
   *      - column
   *      - compile_error
   *      - concat_idents
   *      - concat
   *      - env
   *      - file
   *      - format_args_nl
   *      - format_args
   *      - global_asm
   *      - include_bytes
   *      - include_str
   *      - include
   *      - line
   *      - log_syntax
   *      - module_path
   *      - option_env
   *      - stringify
   *      - trace_macros
   *  Attr
   *      - bench
   *      - global_allocator
   *      - test
   *      - test_case
   *  Derive
   *      - Clone
   *      - Copy
   *      - Debug
   *      - Default
   *      - Eq
   *      - Hash
   *      - Ord
   *      - PartialEq
   *      - PartialOrd
   *      - RustcDecodable
   *      - RustcEncodable
   * rustc also has a "quote" macro that is defined differently and is
   * supposedly not stable so eh. */
  /* TODO: actually implement injection of these macros. In particular, derive
   * macros, cfg, and test should be prioritised since they seem to be used
   * the most. */

  // crate injection
  std::vector<std::string> names;
  if (contains_name (crate.inner_attrs, "no_core"))
    {
      // no prelude
      injected_crate_name = "";
    }
  else if (contains_name (crate.inner_attrs, "no_std"))
    {
      names.push_back ("core");

      if (!contains_name (crate.inner_attrs, "compiler_builtins"))
	{
	  names.push_back ("compiler_builtins");
	}

      injected_crate_name = "core";
    }
  else
    {
      names.push_back ("std");

      injected_crate_name = "std";
    }

  // reverse iterate through names to insert crate items in "forward" order at
  // beginning of crate
  for (auto it = names.rbegin (); it != names.rend (); ++it)
    {
      // create "macro use" attribute for use on extern crate item to enable
      // loading macros from it
      AST::Attribute attr (AST::SimplePath::from_str ("macro_use", Location ()),
			   nullptr);

      // create "extern crate" item with the name
      std::unique_ptr<AST::ExternCrate> extern_crate (
	new AST::ExternCrate (*it, AST::Visibility::create_error (),
			      {std::move (attr)},
			      Linemap::unknown_location ()));

      // insert at beginning
      crate.items.insert (crate.items.begin (), std::move (extern_crate));
    }

  // create use tree path
  // prelude is injected_crate_name
  // FIXME: Once we do want to include the standard library, add the prelude
  // use item
  // std::vector<AST::SimplePathSegment> segments
  //   = {AST::SimplePathSegment (injected_crate_name, Location ()),
  //      AST::SimplePathSegment ("prelude", Location ()),
  //      AST::SimplePathSegment ("v1", Location ())};
  // // create use tree and decl
  // std::unique_ptr<AST::UseTreeGlob> use_tree (
  //   new AST::UseTreeGlob (AST::UseTreeGlob::PATH_PREFIXED,
  //     		  AST::SimplePath (std::move (segments)), Location ()));
  // AST::Attribute prelude_attr (AST::SimplePath::from_str ("prelude_import",
  //     						  Location ()),
  //     		       nullptr);
  // std::unique_ptr<AST::UseDeclaration> use_decl (
  //   new AST::UseDeclaration (std::move (use_tree),
  //     		     AST::Visibility::create_error (),
  //     		     {std::move (prelude_attr)}, Location ()));

  // crate.items.insert (crate.items.begin (), std::move (use_decl));

  /* TODO: potentially add checking attribute crate type? I can't figure out
   * what this does currently comment says "Unconditionally collect crate
   * types from attributes to make them used", which presumably refers to
   * checking the linkage info by "crate_type". It also seems to ensure that
   * an invalid crate type is not specified, so maybe just do that. Valid
   * crate types: bin lib dylib staticlib cdylib rlib proc-macro */

  rust_debug ("finished injection");
}

void
Session::expansion (AST::Crate &crate)
{
  rust_debug ("started expansion");

  /* rustc has a modification to windows PATH temporarily here, which may end
   * up being required */

  // create macro expansion config?
  // if not, would at least have to configure recursion_limit
  ExpansionCfg cfg;

  // create extctxt? from parse session, cfg, and resolver?
  /* expand by calling cxtctxt object's monotonic_expander's expand_crate
   * method. */
  MacroExpander expander (crate, cfg, *this);
  expander.expand_crate ();

  // error reporting - check unused macros, get missing fragment specifiers

  // build test harness

  // ast validation (also with proc macro decls)

  // maybe create macro crate if not rustdoc

  rust_debug ("finished expansion");
}

void
Session::dump_lex (Parser<Lexer> &parser) const
{
  std::ofstream out;
  out.open (kLexDumpFile);
  if (out.fail ())
    {
      rust_error_at (Linemap::unknown_location (), "cannot open %s:%m; ignored",
		     kLexDumpFile);
      return;
    }

  // TODO: rewrite lexer dump or something so that it allows for the crate
  // to already be parsed
  parser.debug_dump_lex_output (out);
  out.close ();
}

void
Session::dump_ast (Parser<Lexer> &parser, AST::Crate &crate) const
{
  std::ofstream out;
  out.open (kASTDumpFile);
  if (out.fail ())
    {
      rust_error_at (Linemap::unknown_location (), "cannot open %s:%m; ignored",
		     kASTDumpFile);
      return;
    }

  parser.debug_dump_ast_output (crate, out);
  out.close ();
}

void
Session::dump_ast_pretty (AST::Crate &crate) const
{
  std::ofstream out;
  out.open (kASTPrettyDumpFile);
  if (out.fail ())
    {
      rust_error_at (Linemap::unknown_location (), "cannot open %s:%m; ignored",
		     kASTDumpFile);
      return;
    }

  AST::Dump (out).go (crate);

  out.close ();
}

void
Session::dump_ast_expanded (Parser<Lexer> &parser, AST::Crate &crate) const
{
  std::ofstream out;
  out.open (kASTExpandedDumpFile);
  if (out.fail ())
    {
      rust_error_at (Linemap::unknown_location (), "cannot open %s:%m; ignored",
		     kASTExpandedDumpFile);
      return;
    }

  parser.debug_dump_ast_output (crate, out);
  out.close ();
}

void
Session::dump_hir (HIR::Crate &crate) const
{
  std::ofstream out;
  out.open (kHIRDumpFile);
  if (out.fail ())
    {
      rust_error_at (Linemap::unknown_location (), "cannot open %s:%m; ignored",
		     kHIRDumpFile);
      return;
    }

  out << crate.as_string ();
  out.close ();
}

void
Session::dump_hir_pretty (HIR::Crate &crate) const
{
  std::ofstream out;
  out.open (kHIRPrettyDumpFile);
  if (out.fail ())
    {
      rust_error_at (Linemap::unknown_location (), "cannot open %s:%m; ignored",
		     kHIRPrettyDumpFile);
      return;
    }

  HIR::Dump (out).go (crate);
  out.close ();
}

void
Session::dump_type_resolution (HIR::Crate &hir) const
{
  std::ofstream out;
  out.open (kHIRTypeResolutionDumpFile);
  if (out.fail ())
    {
      rust_error_at (Linemap::unknown_location (), "cannot open %s:%m; ignored",
		     kHIRTypeResolutionDumpFile);
      return;
    }

  Resolver::TypeResolverDump::go (hir, out);
  out.close ();
}

void
TargetOptions::dump_target_options () const
{
  std::ofstream out;
  out.open (kTargetOptionsDumpFile);
  if (out.fail ())
    {
      rust_error_at (Linemap::unknown_location (), "cannot open %s:%m; ignored",
		     kTargetOptionsDumpFile);
      return;
    }

  if (features.empty ())
    {
      out << "No target options available!\n";
    }

  for (const auto &pairs : features)
    {
      for (const auto &value : pairs.second)
	out << pairs.first + ": \"" + value + "\"\n";

      if (pairs.second.empty ())
	out << pairs.first + "\n";
    }

  out.close ();
}

void
TargetOptions::init_derived_values ()
{
  // enable derived values based on target families
  if (has_key_value_pair ("target_family", "unix"))
    insert_key ("unix");
  if (has_key_value_pair ("target_family", "windows"))
    insert_key ("windows");

  // implicitly enable features - this should not be required in general
  if (has_key_value_pair ("target_feature", "aes"))
    enable_implicit_feature_reqs ("aes");
  if (has_key_value_pair ("target_feature", "avx"))
    enable_implicit_feature_reqs ("sse4.2");
  if (has_key_value_pair ("target_feature", "avx2"))
    enable_implicit_feature_reqs ("avx");
  if (has_key_value_pair ("target_feature", "pclmulqdq"))
    enable_implicit_feature_reqs ("sse2");
  if (has_key_value_pair ("target_feature", "sha"))
    enable_implicit_feature_reqs ("sse2");
  if (has_key_value_pair ("target_feature", "sse2"))
    enable_implicit_feature_reqs ("sse");
  if (has_key_value_pair ("target_feature", "sse3"))
    enable_implicit_feature_reqs ("sse2");
  if (has_key_value_pair ("target_feature", "sse4.1"))
    enable_implicit_feature_reqs ("sse3");
  if (has_key_value_pair ("target_feature", "sse4.2"))
    enable_implicit_feature_reqs ("sse4.1");
  if (has_key_value_pair ("target_feature", "ssse3"))
    enable_implicit_feature_reqs ("sse3");
}

void
TargetOptions::enable_implicit_feature_reqs (std::string feature)
{
  if (feature == "aes")
    enable_implicit_feature_reqs ("sse2");
  else if (feature == "avx")
    enable_implicit_feature_reqs ("sse4.2");
  else if (feature == "avx2")
    enable_implicit_feature_reqs ("avx");
  else if (feature == "fma")
    enable_implicit_feature_reqs ("avx");
  else if (feature == "pclmulqdq")
    enable_implicit_feature_reqs ("sse2");
  else if (feature == "sha")
    enable_implicit_feature_reqs ("sse2");
  else if (feature == "sse2")
    enable_implicit_feature_reqs ("sse");
  else if (feature == "sse3")
    enable_implicit_feature_reqs ("sse2");
  else if (feature == "sse4.1")
    enable_implicit_feature_reqs ("sse3");
  else if (feature == "sse4.2")
    enable_implicit_feature_reqs ("sse4.1");
  else if (feature == "ssse3")
    enable_implicit_feature_reqs ("sse3");

  if (!has_key_value_pair ("target_feature", feature))
    {
      insert_key_value_pair ("target_feature", feature);

      rust_debug ("had to implicitly enable feature '%s'!", feature.c_str ());
    }
}

// NOTEs:
/* mrustc compile pipeline:
 *  - target load (pass target spec to parser?)
 *  - parse (convert source to AST)
 *  - load crates (load any explicitly mentioned extern crates [not all of
 * them])
 *  - expand (AST transformations from attributes and macros, loads remaining
 * extern crates [std/core and any triggered by macro expansion])
 *  - implicit crates (test harness, allocator crate, panic crate)
 *  - resolve use (annotate every 'use' item with source [supposedly handles
 * nasty recursion])
 *  - resolve index (generate index of visible items for every module [avoids
 * recursion in next pass])
 *  - resolve absolute (resolve all paths into either variable names
 * [types/values] or absolute paths)
 *  - HIR lower (convert modified AST to simpler HIR [both expressions and
 * module tree])
 *  - resolve type aliases (replace any usages of type aliases with actual
 * type [except associated types])
 *  - resolve bind (iterate HIR tree and set binding annotations on all
 * concrete types [avoids path lookups later])
 *  - resolve HIR markings (generate "markings" [e.g. for Copy/Send/Sync/...]
 * for all types
 *  - sort impls (small pass - sort impls into groups)
 *  - resolve UFCS outer (determine source trait for all top-level <T>::Type
 * [qualified] paths)
 *  - resolve UFCS paths (do the same, but include for exprs this time. also
 * normalises results of previous pass [expanding known associated types])
 *  - constant evaluate (evaluate all constants)
 *  - typecheck outer (checks impls are sane)
 *  - typecheck expressions (resolve and check types for all exprs)
 *  - expand HIR annotate (annotate how exprs are used - used for closure
 * extractions and reborrows)
 *  - expand HIR closures (extract closures into structs implementing Fn*
 * traits)
 *  - expand HIR vtables (generate vtables for types with dyn dispatch)
 *  - expand HIR calls (converts method and callable calls into explicit
 * function calls)
 *  - expand HIR reborrows (apply reborrow rules [taking '&mut *v' instead of
 * 'v'])
 *  - expand HIR erasedtype (replace all erased types 'impl Trait' with the
 * true type)
 *  - typecheck expressions (validate - double check that previous passes
 * haven't broke type system rules)
 *  - lower MIR (convert HIR exprs into a control-flow graph [MIR])
 *  - MIR validate (check that the generated MIR is consistent)
 *  - MIR cleanup (perform various transformations on MIR - replace reads of
 * const items with the item itself; convert casts to unsized types into
 * 'MakeDst' operations)
 *  - MIR optimise (perform various simple optimisations on the MIR - constant
 * propagation, dead code elimination, borrow elimination, some inlining)
 *  - MIR validate PO (re-validate the MIR)
 *  - MIR validate full (optionally: perform expensive state-tracking
 * validation on MIR)
 *  - trans enumerate (enumerate all items needed for code generation,
 * primarily types used for generics)
 *  - trans auto impls (create magic trait impls as enumerated in previous
 * pass)
 *  - trans monomorph (generate monomorphised copies of all functions [with
 * generics replaced with real types])
 *  - MIR optimise inline (run optimisation again, this time with full type
 * info [primarily for inlining])
 *  - HIR serialise (write out HIR dump [module tree and generic/inline MIR])
 *  - trans codegen (generate final output file: emit C source file and call C
 * compiler) */

/* rustc compile pipeline (basic, in way less detail):
 *  - parse input (parse .rs to AST)
 *  - name resolution, macro expansion, and configuration (process AST
 * recursively, resolving paths, expanding macros, processing #[cfg] nodes
 * [i.e. maybe stripping stuff from AST])
 *  - lower to HIR
 *  - type check and other analyses (e.g. privacy checking)
 *  - lower to MIR and post-processing (and do stuff like borrow checking)
 *  - translation to LLVM IR and LLVM optimisations (produce the .o files)
 *  - linking (link together .o files) */

/* Pierced-together rustc compile pipeline (from source):
 *  - parse input (parse file to crate)
 *  - register plugins (attributes injection, set various options, register
 * lints, load plugins)
 *  - expansion/configure and expand (initial 'cfg' processing, 'loading
 * compiler plugins', syntax expansion, secondary 'cfg' expansion, synthesis
 * of a test harness if required, injection of any std lib dependency and
 * prelude, and name resolution) - actually documented inline
 *      - seeming pierced-together order: pre-AST expansion lint checks,
 * registering builtin macros, crate injection, then expand all macros, then
 * maybe build test harness, AST validation, maybe create a macro crate (if
 * not rustdoc), name resolution, complete gated feature checking, add all
 * buffered lints
 *  - create global context (lower to HIR)
 *  - analysis on global context (HIR optimisations? create MIR?)
 *  - code generation
 *  - link */
} // namespace Rust

#if CHECKING_P
namespace selftest {
void
rust_crate_name_validation_test (void)
{
  auto error = Rust::Error (Location (), std::string ());
  ASSERT_TRUE (Rust::validate_crate_name ("example", error));
  ASSERT_TRUE (Rust::validate_crate_name ("abcdefg_1234", error));
  ASSERT_TRUE (Rust::validate_crate_name ("1", error));
  // FIXME: The next test does not pass as of current implementation
  // ASSERT_TRUE (Rust::CompileOptions::validate_crate_name ("惊吓"));
  // NOTE: - is not allowed in the crate name ...

  ASSERT_FALSE (Rust::validate_crate_name ("abcdefg-1234", error));
  ASSERT_FALSE (Rust::validate_crate_name ("a+b", error));
  ASSERT_FALSE (Rust::validate_crate_name ("/a+b/", error));

  /* Tests for crate name inference */
  ASSERT_EQ (Rust::infer_crate_name ("c.rs"), "c");
  // NOTE: ... but - is allowed when in the filename
  ASSERT_EQ (Rust::infer_crate_name ("a-b.rs"), "a_b");
  ASSERT_EQ (Rust::infer_crate_name ("book.rs.txt"), "book.rs");
#if defined(HAVE_DOS_BASED_FILE_SYSTEM)
  ASSERT_EQ (Rust::infer_crate_name ("a\\c\\a-b.rs"), "a_b");
#else
  ASSERT_EQ (Rust::infer_crate_name ("a/c/a-b.rs"), "a_b");
#endif
}
} // namespace selftest
#endif // CHECKING_P
