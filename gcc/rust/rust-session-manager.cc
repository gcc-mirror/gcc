// Copyright (C) 2020 Free Software Foundation, Inc.

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

#include <fstream>
#include <sstream>
#include "rust-session-manager.h"
#include "rust-diagnostics.h"
#include "diagnostic.h"
#include "input.h"

#include "target.h"
#include "tm.h"
#include "memmodel.h"
#include "tm_p.h"

//#include "rust-target.h"
/*TODO This isn't (currently?) necessary, but if '#include'd after '#include
   "target.h"', causes: In file included from
   [...]/gcc/rust/rust-session-manager.cc:31:
    [...]/gcc/rust/rust-target.h:23: error: "DEFHOOK" redefined [-Werror]
       23 | #define DEFHOOK(NAME, DOC, TYPE, PARAMS, INIT) TYPE (*NAME) PARAMS;
	  |
    In file included from [...]/gcc/rust/rust-session-manager.cc:27:
    [...]/gcc/target.h:272: note: this is the location of the previous
   definition 272 | #define DEFHOOK(NAME, DOC, TYPE, PARAMS, INIT) TYPE (* NAME)
   PARAMS;
	  |
*/

#include "rust-lex.h"
#include "rust-parse.h"
#include "rust-macro-expand.h"
#include "rust-ast-resolve.h"
#include "rust-ast-lower.h"
#include "rust-hir-type-check.h"
#include "rust-lint-scan-deadcode.h"
#include "rust-tycheck-dump.h"
#include "rust-ast-resolve-unused.h"
#include "rust-hir-const-fold.h"
#include "rust-compile.h"

extern Linemap *
rust_get_linemap ();

extern Backend *
rust_get_backend ();

namespace Rust {

const char *kLexDumpFile = "gccrs.lex.dump";
const char *kASTDumpFile = "gccrs.ast.dump";
const char *kASTExpandedDumpFile = "gccrs.ast-expanded.dump";
const char *kHIRDumpFile = "gccrs.hir.dump";
const char *kHIRTypeResolutionDumpFile = "gccrs.type-resolution.dump";
const char *kTargetOptionsDumpFile = "gccrs.target-options.dump";

// FIME in the imports/visibility milestone - this needs to be command line
// option
const std::string kDefaultCrateName = "TestCrate";

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
      if (TARGET_ISA_AES) {
	  // enable aes, implicitly enable sse2
	  implicitly_enable_feature("aes");
      }

      if (TARGET_ISA_AVX) {
	  // enable avx, implicitly enable sse4.2
	  implicitly_enable_feature("sse4.2");
      }

      if (TARGET_ISA_AVX2) {
	  // enable avx2, implicitly enable avx
	  implicitly_enable_feature("avx");
      }

      if (TARGET_ISA_BMI) {
	  // enable bmi1
	  implicitly_enable_feature("bmi1");
      }

      if (TARGET_ISA_BMI2) {
	  // enable bmi2
	  implicitly_enable_feature("bmi2");
      }

      if (TARGET_ISA_FMA) {
	  // enable fma, implicitly enable avx
	  implicitly_enable_feature("fma");
      }

      if (TARGET_ISA_FXSR) {
	  // enable fxsr
	  implicitly_enable_feature("fxsr");
      }

      if (TARGET_ISA_LZCNT) {
	  // enable lzcnt
	  implicitly_enable_feature("lzcnt");
      }

      if (TARGET_ISA_VPCLMULQDQ) {
	  // enable pclmulqdq, implicitly enable sse2
	  implicitly_enable_feature("pclmulqdq");
      }

      if (TARGET_ISA_POPCNT) {
	  // enable popcnt
	  implicitly_enable_feature("popcnt");
      }

      if (TARGET_ISA_RDRND) {
	  // enable rdrand
	  implicitly_enable_feature("rdrand");
      }

      if (TARGET_ISA_RDSEED) {
	  // enable rdseed
	  implicitly_enable_feature("rdseed");
      }

      if (TARGET_ISA_SHA) {
	  // enable sha, implicitly enable sse2
	  implicitly_enable_feature("sha");
      }

      if (TARGET_ISA_SSE) {
	  // enable sse
	  implicitly_enable_feature("sse");
      }

      if (TARGET_ISA_SSE2) {
	  // enable sse2, implicitly enable sse
	  implicitly_enable_feature("sse2");
      }

      if (TARGET_ISA_SSE3) {
	  // enable sse3, implicitly enable sse2
	  implicitly_enable_feature("sse3");
      }

      if (TARGET_ISA_SSE4_1) {
	  // enable sse4.1, implicitly enable sse3
	  implicitly_enable_feature("sse4.1");
      }

      if (TARGET_ISA_SSE4_2) {
	  // enable sse4.2, implicitly enable sse4.1
	  implicitly_enable_feature("sse4.2");
      }

      if (TARGET_ISA_SSSE3) {
	  // enable ssse3, implicitly enable sse3
	  implicitly_enable_feature("ssse3");
      }

      if (TARGET_ISA_XSAVE) {
	  // enable xsave
	  implicitly_enable_feature("xsave");
      }

      if (TARGET_ISA_XSAVEC) {
	  // enable xsavec
	  implicitly_enable_feature("xsavec");
      }

      if (TARGET_ISA_XSAVEOPT) {
	  // enable xsaveopt
	  implicitly_enable_feature("xsaveopt");
      }

      if (TARGET_ISA_XSAVES) {
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

  // the constant folder uses gcc
  ConstFold::Context::init (backend);
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
      if (arg != nullptr)
	ret = Compile::Mangler::choose_mangling (std::string (arg));
    // no option handling for -o
    default:
      // return 1 to indicate option is valid
      break;
    }

  return ret;
}

/* Enables a certain dump depending on the name passed in. Returns true if name
 * is valid, false otherwise. */
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
  auto mappings = Analysis::Mappings::get ();
  CrateNum crate_num = mappings->setup_crate_mappings (kDefaultCrateName);
  mappings->set_current_crate (crate_num);

  for (int i = 0; i < num_files; i++)
    {
      rust_debug ("Attempting to parse file: %s", files[i]);
      parse_file (files[i]);
    }
  /* TODO: should semantic analysis be dealed with here? or per file? for now,
   * per-file. */
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

  if (options.dump_option_enabled (CompileOptions::LEXER_DUMP))
    {
      dump_lex (parser);
    }
  if (options.dump_option_enabled (CompileOptions::PARSER_AST_DUMP))
    {
      dump_ast (parser, parsed_crate);
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
   *  - expansion (expands all macros, maybe build test harness, AST validation,
   * maybe macro crate)
   *  - resolution (name resolution, type resolution, maybe feature checking,
   * maybe buffered lints)
   *  TODO not done */

  rust_debug ("\033[0;31mSUCCESSFULLY PARSED CRATE \033[0m");

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

  if (saw_errors ())
    return;

  // type resolve
  Resolver::TypeResolution::Resolve (hir);
  if (options.dump_option_enabled (CompileOptions::TYPE_RESOLUTION_DUMP))
    {
      dump_type_resolution (hir);
    }

  if (saw_errors ())
    return;

  // scan dead code
  Analysis::ScanDeadcode::Scan (hir);

  if (saw_errors ())
    return;

  // scan unused has to be done after type resolution since methods are resolved
  // at that point
  Resolver::ScanUnused::Scan ();

  if (saw_errors ())
    return;

  // do compile
  Compile::Context ctx (backend);
  Compile::CompileCrate::Compile (hir, &ctx);

  if (saw_errors ())
    return;

  // pass to GCC
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
   * crate names. In these names, insert items at beginning of crate items. This
   * is crate injection. Also, inject prelude use decl at beginning (first name
   * is assumed to be prelude - prelude is a use decl automatically generated to
   * enable using Option and Copy without qualifying it or importing it via
   * 'use' manually) */

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
   * "bang" macros, "attr" macros, and "derive" macros. I think the meanings of
   * these categories should be fairly obvious to anyone who has used rust.
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
   * macros, cfg, and test should be prioritised since they seem to be used the
   * most. */

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
      AST::Attribute attr (AST::SimplePath::from_str ("macro_use"), nullptr);

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
  std::vector<AST::SimplePathSegment> segments
    = {AST::SimplePathSegment (injected_crate_name),
       AST::SimplePathSegment ("prelude"), AST::SimplePathSegment ("v1")};
  // create use tree and decl
  std::unique_ptr<AST::UseTreeGlob> use_tree (
    new AST::UseTreeGlob (AST::UseTreeGlob::PATH_PREFIXED,
			  AST::SimplePath (std::move (segments)), Location ()));
  AST::Attribute prelude_attr (AST::SimplePath::from_str ("prelude_import"),
			       nullptr);
  std::unique_ptr<AST::UseDeclaration> use_decl (
    new AST::UseDeclaration (std::move (use_tree),
			     AST::Visibility::create_error (),
			     {std::move (prelude_attr)}, Location ()));

  crate.items.insert (crate.items.begin (), std::move (use_decl));

  /* TODO: potentially add checking attribute crate type? I can't figure out
   * what this does currently comment says "Unconditionally collect crate types
   * from attributes to make them used", which presumably refers to checking the
   * linkage info by "crate_type". It also seems to ensure that an invalid crate
   * type is not specified, so maybe just do that. Valid crate types: bin lib
   * dylib staticlib cdylib rlib proc-macro */

  rust_debug ("finished injection");
}

void
Session::expansion (AST::Crate &crate)
{
  rust_debug ("started expansion");

  /* rustc has a modification to windows PATH temporarily here, which may end up
   * being required */

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
Session::dump_hir (HIR::Crate &hir) const
{
  std::ofstream out;
  out.open (kHIRDumpFile);
  if (out.fail ())
    {
      rust_error_at (Linemap::unknown_location (), "cannot open %s:%m; ignored",
		     kHIRDumpFile);
      return;
    }

  out << hir.as_string ();
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
 *  - resolve type aliases (replace any usages of type aliases with actual type
 * [except associated types])
 *  - resolve bind (iterate HIR tree and set binding annotations on all concrete
 * types [avoids path lookups later])
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
 *  - expand HIR erasedtype (replace all erased types 'impl Trait' with the true
 * type)
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
 *  - MIR validate full (optionally: perform expensive state-tracking validation
 * on MIR)
 *  - trans enumerate (enumerate all items needed for code generation, primarily
 * types used for generics)
 *  - trans auto impls (create magic trait impls as enumerated in previous pass)
 *  - trans monomorph (generate monomorphised copies of all functions [with
 * generics replaced with real types])
 *  - MIR optimise inline (run optimisation again, this time with full type info
 * [primarily for inlining])
 *  - HIR serialise (write out HIR dump [module tree and generic/inline MIR])
 *  - trans codegen (generate final output file: emit C source file and call C
 * compiler) */

/* rustc compile pipeline (basic, in way less detail):
 *  - parse input (parse .rs to AST)
 *  - name resolution, macro expansion, and configuration (process AST
 * recursively, resolving paths, expanding macros, processing #[cfg] nodes [i.e.
 * maybe stripping stuff from AST])
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
 * compiler plugins', syntax expansion, secondary 'cfg' expansion, synthesis of
 * a test harness if required, injection of any std lib dependency and prelude,
 * and name resolution) - actually documented inline
 *      - seeming pierced-together order: pre-AST expansion lint checks,
 * registering builtin macros, crate injection, then expand all macros, then
 * maybe build test harness, AST validation, maybe create a macro crate (if not
 * rustdoc), name resolution, complete gated feature checking, add all buffered
 * lints
 *  - create global context (lower to HIR)
 *  - analysis on global context (HIR optimisations? create MIR?)
 *  - code generation
 *  - link */
} // namespace Rust
