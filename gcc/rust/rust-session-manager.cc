// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#include "rust-session-manager.h"
#include "rust-diagnostics.h"
#include "rust-unsafe-checker.h"
#include "rust-lex.h"
#include "rust-parse.h"
#include "rust-macro-expand.h"
#include "rust-ast-resolve.h"
#include "rust-ast-lower.h"
#include "rust-hir-type-check.h"
#include "rust-privacy-check.h"
#include "rust-const-checker.h"
#include "rust-feature-gate.h"
#include "rust-tycheck-dump.h"
#include "rust-compile.h"
#include "rust-cfg-parser.h"
#include "rust-lint-scan-deadcode.h"
#include "rust-lint-unused-var.h"
#include "rust-hir-dump.h"
#include "rust-ast-dump.h"
#include "rust-export-metadata.h"
#include "rust-imports.h"
#include "rust-extern-crate.h"
#include "rust-attributes.h"
#include "rust-early-name-resolver.h"

#include "diagnostic.h"
#include "input.h"
#include "selftest.h"
#include "target.h"

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
const char *kASTPrettyDumpFileExpanded = "gccrs.ast-pretty-expanded.dump";
const char *kASTExpandedDumpFile = "gccrs.ast-expanded.dump";
const char *kHIRDumpFile = "gccrs.hir.dump";
const char *kHIRPrettyDumpFile = "gccrs.hir-pretty.dump";
const char *kHIRTypeResolutionDumpFile = "gccrs.type-resolution.dump";
const char *kTargetOptionsDumpFile = "gccrs.target-options.dump";

const std::string kDefaultCrateName = "rust_out";
const size_t kMaxNameLength = 64;

Session &
Session::get_instance ()
{
  static Session instance;
  return instance;
}

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

void
Session::init ()
{
  options.target_data.insert_key_value_pair ("target_pointer_width",
					     std::to_string (POINTER_SIZE));
  options.target_data.insert_key_value_pair ("target_endian", BYTES_BIG_ENDIAN
								? "big"
								: "little");

  // setup singleton linemap
  linemap = rust_get_linemap ();

  // setup backend to GCC GIMPLE
  backend = rust_get_backend ();

  // setup mappings class
  mappings = Analysis::Mappings::get ();
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
      case OPT_L: {
	// TODO: add search path
	const std::string p = std::string (arg);
	add_search_path (p);
      }
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
	      error.emit ();
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
    case OPT_frust_compile_until_:
      options.set_compile_step (flag_rust_compile_until);
      break;
    case OPT_frust_metadata_output_:
      options.set_metadata_output (arg);
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
Session::handle_input_files (int num_files, const char **files)
{
  if (num_files != 1)
    rust_fatal_error (Location (),
		      "only one file may be specified on the command line");

  const auto &file = files[0];

  if (options.crate_name.empty ())
    {
      auto filename = "-";
      if (num_files > 0)
	filename = files[0];

      auto crate_name = infer_crate_name (filename);
      rust_debug ("inferred crate name: %s", crate_name.c_str ());
      // set the preliminary crate name here
      // we will figure out the real crate name in `handle_crate_name`
      options.set_crate_name (crate_name);
    }

  CrateNum crate_num = mappings->get_next_crate_num (options.get_crate_name ());
  mappings->set_current_crate (crate_num);

  rust_debug ("Attempting to parse file: %s", file);
  compile_crate (file);
}

void
Session::handle_crate_name (const AST::Crate &parsed_crate)
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
	  error.emit ();
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
      error.emit ();
      rust_inform (linemap->get_location (0),
		   "crate name inferred from this file");
    }
}

// Parses a single file with filename filename.
void
Session::compile_crate (const char *filename)
{
  if (!flag_rust_experimental
      && !std::getenv ("GCCRS_INCOMPLETE_AND_EXPERIMENTAL_COMPILER_DO_NOT_USE"))
    rust_fatal_error (
      Location (), "%s",
      "gccrs is not yet able to compile Rust code "
      "properly. Most of the errors produced will be gccrs' fault and not the "
      "crate you are trying to compile. Because of this, please reports issues "
      "to us directly instead of opening issues on said crate's "
      "repository.\n\nOur github repository: "
      "https://github.com/rust-gcc/gccrs\nOur bugzilla tracker: "
      "https://gcc.gnu.org/bugzilla/"
      "buglist.cgi?bug_status=__open__&component=rust&product=gcc\n\n"
      "If you understand this, and understand that the binaries produced might "
      "not behave accordingly, you may attempt to use gccrs in an experimental "
      "manner by passing the following flag:\n\n"
      "`-frust-incomplete-and-experimental-compiler-do-not-use`\n\nor by "
      "defining the following environment variable (any value will "
      "do)\n\nGCCRS_INCOMPLETE_AND_EXPERIMENTAL_COMPILER_DO_NOT_USE\n\nFor "
      "cargo-gccrs, this means passing\n\n"
      "GCCRS_EXTRA_ARGS=\"-frust-incomplete-and-experimental-compiler-do-not-"
      "use\"\n\nas an environment variable.");

  RAIIFile file_wrap (filename);
  if (!file_wrap.ok ())
    {
      rust_error_at (Location (), "cannot open filename %s: %m", filename);
      return;
    }

  auto last_step = options.get_compile_until ();

  // parse file here
  /* create lexer and parser - these are file-specific and so aren't instance
   * variables */
  Optional<std::ofstream &> dump_lex_opt = Optional<std::ofstream &>::none ();
  std::ofstream dump_lex_stream;
  if (options.dump_option_enabled (CompileOptions::LEXER_DUMP))
    {
      dump_lex_stream.open (kLexDumpFile);
      if (dump_lex_stream.fail ())
	{
	  rust_error_at (Linemap::unknown_location (),
			 "cannot open %s:%m; ignored", kLexDumpFile);
	}
      auto stream = Optional<std::ofstream &>::some (dump_lex_stream);
      dump_lex_opt = std::move (stream);
    }

  Lexer lex (filename, std::move (file_wrap), linemap, dump_lex_opt);

  Parser<Lexer> parser (lex);

  // generate crate from parser
  std::unique_ptr<AST::Crate> ast_crate = parser.parse_crate ();

  // handle crate name
  handle_crate_name (*ast_crate.get ());

  // dump options except lexer dump
  if (options.dump_option_enabled (CompileOptions::PARSER_AST_DUMP))
    {
      dump_ast (parser, *ast_crate.get ());
    }
  if (options.dump_option_enabled (CompileOptions::AST_DUMP_PRETTY))
    {
      dump_ast_pretty (*ast_crate.get ());
    }
  if (options.dump_option_enabled (CompileOptions::TARGET_OPTION_DUMP))
    {
      options.target_data.dump_target_options ();
    }

  if (saw_errors ())
    return;

  // setup the mappings for this AST
  CrateNum current_crate = mappings->get_current_crate ();
  AST::Crate &parsed_crate
    = mappings->insert_ast_crate (std::move (ast_crate), current_crate);

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
  if (flag_syntax_only || last_step == CompileOptions::CompileStep::Ast)
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

  if (last_step == CompileOptions::CompileStep::AttributeCheck)
    return;

  Analysis::AttributeChecker ().go (parsed_crate);

  if (last_step == CompileOptions::CompileStep::Expansion)
    return;

  // expansion pipeline stage
  expansion (parsed_crate);
  rust_debug ("\033[0;31mSUCCESSFULLY FINISHED EXPANSION \033[0m");
  if (options.dump_option_enabled (CompileOptions::EXPANSION_DUMP))
    {
      // dump AST with expanded stuff
      rust_debug ("BEGIN POST-EXPANSION AST DUMP");
      dump_ast_expanded (parser, parsed_crate);
      dump_ast_pretty (parsed_crate, true);
      rust_debug ("END POST-EXPANSION AST DUMP");
    }

  // feature gating
  FeatureGate ().check (parsed_crate);

  if (last_step == CompileOptions::CompileStep::NameResolution)
    return;

  // resolution pipeline stage
  Resolver::NameResolution::Resolve (parsed_crate);
  if (options.dump_option_enabled (CompileOptions::RESOLUTION_DUMP))
    {
      // TODO: what do I dump here? resolved names? AST with resolved names?
    }

  if (saw_errors ())
    return;

  if (last_step == CompileOptions::CompileStep::Lowering)
    return;

  // lower AST to HIR
  std::unique_ptr<HIR::Crate> lowered
    = HIR::ASTLowering::Resolve (parsed_crate);
  if (saw_errors ())
    return;

  // add the mappings to it
  HIR::Crate &hir = mappings->insert_hir_crate (std::move (lowered));
  if (options.dump_option_enabled (CompileOptions::HIR_DUMP))
    {
      dump_hir (hir);
    }
  if (options.dump_option_enabled (CompileOptions::HIR_DUMP_PRETTY))
    {
      dump_hir_pretty (hir);
    }

  if (last_step == CompileOptions::CompileStep::TypeCheck)
    return;

  // type resolve
  Resolver::TypeResolution::Resolve (hir);
  if (options.dump_option_enabled (CompileOptions::TYPE_RESOLUTION_DUMP))
    {
      dump_type_resolution (hir);
    }

  if (saw_errors ())
    return;

  if (last_step == CompileOptions::CompileStep::Privacy)
    return;

  // Various HIR error passes. The privacy pass happens before the unsafe checks
  Privacy::Resolver::resolve (hir);
  if (saw_errors ())
    return;

  if (last_step == CompileOptions::CompileStep::Unsafety)
    return;

  HIR::UnsafeChecker ().go (hir);

  if (last_step == CompileOptions::CompileStep::Const)
    return;

  HIR::ConstChecker ().go (hir);

  if (saw_errors ())
    return;

  if (last_step == CompileOptions::CompileStep::Compilation)
    return;

  // do compile to gcc generic
  Compile::Context ctx (backend);
  Compile::CompileCrate::Compile (hir, &ctx);

  // we can't do static analysis if there are errors to worry about
  if (!saw_errors ())
    {
      // lints
      Analysis::ScanDeadcode::Scan (hir);
      Analysis::UnusedVariables::Lint (ctx);

      // metadata
      bool specified_emit_metadata
	= flag_rust_embed_metadata || options.metadata_output_path_set ();
      if (!specified_emit_metadata)
	{
	  Metadata::PublicInterface::ExportTo (
	    hir, Metadata::PublicInterface::expected_metadata_filename ());
	}
      else
	{
	  if (flag_rust_embed_metadata)
	    Metadata::PublicInterface::Export (hir);
	  if (options.metadata_output_path_set ())
	    Metadata::PublicInterface::ExportTo (
	      hir, options.get_metadata_output ());
	}
    }

  // pass to GCC middle-end
  ctx.write_to_backend ();
}

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
      // crate.items.insert (crate.items.begin (), std::move (extern_crate));
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

  // this crate type will have options affecting the metadata ouput

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

  auto fixed_point_reached = false;
  unsigned iterations = 0;

  // create extctxt? from parse session, cfg, and resolver?
  /* expand by calling cxtctxt object's monotonic_expander's expand_crate
   * method. */
  MacroExpander expander (crate, cfg, *this);

  while (!fixed_point_reached && iterations < cfg.recursion_limit)
    {
      /* We need to name resolve macros and imports here */
      Resolver::EarlyNameResolver ().go (crate);

      expander.expand_crate ();

      fixed_point_reached = !expander.has_changed ();
      expander.reset_changed_state ();
      iterations++;

      if (saw_errors ())
	break;
    }

  if (iterations == cfg.recursion_limit)
    {
      auto last_invoc = expander.get_last_invocation ();
      auto last_def = expander.get_last_definition ();

      rust_assert (last_def && last_invoc);

      RichLocation range (last_invoc->get_locus ());
      range.add_range (last_def->get_locus ());

      rust_error_at (range, "reached recursion limit");
    }

  // error reporting - check unused macros, get missing fragment specifiers

  // build test harness

  // ast validation (also with proc macro decls)

  // maybe create macro crate if not rustdoc

  rust_debug ("finished expansion");
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
Session::dump_ast_pretty (AST::Crate &crate, bool expanded) const
{
  std::ofstream out;
  if (expanded)
    out.open (kASTPrettyDumpFileExpanded);
  else
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

// imports

NodeId
Session::load_extern_crate (const std::string &crate_name, Location locus)
{
  // has it already been loaded?
  CrateNum found_crate_num = UNKNOWN_CREATENUM;
  bool found = mappings->lookup_crate_name (crate_name, found_crate_num);
  if (found)
    {
      NodeId resolved_node_id = UNKNOWN_NODEID;
      bool resolved
	= mappings->crate_num_to_nodeid (found_crate_num, resolved_node_id);
      rust_assert (resolved);

      return resolved_node_id;
    }

  std::string relative_import_path = "";
  Import::Stream *s
    = Import::open_package (crate_name, locus, relative_import_path);
  if (s == NULL)
    {
      rust_error_at (locus, "failed to locate crate %<%s%>",
		     crate_name.c_str ());
      return UNKNOWN_NODEID;
    }

  Imports::ExternCrate extern_crate (*s);
  bool ok = extern_crate.load (locus);
  if (!ok)
    {
      rust_error_at (locus, "failed to load crate metadata");
      return UNKNOWN_NODEID;
    }

  // ensure the current vs this crate name don't collide
  const std::string current_crate_name = mappings->get_current_crate_name ();
  if (current_crate_name.compare (extern_crate.get_crate_name ()) == 0)
    {
      rust_error_at (locus, "current crate name %<%s%> collides with this",
		     current_crate_name.c_str ());
      return UNKNOWN_NODEID;
    }

  // setup mappings
  CrateNum saved_crate_num = mappings->get_current_crate ();
  CrateNum crate_num
    = mappings->get_next_crate_num (extern_crate.get_crate_name ());
  mappings->set_current_crate (crate_num);

  // then lets parse this as a 2nd crate
  Lexer lex (extern_crate.get_metadata ());
  Parser<Lexer> parser (lex);
  std::unique_ptr<AST::Crate> metadata_crate = parser.parse_crate ();
  AST::Crate &parsed_crate
    = mappings->insert_ast_crate (std::move (metadata_crate), crate_num);

  // name resolve it
  Resolver::NameResolution::Resolve (parsed_crate);

  // perform hir lowering
  std::unique_ptr<HIR::Crate> lowered
    = HIR::ASTLowering::Resolve (parsed_crate);
  HIR::Crate &hir = mappings->insert_hir_crate (std::move (lowered));

  // perform type resolution
  Resolver::TypeResolution::Resolve (hir);

  // always restore the crate_num
  mappings->set_current_crate (saved_crate_num);

  return parsed_crate.get_node_id ();
}
//

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
