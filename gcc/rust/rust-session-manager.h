// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#ifndef RUST_SESSION_MANAGER_H
#define RUST_SESSION_MANAGER_H

#include "rust-linemap.h"
#include "rust-backend.h"
#include "rust-hir-map.h"
#include "safe-ctype.h"
#include "rust-name-resolution-context.h"

#include "config.h"
#include "rust-system.h"
#include "coretypes.h"
#include "options.h"

#include "optional.h"

namespace Rust {
// parser forward decl
template <typename ManagedTokenSource> class Parser;
class Lexer;
// crate forward decl
namespace AST {
struct Crate;
}
// crate forward decl
namespace HIR {
class Crate;
}

/* Data related to target, most useful for conditional compilation and
 * whatever. */
struct TargetOptions
{
  /* TODO: maybe make private and access through helpers to allow changes to
   * impl */
  std::unordered_map<std::string, std::unordered_set<tl::optional<std::string>>>
    features;

  enum class CrateType
  {
    BIN = 0,
    LIB,
    RLIB,
    DYLIB,
    CDYLIB,
    STATICLIB,
    PROC_MACRO
  } crate_type
    = CrateType::BIN;

public:
  void set_crate_type (int raw_type)
  {
    crate_type = static_cast<CrateType> (raw_type);
  }

  const CrateType &get_crate_type () const { return crate_type; }

  // Returns whether a key is defined in the feature set.
  bool has_key (std::string key) const
  {
    auto it = features.find (key);
    return it != features.end ()
	   && it->second.find (tl::nullopt) != it->second.end ();
  }

  // Returns whether a key exists with the given value in the feature set.
  bool has_key_value_pair (std::string key, std::string value) const
  {
    auto it = features.find (key);
    if (it != features.end ())
      {
	auto set = it->second;
	auto it2 = set.find (value);
	if (it2 != set.end ())
	  return true;
      }
    return false;
  }

  /* Returns the singular value from the key, or if the key has multiple, an
   * empty string. */
  std::string get_singular_value (std::string key) const
  {
    auto it = features.find (key);
    if (it != features.end ())
      {
	auto set = it->second;
	if (set.size () == 1 && set.begin ()->has_value ())
	  return set.begin ()->value ();
      }
    return "";
  }

  /* Returns all values associated with a key (including none), or an empty
   * set if no key is found. */
  std::unordered_set<std::string> get_values_for_key (std::string key) const
  {
    std::unordered_set<std::string> ret;

    auto it = features.find (key);
    if (it == features.end ())
      return {};

    for (auto &val : it->second)
      if (val.has_value ())
	ret.insert (val.value ());

    return ret;
  }

  /* Inserts a key (no value) into the feature set. This will do nothing if
   * the key already exists. This returns whether the insertion was successful
   * (i.e. whether key already existed). */
  bool insert_key (std::string key)
  {
    auto it = features.find (key);

    if (it == features.end ())
      it
	= features
	    .insert (
	      std::make_pair (std::move (key),
			      std::unordered_set<tl::optional<std::string>> ()))
	    .first;

    return it->second.insert (tl::nullopt).second;
  }

  // Inserts a key-value pair into the feature set.
  void insert_key_value_pair (std::string key, std::string value)
  {
    auto it = features.find (key);

    if (it == features.end ())
      it
	= features
	    .insert (
	      std::make_pair (std::move (key),
			      std::unordered_set<tl::optional<std::string>> ()))
	    .first;

    it->second.insert (std::move (value));
  }

  // Dump all target options to stderr.
  void dump_target_options () const;

  /* Creates derived values and implicit enables after all target info is
   * added (e.g. "unix"). */
  void init_derived_values ();

  /* Enables all requirements for the feature given, and will enable feature
   * itself if not enabled. */
  void enable_implicit_feature_reqs (std::string feature);

  /* According to reference, Rust uses either multi-map key-values or just
   * values (although values may be aliases for a key-value value). This seems
   * like overkill. Thus, depending on whether the attributes used in cfg are
   * fixed or not, I think I'll either put each non-multimap "key-value" as a
   * separate field and have the multimap "key-values" in a regular map for
   * that one key, or actually use a multimap.
   *
   * rustc itself uses a set of key-value tuples where the second tuple
   * element is optional. This gets rid of the requirement to make a
   * multi-map, I guess, but seems like it might make search slow (unless all
   * "is defined"-only ones have empty string as second element). */
  /* cfg attributes:
   * - target_arch: single value
   * - target_feature: multiple values possible
   * - target_os: single value
   * - target_family: single value (or no value?)
   * - unix: set when target_family = "unix"
   * - windows: set when target_family = "windows"
   *  - if these are just syntactic sugar, then maybe have a separate set or
   * map for this kind of stuff
   * - target_env: set when needed for disambiguation about ABI - usually
   * empty string for GNU, complicated
   *  - seems to be a single value (if any)
   * - target_endian: single value; "little" or "big"
   * - target_pointer_width: single value, "32" for 32-bit pointers, etc.
   * - target_vendor, single value
   * - test: set when testing is being done
   *  - again, seems similar to a "is defined" rather than "is equal to" like
   * unix
   * - debug_assertions: seems to "is defined"
   * - proc_macro: no idea, bad docs. seems to be boolean, so maybe "is
   * defined"
   */
};

// Defines compiler options (e.g. dump, etc.).
struct CompileOptions
{
  enum DumpOption
  {
    LEXER_DUMP,
    AST_DUMP_PRETTY,
    REGISTER_PLUGINS_DUMP,
    INJECTION_DUMP,
    EXPANSION_DUMP,
    RESOLUTION_DUMP,
    TARGET_OPTION_DUMP,
    HIR_DUMP,
    HIR_DUMP_PRETTY,
    BIR_DUMP,
  };

  std::set<DumpOption> dump_options;

  /* configuration options - actually useful for conditional compilation and
   * whatever data related to target arch, features, os, family, env, endian,
   * pointer width, vendor */
  TargetOptions target_data;
  std::string crate_name;
  bool crate_name_set_manually = false;
  bool enable_test = false;
  bool debug_assertions = false;
  std::string metadata_output_path;

  enum class Edition
  {
    E2015 = 0,
    E2018,
    E2021,
  } edition
    = Edition::E2015;

  enum class CompileStep
  {
    Ast,
    AttributeCheck,
    Expansion,
    ASTValidation,
    FeatureGating,
    NameResolution,
    Lowering,
    TypeCheck,
    Privacy,
    Unsafety,
    Const,
    BorrowCheck,
    Compilation,
    End,
  } compile_until
    = CompileStep::End;

  bool dump_option_enabled (DumpOption option) const
  {
    return dump_options.find (option) != dump_options.end ();
  }

  void enable_dump_option (DumpOption option) { dump_options.insert (option); }

  void enable_all_dump_options ()
  {
    enable_dump_option (DumpOption::LEXER_DUMP);
    enable_dump_option (DumpOption::AST_DUMP_PRETTY);
    enable_dump_option (DumpOption::REGISTER_PLUGINS_DUMP);
    enable_dump_option (DumpOption::INJECTION_DUMP);
    enable_dump_option (DumpOption::EXPANSION_DUMP);
    enable_dump_option (DumpOption::RESOLUTION_DUMP);
    enable_dump_option (DumpOption::TARGET_OPTION_DUMP);
    enable_dump_option (DumpOption::HIR_DUMP);
    enable_dump_option (DumpOption::HIR_DUMP_PRETTY);
    enable_dump_option (DumpOption::BIR_DUMP);
  }

  void set_crate_name (std::string name)
  {
    rust_assert (!name.empty ());

    crate_name = std::move (name);
  }

  const std::string &get_crate_name () const
  {
    rust_assert (!crate_name.empty ());
    return crate_name;
  }

  void set_edition (int raw_edition)
  {
    edition = static_cast<Edition> (raw_edition);
  }

  const Edition &get_edition () const { return edition; }

  void set_crate_type (int raw_type) { target_data.set_crate_type (raw_type); }

  bool is_proc_macro () const
  {
    return target_data.get_crate_type ()
	   == TargetOptions::CrateType::PROC_MACRO;
  }

  void set_compile_step (int raw_step)
  {
    compile_until = static_cast<CompileStep> (raw_step);
  }

  const CompileStep &get_compile_until () const { return compile_until; }

  void set_metadata_output (const std::string &path)
  {
    metadata_output_path = path;
  }

  const std::string &get_metadata_output () const
  {
    return metadata_output_path;
  }

  bool metadata_output_path_set () const
  {
    return !metadata_output_path.empty ();
  }
};

/* Defines a compiler session. This is for a single compiler invocation, so
 * potentially includes parsing multiple crates. */
struct Session
{
  CompileOptions options;
  /* This should really be in a per-crate storage area but it is wiped with
   * every file so eh. */
  std::string injected_crate_name;
  std::map<std::string, std::string> extern_crates;

  /* extra files get included during late stages of compilation (e.g. macro
   * expansion) */
  std::vector<std::string> extra_files;

  // backend linemap
  Linemap *linemap;

  // mappings
  Analysis::Mappings *mappings;

public:
  /* Get a reference to the static session instance */
  static Session &get_instance ();

  Session () = default;
  ~Session () = default;

  /* This initializes the compiler session. Corresponds to langhook
   * grs_langhook_init(). Note that this is called after option handling. */
  void init ();

  // delete those constructors so we don't access the singleton in any
  // other way than via `get_instance()`
  Session (Session const &) = delete;
  void operator= (Session const &) = delete;

  bool handle_option (enum opt_code code, const char *arg, HOST_WIDE_INT value,
		      int kind, location_t loc,
		      const struct cl_option_handlers *handlers);
  void handle_input_files (int num_files, const char **files);
  void init_options ();
  void handle_crate_name (const AST::Crate &parsed_crate);

  /* This function saves the filename data into the session manager using the
   * `move` semantics, and returns a C-style string referencing the input
   * std::string */
  inline const char *include_extra_file (std::string filename)
  {
    extra_files.push_back (std::move (filename));
    return extra_files.back ().c_str ();
  }

  NodeId load_extern_crate (const std::string &crate_name, location_t locus);

private:
  void compile_crate (const char *filename);
  bool enable_dump (std::string arg);

  void dump_lex (Parser<Lexer> &parser) const;
  void dump_ast_pretty (AST::Crate &crate, bool expanded = false) const;
  void dump_name_resolution (Resolver2_0::NameResolutionContext &ctx) const;
  void dump_hir (HIR::Crate &crate) const;
  void dump_hir_pretty (HIR::Crate &crate) const;

  // pipeline stages - TODO maybe move?
  /* Register plugins pipeline stage. TODO maybe move to another object?
   * Currently dummy stage. In future will handle attribute injection
   * (top-level inner attribute creation from command line arguments), setting
   * options maybe, registering lints maybe, loading plugins maybe. */
  void register_plugins (AST::Crate &crate);

  /* Injection pipeline stage. TODO maybe move to another object? Maybe have
   * some lint checks (in future, obviously), register builtin macros, crate
   * injection. */
  void injection (AST::Crate &crate);

  /* Expansion pipeline stage. TODO maybe move to another object? Expands all
   * macros, maybe build test harness in future, AST validation, maybe create
   * macro crate (if not rustdoc).*/
  void expansion (AST::Crate &crate, Resolver2_0::NameResolutionContext &ctx);

  // handle cfg_option
  bool handle_cfg_option (std::string &data);

  bool handle_extern_option (std::string &data);
};

} // namespace Rust

#if CHECKING_P
namespace selftest {
extern void
rust_crate_name_validation_test (void);
}
#endif // CHECKING_P

#endif
