#ifndef RUST_SESSION_MANAGER_H
#define RUST_SESSION_MANAGER_H
// Session manager - controls compiler session.

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"
#include "rust-system.h"

#include "rust-linemap.h"
#include "rust-backend.h"

#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <utility>

namespace Rust {
// parser forward decl
template <typename ManagedTokenSource> class Parser;
class Lexer;
// crate forward decl
namespace AST {
struct Crate;
}

/* Data related to target, most useful for conditional compilation and
 * whatever. */
struct TargetOptions
{
  /* TODO: maybe make private and access through helpers to allow changes to
   * impl */
  std::unordered_map<std::string, std::unordered_set<std::string>> features;

public:
  // Returns whether a key is defined in the feature set.
  bool has_key (std::string key) const
  {
    return features.find (key) != features.end ();
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
	if (set.size () == 1)
	  return *set.begin ();
      }
    return "";
  }

  /* Returns all values associated with a key (including none), or an empty
   * set if no key is found. */
  std::unordered_set<std::string> get_values_for_key (std::string key) const
  {
    auto it = features.find (key);
    if (it != features.end ())
      return it->second;
    return {};
  }

  /* Inserts a key (no value) into the feature set. This will do nothing if
   * the key already exists. This returns whether the insertion was successful
   * (i.e. whether key already existed). */
  bool insert_key (std::string key)
  {
    return features
      .insert (std::make_pair (key, std::unordered_set<std::string> ()))
      .second;
  }

  // Inserts a key-value pair into the feature set.
  void insert_key_value_pair (std::string key, std::string value)
  {
    auto existing_set = get_values_for_key (key);
    existing_set.insert (std::move (value));
    features[std::move (key)] = std::move (existing_set);
  }

  // Dump all target options to stderr.
  void dump_target_options () const;

  /* Creates derived values and implicit enables after all target info is added
   * (e.g. "unix"). */
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
  // TODO: use bitfield for smaller memory requirements?

  /* FIXME: this is set up for "instead of" dumping - in future, dumps should
   * not inhibit compilation */
  enum DumpOptions
  {
    NO_DUMP,
    LEXER_DUMP,
    PARSER_AST_DUMP,
    REGISTER_PLUGINS_DUMP,
    INJECTION_DUMP,
    EXPANSION_DUMP,
    RESOLUTION_DUMP,
    TARGET_OPTION_DUMP,
    // TODO: add more?
  } dump_option;

  /* configuration options - actually useful for conditional compilation and
   * whatever data related to target arch, features, os, family, env, endian,
   * pointer width, vendor */
  TargetOptions target_data;
  bool enable_test = false;
  bool debug_assertions = false;
  bool proc_macro = false;
};

/* Defines a compiler session. This is for a single compiler invocation, so
 * potentially includes parsing multiple crates. */
struct Session
{
  CompileOptions options;
  /* This should really be in a per-crate storage area but it is wiped with
   * every file so eh. */
  std::string injected_crate_name;

  // backend wrapper to GCC GENERIC
  Backend *backend;

  // backend linemap
  Linemap *linemap;

  // TODO: replace raw pointers with smart pointers?

public:
  /* Initialise compiler session. Corresponds to langhook grs_langhook_init().
   * Note that this is called after option handling. */
  void init ();
  bool handle_option (enum opt_code code, const char *arg, HOST_WIDE_INT value,
		      int kind, location_t loc,
		      const struct cl_option_handlers *handlers);
  void parse_files (int num_files, const char **files);
  void init_options ();

private:
  // TODO: should this be private or public?
  void parse_file (const char *filename);
  bool enable_dump (std::string arg);

  void debug_dump_load_crates (Parser<Lexer> &parser);

  void implicitly_enable_feature (std::string feature_name);
  void enable_features ();

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
  void expansion (AST::Crate &crate);
  /* Resolution pipeline stage. TODO maybe move to another object.
   * Performs name resolution and type resolution, maybe complete gated
   * feature checking, maybe create buffered lints in future. */
  void resolution (AST::Crate &crate);
};
} // namespace Rust

#endif
