/* Definitions for the shared dumpfile.
   Copyright (C) 2004-2018 Free Software Foundation, Inc.

This file is part of GCC.

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


#ifndef GCC_DUMPFILE_H
#define GCC_DUMPFILE_H 1

#include "profile-count.h"

/* An attribute for annotating formatting printing functions that use
   the dumpfile/optinfo formatting codes.  These are the pretty_printer
   format codes (see pretty-print.c), with additional codes for middle-end
   specific entities (see dumpfile.c).  */

#if GCC_VERSION >= 9000
#define ATTRIBUTE_GCC_DUMP_PRINTF(m, n) \
  __attribute__ ((__format__ (__gcc_dump_printf__, m ,n))) \
  ATTRIBUTE_NONNULL(m)
#else
#define ATTRIBUTE_GCC_DUMP_PRINTF(m, n) ATTRIBUTE_NONNULL(m)
#endif

/* Different tree dump places.  When you add new tree dump places,
   extend the DUMP_FILES array in dumpfile.c.  */
enum tree_dump_index
{
  TDI_none,			/* No dump */
  TDI_cgraph,			/* dump function call graph.  */
  TDI_inheritance,		/* dump type inheritance graph.  */
  TDI_clones,			/* dump IPA cloning decisions.  */
  TDI_original,			/* dump each function before optimizing it */
  TDI_gimple,			/* dump each function after gimplifying it */
  TDI_nested,			/* dump each function after unnesting it */
  TDI_lto_stream_out,		/* dump information about lto streaming */

  TDI_lang_all,			/* enable all the language dumps.  */
  TDI_tree_all,			/* enable all the GENERIC/GIMPLE dumps.  */
  TDI_rtl_all,			/* enable all the RTL dumps.  */
  TDI_ipa_all,			/* enable all the IPA dumps.  */

  TDI_end
};

/* Enum used to distinguish dump files to types.  */

enum dump_kind
{
  DK_none,
  DK_lang,
  DK_tree,
  DK_rtl,
  DK_ipa
};

/* Bit masks to control dumping. Not all values are applicable to all
   dumps. Add new ones at the end. When you define new values, extend
   the DUMP_OPTIONS array in dumpfile.c. The TDF_* flags coexist with
   MSG_* flags (for -fopt-info) and the bit values must be chosen to
   allow that.  */
enum dump_flag
{
  /* Value of TDF_NONE is used just for bits filtered by TDF_KIND_MASK.  */
  TDF_NONE  = 0,

  /* Dump node addresses.  */
  TDF_ADDRESS = (1 << 0),

  /* Don't go wild following links.  */
  TDF_SLIM = (1 << 1),

  /* Don't unparse the function.  */
  TDF_RAW = (1 << 2),

  /* Show more detailed info about each pass.  */
  TDF_DETAILS = (1 << 3),

  /* Dump various statistics about each pass.  */
  TDF_STATS = (1 << 4),

  /* Display basic block boundaries.  */
  TDF_BLOCKS = (1 << 5),

  /* Display virtual operands.  */
  TDF_VOPS = (1 << 6),

  /* Display statement line numbers.  */
  TDF_LINENO = (1 << 7),

  /* Display decl UIDs.  */
  TDF_UID  = (1 << 8),

  /* Address of stmt.  */
  TDF_STMTADDR = (1 << 9),

  /* A graph dump is being emitted.  */
  TDF_GRAPH = (1 << 10),

  /* Display memory symbols in expr.
     Implies TDF_VOPS.  */
  TDF_MEMSYMS = (1 << 11),

  /* A flag to only print the RHS of a gimple stmt.  */
  TDF_RHS_ONLY = (1 << 12),

  /* Display asm names of decls.  */
  TDF_ASMNAME = (1 << 13),

  /* Display EH region number holding this gimple statement.  */
  TDF_EH  = (1 << 14),

  /* Omit UIDs from dumps.  */
  TDF_NOUID = (1 << 15),

  /* Display alias information.  */
  TDF_ALIAS = (1 << 16),

  /* Enumerate locals by uid.  */
  TDF_ENUMERATE_LOCALS = (1 << 17),

  /* Dump cselib details.  */
  TDF_CSELIB = (1 << 18),

  /* Dump SCEV details.  */
  TDF_SCEV = (1 << 19),

  /* Dump in GIMPLE FE syntax  */
  TDF_GIMPLE = (1 << 20),

  /* Dump folding details.  */
  TDF_FOLDING = (1 << 21),

  /* -fopt-info optimized sources.  */
  MSG_OPTIMIZED_LOCATIONS = (1 << 22),

  /* Missed opportunities.  */
  MSG_MISSED_OPTIMIZATION = (1 << 23),

  /* General optimization info.  */
  MSG_NOTE = (1 << 24),

  MSG_ALL = (MSG_OPTIMIZED_LOCATIONS
	     | MSG_MISSED_OPTIMIZATION
	     | MSG_NOTE),

  /* Dumping for -fcompare-debug.  */
  TDF_COMPARE_DEBUG = (1 << 25),

  /* All values.  */
  TDF_ALL_VALUES = (1 << 26) - 1
};

/* Dump flags type.  */

typedef enum dump_flag dump_flags_t;

static inline dump_flags_t
operator| (dump_flags_t lhs, dump_flags_t rhs)
{
  return (dump_flags_t)((int)lhs | (int)rhs);
}

static inline dump_flags_t
operator& (dump_flags_t lhs, dump_flags_t rhs)
{
  return (dump_flags_t)((int)lhs & (int)rhs);
}

static inline dump_flags_t
operator~ (dump_flags_t flags)
{
  return (dump_flags_t)~((int)flags);
}

static inline dump_flags_t &
operator|= (dump_flags_t &lhs, dump_flags_t rhs)
{
  lhs = (dump_flags_t)((int)lhs | (int)rhs);
  return lhs;
}

static inline dump_flags_t &
operator&= (dump_flags_t &lhs, dump_flags_t rhs)
{
  lhs = (dump_flags_t)((int)lhs & (int)rhs);
  return lhs;
}

/* Flags to control high-level -fopt-info dumps.  Usually these flags
   define a group of passes.  An optimization pass can be part of
   multiple groups.  */

enum optgroup_flag
{
  OPTGROUP_NONE = 0,

  /* IPA optimization passes */
  OPTGROUP_IPA  = (1 << 1),

  /* Loop optimization passes */
  OPTGROUP_LOOP = (1 << 2),

  /* Inlining passes */
  OPTGROUP_INLINE = (1 << 3),

  /* OMP (Offloading and Multi Processing) transformations */
  OPTGROUP_OMP = (1 << 4),

  /* Vectorization passes */
  OPTGROUP_VEC = (1 << 5),

  /* All other passes */
  OPTGROUP_OTHER = (1 << 6),

  OPTGROUP_ALL = (OPTGROUP_IPA | OPTGROUP_LOOP | OPTGROUP_INLINE
		  | OPTGROUP_OMP | OPTGROUP_VEC | OPTGROUP_OTHER)
};

typedef enum optgroup_flag optgroup_flags_t;

static inline optgroup_flags_t
operator| (optgroup_flags_t lhs, optgroup_flags_t rhs)
{
  return (optgroup_flags_t)((int)lhs | (int)rhs);
}

static inline optgroup_flags_t &
operator|= (optgroup_flags_t &lhs, optgroup_flags_t rhs)
{
  lhs = (optgroup_flags_t)((int)lhs | (int)rhs);
  return lhs;
}

/* Define a tree dump switch.  */
struct dump_file_info
{
  /* Suffix to give output file.  */
  const char *suffix;
  /* Command line dump switch.  */
  const char *swtch;
  /* Command line glob.  */
  const char *glob;
  /* Filename for the pass-specific stream.  */
  const char *pfilename;
  /* Filename for the -fopt-info stream.  */
  const char *alt_filename;
  /* Pass-specific dump stream.  */
  FILE *pstream;
  /* -fopt-info stream.  */
  FILE *alt_stream;
  /* Dump kind.  */
  dump_kind dkind;
  /* Dump flags.  */
  dump_flags_t pflags;
  /* A pass flags for -fopt-info.  */
  dump_flags_t alt_flags;
  /* Flags for -fopt-info given by a user.  */
  optgroup_flags_t optgroup_flags;
  /* State of pass-specific stream.  */
  int pstate;
  /* State of the -fopt-info stream.  */
  int alt_state;
  /* Dump file number.  */
  int num;
  /* Fields "suffix", "swtch", "glob" can be const strings,
     or can be dynamically allocated, needing free.  */
  bool owns_strings;
  /* When a given dump file is being initialized, this flag is set to true
     if the corresponding TDF_graph dump file has also been initialized.  */
  bool graph_dump_initialized;
};

/* A class for describing where in the user's source that a dump message
   relates to, with various constructors for convenience.
   In particular, this lets us associate dump messages
   with hotness information (e.g. from PGO), allowing them to
   be prioritized by code hotness.  */

class dump_user_location_t
{
 public:
  /* Default constructor, analogous to UNKNOWN_LOCATION.  */
  dump_user_location_t () : m_count (), m_loc (UNKNOWN_LOCATION) {}

  /* Construct from a gimple statement (using its location and hotness).  */
  dump_user_location_t (const gimple *stmt);

  /* Construct from an RTL instruction (using its location and hotness).  */
  dump_user_location_t (const rtx_insn *insn);

  /* Construct from a location_t.  This one is deprecated (since it doesn't
     capture hotness information); it thus needs to be spelled out.  */
  static dump_user_location_t
  from_location_t (location_t loc)
  {
    return dump_user_location_t (profile_count (), loc);
  }

  /* Construct from a function declaration.  This one requires spelling out
     to avoid accidentally constructing from other kinds of tree.  */
  static dump_user_location_t
  from_function_decl (tree fndecl);

  profile_count get_count () const { return m_count; }
  location_t get_location_t () const { return m_loc; }

 private:
  /* Private ctor from count and location, for use by from_location_t.  */
  dump_user_location_t (profile_count count, location_t loc)
    : m_count (count), m_loc (loc)
  {}

  profile_count m_count;
  location_t m_loc;
};

/* A class for identifying where in the compiler's own source
   (or a plugin) that a dump message is being emitted from.  */

struct dump_impl_location_t
{
  dump_impl_location_t (
#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 8)
			const char *file = __builtin_FILE (),
			int line = __builtin_LINE (),
			const char *function = __builtin_FUNCTION ()
#else
			const char *file = __FILE__,
			int line = __LINE__,
			const char *function = NULL
#endif
  )
  : m_file (file), m_line (line), m_function (function)
  {}

  const char *m_file;
  int m_line;
  const char *m_function;
};

/* A bundle of information for describing the location of a dump message:
   (a) the source location and hotness within the user's code, together with
   (b) the source location within the compiler/plugin.

   The constructors use default parameters so that (b) gets sets up
   automatically.

   The upshot is that you can pass in e.g. a gimple * to dump_printf_loc,
   and the dump call will automatically record where in GCC's source
   code the dump was emitted from.  */

class dump_location_t
{
 public:
  /* Default constructor, analogous to UNKNOWN_LOCATION.  */
  dump_location_t (const dump_impl_location_t &impl_location
		     = dump_impl_location_t ())
  : m_user_location (dump_user_location_t ()),
    m_impl_location (impl_location)
  {
  }

  /* Construct from a gimple statement (using its location and hotness).  */
  dump_location_t (const gimple *stmt,
		   const dump_impl_location_t &impl_location
		     = dump_impl_location_t ())
  : m_user_location (dump_user_location_t (stmt)),
    m_impl_location (impl_location)
  {
  }

  /* Construct from an RTL instruction (using its location and hotness).  */
  dump_location_t (const rtx_insn *insn,
		   const dump_impl_location_t &impl_location
		   = dump_impl_location_t ())
  : m_user_location (dump_user_location_t (insn)),
    m_impl_location (impl_location)
  {
  }

  /* Construct from a dump_user_location_t.  */
  dump_location_t (const dump_user_location_t &user_location,
		   const dump_impl_location_t &impl_location
		     = dump_impl_location_t ())
  : m_user_location (user_location),
    m_impl_location (impl_location)
  {
  }

  /* Construct from a location_t.  This one is deprecated (since it doesn't
     capture hotness information), and thus requires spelling out.  */
  static dump_location_t
  from_location_t (location_t loc,
		   const dump_impl_location_t &impl_location
		     = dump_impl_location_t ())
  {
    return dump_location_t (dump_user_location_t::from_location_t (loc),
			    impl_location);
  }

  const dump_user_location_t &
  get_user_location () const { return m_user_location; }

  const dump_impl_location_t &
  get_impl_location () const { return m_impl_location; }

  location_t get_location_t () const
  {
    return m_user_location.get_location_t ();
  }

  profile_count get_count () const { return m_user_location.get_count (); }

 private:
  dump_user_location_t m_user_location;
  dump_impl_location_t m_impl_location;
};

/* In dumpfile.c */
extern FILE *dump_begin (int, dump_flags_t *, int part=-1);
extern void dump_end (int, FILE *);
extern int opt_info_switch_p (const char *);
extern const char *dump_flag_name (int);
extern const kv_pair<optgroup_flags_t> optgroup_options[];

/* Global variables used to communicate with passes.  */
extern FILE *dump_file;
extern dump_flags_t dump_flags;
extern const char *dump_file_name;

extern bool dumps_are_enabled;

extern void set_dump_file (FILE *new_dump_file);

/* Return true if any of the dumps is enabled, false otherwise. */
static inline bool
dump_enabled_p (void)
{
  return dumps_are_enabled;
}

/* The following API calls (which *don't* take a "FILE *")
   write the output to zero or more locations.

   Some destinations are written to immediately as dump_* calls
   are made; for others, the output is consolidated into an "optinfo"
   instance (with its own metadata), and only emitted once the optinfo
   is complete.

   The destinations are:

   (a) the "immediate" destinations:
       (a.1) the active dump_file, if any
       (a.2) the -fopt-info destination, if any
   (b) the "optinfo" destinations, if any:
       (b.1) as optimization records

   dump_* (MSG_*) --> dumpfile.c --> items --> (a.1) dump_file
                                       |   `-> (a.2) alt_dump_file
                                       |
                                       `--> (b) optinfo
                                                `---> optinfo destinations
                                                      (b.1) optimization records

   For optinfos, the dump_*_loc mark the beginning of an optinfo
   instance: all subsequent dump_* calls are consolidated into
   that optinfo, until the next dump_*_loc call (or a change in
   dump scope, or a call to dumpfile_ensure_any_optinfo_are_flushed).

   A group of dump_* calls should be guarded by:

     if (dump_enabled_p ())

   to minimize the work done for the common case where dumps
   are disabled.  */

extern void dump_printf (dump_flags_t, const char *, ...)
  ATTRIBUTE_GCC_DUMP_PRINTF (2, 3);

extern void dump_printf_loc (dump_flags_t, const dump_location_t &,
			     const char *, ...)
  ATTRIBUTE_GCC_DUMP_PRINTF (3, 0);
extern void dump_function (int phase, tree fn);
extern void dump_basic_block (dump_flags_t, basic_block, int);
extern void dump_generic_expr_loc (dump_flags_t, const dump_location_t &,
				   dump_flags_t, tree);
extern void dump_generic_expr (dump_flags_t, dump_flags_t, tree);
extern void dump_gimple_stmt_loc (dump_flags_t, const dump_location_t &,
				  dump_flags_t, gimple *, int);
extern void dump_gimple_stmt (dump_flags_t, dump_flags_t, gimple *, int);
extern void dump_gimple_expr_loc (dump_flags_t, const dump_location_t &,
				  dump_flags_t, gimple *, int);
extern void dump_gimple_expr (dump_flags_t, dump_flags_t, gimple *, int);
extern void dump_symtab_node (dump_flags_t, symtab_node *);

template<unsigned int N, typename C>
void dump_dec (dump_flags_t, const poly_int<N, C> &);
extern void dump_dec (dump_flags_t, const poly_wide_int &, signop);
extern void dump_hex (dump_flags_t, const poly_wide_int &);

extern void dumpfile_ensure_any_optinfo_are_flushed ();

/* Managing nested scopes, so that dumps can express the call chain
   leading to a dump message.  */

extern unsigned int get_dump_scope_depth ();
extern void dump_begin_scope (const char *name, const dump_location_t &loc);
extern void dump_end_scope ();

/* Implementation detail of the AUTO_DUMP_SCOPE macro below.

   A RAII-style class intended to make it easy to emit dump
   information about entering and exiting a collection of nested
   function calls.  */

class auto_dump_scope
{
 public:
  auto_dump_scope (const char *name, dump_location_t loc)
  {
    if (dump_enabled_p ())
      dump_begin_scope (name, loc);
  }
  ~auto_dump_scope ()
  {
    if (dump_enabled_p ())
      dump_end_scope ();
  }
};

/* A macro for calling:
     dump_begin_scope (NAME, LOC);
   via an RAII object, thus printing "=== MSG ===\n" to the dumpfile etc,
   and then calling
     dump_end_scope ();
   once the object goes out of scope, thus capturing the nesting of
   the scopes.  */

#define AUTO_DUMP_SCOPE(NAME, LOC) \
  auto_dump_scope scope (NAME, LOC)

extern void dump_function (int phase, tree fn);
extern void print_combine_total_stats (void);
extern bool enable_rtl_dump_file (void);

/* In tree-dump.c  */
extern void dump_node (const_tree, dump_flags_t, FILE *);

/* In combine.c  */
extern void dump_combine_total_stats (FILE *);
/* In cfghooks.c  */
extern void dump_bb (FILE *, basic_block, int, dump_flags_t);

namespace gcc {

/* A class for managing all of the various dump files used by the
   optimization passes.  */

class dump_manager
{
public:

  dump_manager ();
  ~dump_manager ();

  /* Register a dumpfile.

     TAKE_OWNERSHIP determines whether callee takes ownership of strings
     SUFFIX, SWTCH, and GLOB. */
  unsigned int
  dump_register (const char *suffix, const char *swtch, const char *glob,
		 dump_kind dkind, optgroup_flags_t optgroup_flags,
		 bool take_ownership);

  /* Allow languages and middle-end to register their dumps before the
     optimization passes.  */
  void
  register_dumps ();

  /* Return the dump_file_info for the given phase.  */
  struct dump_file_info *
  get_dump_file_info (int phase) const;

  struct dump_file_info *
  get_dump_file_info_by_switch (const char *swtch) const;

  /* Return the name of the dump file for the given phase.
     If the dump is not enabled, returns NULL.  */
  char *
  get_dump_file_name (int phase, int part = -1) const;

  char *
  get_dump_file_name (struct dump_file_info *dfi, int part = -1) const;

  int
  dump_switch_p (const char *arg);

  /* Start a dump for PHASE. Store user-supplied dump flags in
     *FLAG_PTR.  Return the number of streams opened.  Set globals
     DUMP_FILE, and ALT_DUMP_FILE to point to the opened streams, and
     set dump_flags appropriately for both pass dump stream and
     -fopt-info stream. */
  int
  dump_start (int phase, dump_flags_t *flag_ptr);

  /* Finish a tree dump for PHASE and close associated dump streams.  Also
     reset the globals DUMP_FILE, ALT_DUMP_FILE, and DUMP_FLAGS.  */
  void
  dump_finish (int phase);

  FILE *
  dump_begin (int phase, dump_flags_t *flag_ptr, int part);

  /* Returns nonzero if tree dump PHASE has been initialized.  */
  int
  dump_initialized_p (int phase) const;

  /* Returns the switch name of PHASE.  */
  const char *
  dump_flag_name (int phase) const;

private:

  int
  dump_phase_enabled_p (int phase) const;

  int
  dump_switch_p_1 (const char *arg, struct dump_file_info *dfi, bool doglob);

  int
  dump_enable_all (dump_kind dkind, dump_flags_t flags, const char *filename);

  int
  opt_info_enable_passes (optgroup_flags_t optgroup_flags, dump_flags_t flags,
			  const char *filename);

private:

  /* Dynamically registered dump files and switches.  */
  int m_next_dump;
  struct dump_file_info *m_extra_dump_files;
  size_t m_extra_dump_files_in_use;
  size_t m_extra_dump_files_alloced;

  /* Grant access to dump_enable_all.  */
  friend bool ::enable_rtl_dump_file (void);

  /* Grant access to opt_info_enable_passes.  */
  friend int ::opt_info_switch_p (const char *arg);

}; // class dump_manager

} // namespace gcc

#endif /* GCC_DUMPFILE_H */
