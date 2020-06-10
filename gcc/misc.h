// -*- C++ -*-

// Miscellaneous support routines not intended for upstream merge.

// Class to report any possible changes to the IL while folding.
//
// When environment variable EVRP_TRAPS is set, it points to the
// output file to dump any IL changes to.  If EVRP_TRAPS is not set,
// any diagnostics are dumped to stderr and a trap occurs at the first
// change.

class gimple_state
{
public:
  gimple_state ();
  ~gimple_state ();
  gimple *set_orig_stmt (gimple *stmt);
  void maybe_dump_differences_and_trap (gimple *new_stmt, tree lhs = NULL);

private:
  gimple *orig_stmt;
  gimple *untainted_stmt;
  FILE *out;
  bool accumulate_changes;
};

// Hook for pretty printer to highlight a particular statement.

class highlighter
{
public:
  void set (gimple *untainted_stmt = 0,
	    gimple *old_stmt = 0, gimple *new_stmt = 0,
	    tree lhs = 0);
  void on (class pretty_printer *, int spc, gimple *);
  void off (class pretty_printer *, int spc, gimple *);
private:
  gimple *untainted_stmt;
  gimple *old_stmt;
  gimple *new_stmt;
  tree lhs;
};

extern class highlighter highlighter;

// Class to assert that the new range is at least as good as the old
// one.

class vr_comparison
{
public:
  vr_comparison (const irange *, const irange *, class vr_values * = 0);
  void compare (tree name, edge);
  void compare (gimple *);
private:
  void compare ();
  void dump_differences_and_trap () const;
  void dump_differences (FILE *) const;
  void dump_improvements (FILE *) const;
  bool new_range_is_same () const;
  bool new_range_is_better () const;
  tree m_name;
  edge m_edge;
  gimple *m_stmt;
  const irange *m_old_range;
  const irange *m_new_range;
  class vr_values *m_vr_values;
};

static inline bool
evrp_trap_p ()
{
  return flag_rvrp1_changes > 0;
}
