/* Stacks of set of classifications of diagnostics.
   Copyright (C) 2000-2026 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_DIAGNOSTICS_OPTION_CLASSIFIER_H
#define GCC_DIAGNOSTICS_OPTION_CLASSIFIER_H

#include "diagnostics/option-id.h"
#include "diagnostics/kinds.h"

namespace diagnostics {

/*  Forward declarations.  */
class context;
struct diagnostic_info;

/* A stack of sets of classifications: each entry in the stack is
   a mapping from option index to diagnostic severity that can be changed
   via pragmas.  The stack can be pushed and popped.  */

class option_classifier
{
public:
  void init (int n_opts);
  void fini ();

  /* Save all diagnostic classifications in a stack.  */
  void push ();

  /* Restore the topmost classification set off the stack.  If the stack
     is empty, revert to the state based on command line parameters.  */
  void pop (location_t where);

  bool option_unspecified_p (option_id opt_id) const
  {
    return get_current_override (opt_id) == kind::unspecified;
  }

  enum kind get_current_override (option_id opt_id) const
  {
    gcc_assert (opt_id.m_idx < m_n_opts);
    return m_classify_diagnostic[opt_id.m_idx];
  }

  enum kind
  classify_diagnostic (const context *context,
		       option_id opt_id,
		       enum kind new_kind,
		       location_t where);

  enum kind
  update_effective_level_from_pragmas (diagnostic_info *diagnostic) const;

  int pch_save (FILE *);
  int pch_restore (FILE *);

private:
  /* Each time a diagnostic's classification is changed with a pragma,
     we record the change and the location of the change in an array of
     these structs.  */
  struct classification_change_t
  {
    location_t location;

    /* For kind::pop, this is the index of the corresponding push (as stored
       in m_push_list).
       Otherwise, this is an option index.  */
    int option;

    enum kind kind;
  };

  int m_n_opts;

  /* For each option index that can be passed to warning() et al
     (OPT_* from options.h when using this code with the core GCC
     options), this array may contain a new kind that the diagnostic
     should be changed to before reporting, or kind::unspecified to leave
     it as the reported kind, or kind::ignored to not report it at
     all.  */
  enum kind *m_classify_diagnostic;

  /* History of all changes to the classifications above.  This list
     is stored in location-order, so we can search it, either
     binary-wise or end-to-front, to find the most recent
     classification for a given diagnostic, given the location of the
     diagnostic.  */
  vec<classification_change_t> m_classification_history;

  /* For context::get_classification_history, declared later.  */
  friend class context;

  /* For pragma push/pop.  */
  vec<int> m_push_list;
};

} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_OPTION_CLASSIFIER_H */
