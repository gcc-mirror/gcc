/* Stacks of set of classifications of diagnostics.
   Copyright (C) 1999-2025 Free Software Foundation, Inc.

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


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "version.h"
#include "diagnostic.h"

namespace diagnostics {

void
option_classifier::init (int n_opts)
{
  m_n_opts = n_opts;
  m_classify_diagnostic = XNEWVEC (enum kind, n_opts);
  for (int i = 0; i < n_opts; i++)
    m_classify_diagnostic[i] = kind::unspecified;
  m_push_list = vNULL;
  m_classification_history = vNULL;
}

void
option_classifier::fini ()
{
  XDELETEVEC (m_classify_diagnostic);
  m_classify_diagnostic = nullptr;
  m_classification_history.release ();
  m_push_list.release ();
}

/* Save the diagnostics::option_classifier state to F for PCH
   output.  Returns 0 on success, -1 on error.  */

int
option_classifier::pch_save (FILE *f)
{
  unsigned int lengths[2] = { m_classification_history.length (),
			      m_push_list.length () };
  if (fwrite (lengths, sizeof (lengths), 1, f) != 1
      || (lengths[0]
	  && fwrite (m_classification_history.address (),
		     sizeof (classification_change_t),
		     lengths[0], f) != lengths[0])
      || (lengths[1]
	  && fwrite (m_push_list.address (), sizeof (int),
		     lengths[1], f) != lengths[1]))
    return -1;
  return 0;
}

/* Read the diagnostics::option_classifier state from F for PCH
   read.  Returns 0 on success, -1 on error.  */

int
option_classifier::pch_restore (FILE *f)
{
  unsigned int lengths[2];
  if (fread (lengths, sizeof (lengths), 1, f) != 1)
    return -1;
  gcc_checking_assert (m_classification_history.is_empty ());
  gcc_checking_assert (m_push_list.is_empty ());
  m_classification_history.safe_grow (lengths[0]);
  m_push_list.safe_grow (lengths[1]);
  if ((lengths[0]
       && fread (m_classification_history.address (),
		 sizeof (classification_change_t),
		 lengths[0], f) != lengths[0])
      || (lengths[1]
	  && fread (m_push_list.address (), sizeof (int),
		    lengths[1], f) != lengths[1]))
    return -1;
  return 0;
}

/* Save all diagnostic classifications in a stack.  */

void
option_classifier::push ()
{
  m_push_list.safe_push (m_classification_history.length ());
}

/* Restore the topmost classification set off the stack.  If the stack
   is empty, revert to the state based on command line parameters.  */

void
option_classifier::pop (location_t where)
{
  int jump_to;

  if (!m_push_list.is_empty ())
    jump_to = m_push_list.pop ();
  else
    jump_to = 0;

  classification_change_t v = { where, jump_to, kind::pop };
  m_classification_history.safe_push (v);
}

/* Interface to specify diagnostic kind overrides.  Returns the
   previous setting, or kind::unspecified if the parameters are out of
   range.  If OPTION_ID is zero, the new setting is for all the
   diagnostics.  */

enum kind
option_classifier::classify_diagnostic (const context *dc,
					option_id opt_id,
					enum kind new_kind,
					location_t where)
{
  enum kind old_kind;

  if (opt_id.m_idx < 0
      || opt_id.m_idx >= m_n_opts
      || new_kind >= kind::last_diagnostic_kind)
    return kind::unspecified;

  old_kind = m_classify_diagnostic[opt_id.m_idx];

  /* Handle pragmas separately, since we need to keep track of *where*
     the pragmas were.  */
  if (where != UNKNOWN_LOCATION)
    {
      unsigned i;

      /* Record the command-line status, so we can reset it back on kind::pop. */
      if (old_kind == kind::unspecified)
	{
	  old_kind = (!dc->option_enabled_p (opt_id)
		      ? kind::ignored : kind::any);
	  m_classify_diagnostic[opt_id.m_idx] = old_kind;
	}

      classification_change_t *p;
      FOR_EACH_VEC_ELT_REVERSE (m_classification_history, i, p)
	if (p->option == opt_id.m_idx)
	  {
	    old_kind = p->kind;
	    break;
	  }

      classification_change_t v
	= { where, opt_id.m_idx, new_kind };
      m_classification_history.safe_push (v);
    }
  else
    m_classify_diagnostic[opt_id.m_idx] = new_kind;

  return old_kind;
}

/* Update the kind of DIAGNOSTIC based on its location(s), including
   any of those in its inlining stack, relative to any
     #pragma GCC diagnostic
   directives recorded within this object.

   Return the new kind of DIAGNOSTIC if it was updated, or kind::unspecified
   otherwise.  */

enum kind
option_classifier::
update_effective_level_from_pragmas (diagnostic_info *diagnostic) const
{
  if (m_classification_history.is_empty ())
    return kind::unspecified;

  /* Iterate over the locations, checking the diagnostic disposition
     for the diagnostic at each.  If it's explicitly set as opposed
     to unspecified, update the disposition for this instance of
     the diagnostic and return it.  */
  for (location_t loc: diagnostic->m_iinfo.m_ilocs)
    {
      /* FIXME: Stupid search.  Optimize later. */
      unsigned int i;
      classification_change_t *p;
      FOR_EACH_VEC_ELT_REVERSE (m_classification_history, i, p)
	{
	  location_t pragloc = p->location;
	  if (!linemap_location_before_p (line_table, pragloc, loc))
	    continue;

	  if (p->kind == kind::pop)
	    {
	      /* Move on to the next region.  */
	      i = p->option;
	      continue;
	    }

	  option_id opt_id = p->option;
	  /* The option 0 is for all the diagnostics.  */
	  if (opt_id == 0 || opt_id == diagnostic->m_option_id)
	    {
	      enum kind kind = p->kind;
	      if (kind != diagnostics::kind::unspecified)
		diagnostic->m_kind = kind;
	      return kind;
	    }
	}
    }

  return kind::unspecified;
}

} // namespace diagnostics
