/* Interprocedural reference lists.
   Copyright (C) 2010-2015 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

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
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "options.h"
#include "wide-int.h"
#include "inchash.h"
#include "tree.h"
#include "fold-const.h"
#include "ggc.h"
#include "target.h"
#include "hash-map.h"
#include "is-a.h"
#include "plugin-api.h"
#include "tm.h"
#include "hard-reg-set.h"
#include "input.h"
#include "function.h"
#include "ipa-ref.h"
#include "cgraph.h"
#include "ipa-utils.h"

/* Remove reference.  */

void
ipa_ref::remove_reference ()
{
  struct ipa_ref_list *list = referred_ref_list ();
  struct ipa_ref_list *list2 = referring_ref_list ();
  vec<ipa_ref_t, va_gc> *old_references = list2->references;
  struct ipa_ref *last;

  gcc_assert (list->referring[referred_index] == this);

  last = list->referring.last ();
  if (this != last)
    {
      if (use == IPA_REF_ALIAS)
        {
	  /* If deleted item is IPA_REF_ALIAS, we have to move last
	  item of IPA_REF_LIST type to the deleted position. After that
	  we replace last node with deletion slot.  */
	  struct ipa_ref *last_alias = list->last_alias ();

	  if (last_alias && referred_index < last_alias->referred_index
	      && last_alias != last)
	  {
	    unsigned last_alias_index = last_alias->referred_index;

	    list->referring[referred_index] = last_alias;
	    list->referring[referred_index]->referred_index = referred_index;

	    /* New position for replacement is previous index
	       of the last_alias.  */
	    referred_index = last_alias_index;
	  }
	}

      list->referring[referred_index] = list->referring.last ();
      list->referring[referred_index]->referred_index= referred_index;
    }
  list->referring.pop ();

  last = &list2->references->last ();

  struct ipa_ref *ref = this;

  if (ref != last)
    {
      *ref = *last;
      ref->referred_ref_list ()->referring[referred_index] = ref;
    }
  list2->references->pop ();
  gcc_assert (list2->references == old_references);
}

/* Return true when execution of reference can lead to return from
   function.  */

bool
ipa_ref::cannot_lead_to_return ()
{
  return dyn_cast <cgraph_node *> (referring)->cannot_return_p ();
}

/* Return reference list this reference is in.  */

struct ipa_ref_list *
ipa_ref::referring_ref_list (void)
{
  return &referring->ref_list;
}

/* Return reference list this reference is in.  */

struct ipa_ref_list *
ipa_ref::referred_ref_list (void)
{
  return &referred->ref_list;
}
