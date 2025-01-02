/* Automatic generation of links into GCC's documentation.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "pretty-print.h"
#include "pretty-print-urlifier.h"
#include "gcc-urlifier.h"
#include "opts.h"
#include "options.h"
#include "diagnostic.h"
#include "selftest.h"
#include "make-unique.h"
#include "target.h"

/* class attribute_urlifier : public urlifier.  */

/* By default, use the target's documentation name.  */

attribute_urlifier::attribute_urlifier ()
: m_target_docs_name (targetm.documentation_name)
{
}

/* Explicitly specify a target's documentation name, for use in selftests.  */

attribute_urlifier::attribute_urlifier (const char *target_docs_name)
: m_target_docs_name (target_docs_name)
{
}

struct attr_url_entry
{
  const char *m_name;
  const char *m_url_suffix;
  const char *m_target_docs_name;
  size_t m_name_len;
};

#include "attr-urls.def"

/* We look in two passes: first for an exact match on target name (if any).
   Otherwise, we look for one with an empty target name.  */

/* Search for STR, LEN in the given TABLE.
   If TARGET_DOCS_NAME is non-null, then look for an exact match on target name.
   If TARGET_DOCS_NAME is null, then look for an empty string for the
   target name.  */


static const attr_url_entry *
find_attr_url_entry (const char *str,
		     size_t str_len,
		     const char *target_docs_name,
		     const attr_url_entry *table,
		     size_t table_sz)
{
  /* This is linear search, but TABLE_SZ ought not to be very large.  */
  for (size_t i = 0; i < table_sz; i++)
    if (str_len == table[i].m_name_len)
      if (0 == strncmp (str, table[i].m_name, str_len))
	{
	  if (target_docs_name)
	    {
	      /* Reject entries with m_target_docs_name that doesn't match.  */
	      if (strcmp (target_docs_name, table[i].m_target_docs_name))
		continue;
	    }
	  else
	    {
	      /* Reject entries for which m_target_docs_name is non-empty.  */
	      if (table[i].m_target_docs_name[0])
		continue;
	    }
	  return &table[i];
	}

  return nullptr;
}

/* Search for STR, LEN in all of the attribute tables, in order.
   TARGET_DOCS_NAME works as above.  */

static const attr_url_entry *
find_attr_url_entry (const char *str,
		     size_t str_len,
		     const char *target_docs_name)
{
  for (size_t table_idx = 0; table_idx < ARRAY_SIZE (attr_url_tables);
       table_idx++)
    if (const attr_url_entry *entry
	  = find_attr_url_entry (str, str_len, target_docs_name,
				 attr_url_tables[table_idx].m_table,
				 attr_url_tables[table_idx].m_table_sz))
      return entry;

  return nullptr;
}

char *
attribute_urlifier::get_url_for_quoted_text (const char *p,
					     size_t sz) const
{
  label_text url_suffix = get_url_suffix_for_quoted_text (p, sz);
  if (url_suffix.get ())
    return make_doc_url (url_suffix.get ());
  return nullptr;
}

label_text
attribute_urlifier::get_url_suffix_for_quoted_text (const char *p,
						    size_t sz) const
{
  /* Skip any text after a non-identifier character, so that
     e.g. given "access(read_write, 2, 3)" we only compare
     against "access".  */
  for (size_t i = 0; i < sz; i++)
    if (!ISIDNUM (p[i]))
      {
	/* Truncate to p[0..i).  */
	sz = i;
	break;
      }

  if (m_target_docs_name)
    if (const attr_url_entry *entry
	  = find_attr_url_entry (p, sz, m_target_docs_name))
      return label_text::borrow (entry->m_url_suffix);

  if (const attr_url_entry *entry = find_attr_url_entry (p, sz, nullptr))
    return label_text::borrow (entry->m_url_suffix);

  return label_text ();
}

label_text
attribute_urlifier::get_url_suffix_for_quoted_text (const char *p) const
{
  return get_url_suffix_for_quoted_text (p, strlen (p));
}

#if CHECKING_P

namespace selftest {

/* Selftests.  */

static void
test_attribute_urlifier ()
{
  attribute_urlifier u;

  ASSERT_EQ (u.get_url_suffix_for_quoted_text ("").get (), nullptr);
  ASSERT_EQ (u.get_url_suffix_for_quoted_text (")").get (), nullptr);

  /* Examples of function attributes.  */
  ASSERT_STREQ (u.get_url_suffix_for_quoted_text ("alias").get (),
		"gcc/Common-Function-Attributes.html"
		"#index-alias-function-attribute");

  ASSERT_STREQ (u.get_url_suffix_for_quoted_text
		  ("access(read_write, 2, 3)").get (),
		"gcc/Common-Function-Attributes.html"
		"#index-access-function-attribute");

  /* Example of enumerator attribute.  */
  ASSERT_STREQ (u.get_url_suffix_for_quoted_text ("deprecated").get (),
		"gcc/Enumerator-Attributes.html"
		"#index-deprecated-enumerator-attribute");

  /* We don't yet have an example of a label attribute, since all
     label attributes have a matching function attribute of the same
     name, which is found first.  */

  /* Example of statement attribute.  */
  ASSERT_STREQ (u.get_url_suffix_for_quoted_text ("assume").get (),
		"gcc/Statement-Attributes.html"
		"#index-assume-statement-attribute");

  /* Examples of type attributes.  */
  ASSERT_STREQ (u.get_url_suffix_for_quoted_text ("hardbool").get (),
		"gcc/Common-Type-Attributes.html"
		"#index-hardbool-type-attribute");
  ASSERT_STREQ (u.get_url_suffix_for_quoted_text
		  ("packed").get (),
		"gcc/Common-Type-Attributes.html"
		"#index-packed-type-attribute");

  /* Example of variable attribute.  */
  ASSERT_STREQ (u.get_url_suffix_for_quoted_text ("nonstring").get (),
		"gcc/Common-Variable-Attributes.html"
		"#index-nonstring-variable-attribute");

  /* Example of target-specific attributes.
     For example, "interrupt" has many target-specific documentation URLs.  */
  {
    attribute_urlifier u_rl78 ("RL78");
    attribute_urlifier u_x86 ("x86");
    attribute_urlifier u_unrecognized ("not-a-target");

    ASSERT_STREQ (u_rl78.get_url_suffix_for_quoted_text ("interrupt").get (),
		  "gcc/RL78-Function-Attributes.html"
		  "#index-interrupt-function-attribute_002c-RL78");
    ASSERT_STREQ (u_x86.get_url_suffix_for_quoted_text ("interrupt").get (),
		  "gcc/x86-Function-Attributes.html"
		  "#index-interrupt-function-attribute_002c-x86");
    ASSERT_STREQ (u_unrecognized.get_url_suffix_for_quoted_text
		    ("interrupt").get (),
		  "gcc/Common-Function-Attributes.html"
		  "#index-interrupt-function-attribute");
  }
}

/* Run all of the selftests within this file.  */

void
gcc_attribute_urlifier_cc_tests ()
{
  test_attribute_urlifier ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
