/* Automatic generation of links into GCC's documentation.
   Copyright (C) 2023 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "pretty-print.h"
#include "pretty-print-urlifier.h"
#include "gcc-urlifier.h"
#include "selftest.h"

namespace {

/* Concrete subclass of urlifier for generating links into
   GCC's HTML documentation.  */

class gcc_urlifier : public urlifier
{
public:
  char *get_url_for_quoted_text (const char *p, size_t sz) const final override;

  const char *get_url_suffix_for_quoted_text (const char *p, size_t sz) const;
  /* We use ATTRIBUTE_UNUSED as this helper is called only from ASSERTs.  */
  const char *get_url_suffix_for_quoted_text (const char *p) const ATTRIBUTE_UNUSED;

private:
  static char *
  make_doc_url (const char *doc_url_suffix);
};

/* class gcc_urlifier : public urlifier.  */

#define DOC_URL(QUOTED_TEXT, URL_SUFFIX) \
  { (QUOTED_TEXT), (URL_SUFFIX) }

const struct
{
  const char *quoted_text;
  const char *url_suffix;
} doc_urls[] = {

#include "gcc-urlifier.def"

};

char *
gcc_urlifier::get_url_for_quoted_text (const char *p, size_t sz) const
{
  if (const char *url_suffix = get_url_suffix_for_quoted_text (p, sz))
    return make_doc_url (url_suffix);
  return nullptr;
}

const char *
gcc_urlifier::get_url_suffix_for_quoted_text (const char *p, size_t sz) const
{
  /* Binary search.  This assumes that the quoted_text fields of doc_urls
     are in sorted order.  */
  int min = 0;
  int max = ARRAY_SIZE (doc_urls) - 1;
  while (true)
    {
      if (min > max)
	return nullptr;
      int midpoint = (min + max) / 2;
      gcc_assert ((size_t)midpoint < ARRAY_SIZE (doc_urls));
      int cmp = strncmp (p, doc_urls[midpoint].quoted_text, sz);
      if (cmp == 0)
	{
	  if (doc_urls[midpoint].quoted_text[sz] == '\0')
	    return doc_urls[midpoint].url_suffix;
	  else
	    max = midpoint - 1;
	}
      else if (cmp < 0)
	max = midpoint - 1;
      else
	min = midpoint + 1;
    }
  return nullptr;
}

const char *
gcc_urlifier::get_url_suffix_for_quoted_text (const char *p) const
{
  return get_url_suffix_for_quoted_text (p, strlen (p));
}

char *
gcc_urlifier::make_doc_url (const char *doc_url_suffix)
{
  if (!doc_url_suffix)
    return nullptr;

  return concat (DOCUMENTATION_ROOT_URL, doc_url_suffix, nullptr);
}

} // anonymous namespace

urlifier *
make_gcc_urlifier ()
{
  return new gcc_urlifier ();
}

#if CHECKING_P

namespace selftest {

/* Selftests.  */

/* Run all of the selftests within this file.  */

void
gcc_urlifier_cc_tests ()
{
  /* Check that doc_urls.quoted_text is sorted.  */
  for (size_t idx = 1; idx < ARRAY_SIZE (doc_urls); idx++)
    gcc_assert (strcmp (doc_urls[idx - 1].quoted_text,
			doc_urls[idx].quoted_text)
		< 0);

  gcc_urlifier u;

  ASSERT_EQ (u.get_url_suffix_for_quoted_text (""), nullptr);
  ASSERT_EQ (u.get_url_suffix_for_quoted_text (")"), nullptr);

  ASSERT_STREQ (u.get_url_suffix_for_quoted_text ("#pragma message"),
		"gcc/Diagnostic-Pragmas.html");

  // Incomplete prefix of a quoted_text
  ASSERT_EQ (u.get_url_suffix_for_quoted_text ("#pragma mess"), nullptr);

  /* Check that every element is findable.  */
  for (size_t idx = 0; idx < ARRAY_SIZE (doc_urls); idx++)
    ASSERT_STREQ
      (u.get_url_suffix_for_quoted_text (doc_urls[idx].quoted_text),
       doc_urls[idx].url_suffix);
}

} // namespace selftest

#endif /* #if CHECKING_P */
