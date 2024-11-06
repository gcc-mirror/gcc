/* Automatic generation of links into GCC's documentation.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.
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
#include "selftest.h"
#include "make-unique.h"

namespace {

/* Concrete subclass of urlifier for generating links into
   GCC's HTML documentation.  */

class gcc_urlifier : public urlifier
{
public:
  gcc_urlifier (unsigned int lang_mask)
  : m_lang_mask (lang_mask)
  {}

  char *get_url_for_quoted_text (const char *p, size_t sz) const final override;

  label_text get_url_suffix_for_quoted_text (const char *p, size_t sz) const;
  /* We use ATTRIBUTE_UNUSED as this helper is called only from ASSERTs.  */
  label_text get_url_suffix_for_quoted_text (const char *p) const ATTRIBUTE_UNUSED;

private:
  label_text get_url_suffix_for_option (const char *p, size_t sz) const;

  static char *
  make_doc_url (const char *doc_url_suffix);

  unsigned int m_lang_mask;
};

/* class gcc_urlifier : public urlifier.  */

/* Manage a hard-coded mapping from quoted string to URL suffixes
   in gcc-urlifier.def  */

#define DOC_URL(QUOTED_TEXT, URL_SUFFIX) \
  { (QUOTED_TEXT), (URL_SUFFIX) }

static const struct
{
  const char *quoted_text;
  const char *url_suffix;
} doc_urls[] = {

#include "gcc-urlifier.def"

};

/* Implementation of urlifier::get_url_for_quoted_text vfunc for GCC
   diagnostics.  */

char *
gcc_urlifier::get_url_for_quoted_text (const char *p, size_t sz) const
{
  label_text url_suffix = get_url_suffix_for_quoted_text (p, sz);
  if (url_suffix.get ())
    return make_doc_url (url_suffix.get ());
  return nullptr;
}

/* Look for a URL for the quoted string (P, SZ).
   Return the url suffix if found, or nullptr otherwise.  */

label_text
gcc_urlifier::get_url_suffix_for_quoted_text (const char *p, size_t sz) const
{
  if (sz == 0)
    return label_text ();

  /* If this is an option, look up the option and see if we have
     a URL for it.  */
  if (p[0] == '-')
    {
      label_text suffix = get_url_suffix_for_option (p, sz);
      if (suffix.get ())
	return suffix;
    }

  /* Otherwise, look within the hard-coded data table in gcc-urlifier.def.

     Binary search.  This assumes that the quoted_text fields of doc_urls
     are in sorted order.  */
  int min = 0;
  int max = ARRAY_SIZE (doc_urls) - 1;
  while (true)
    {
      if (min > max)
	return label_text ();
      int midpoint = (min + max) / 2;
      gcc_assert ((size_t)midpoint < ARRAY_SIZE (doc_urls));
      int cmp = strncmp (p, doc_urls[midpoint].quoted_text, sz);
      if (cmp == 0)
	{
	  if (doc_urls[midpoint].quoted_text[sz] == '\0')
	    return label_text::borrow (doc_urls[midpoint].url_suffix);
	  else
	    max = midpoint - 1;
	}
      else if (cmp < 0)
	max = midpoint - 1;
      else
	min = midpoint + 1;
    }

  /* Not found.  */
  return label_text ();
}

/* For use in selftests.  */

label_text
gcc_urlifier::get_url_suffix_for_quoted_text (const char *p) const
{
  return get_url_suffix_for_quoted_text (p, strlen (p));
}

/* Look for a URL for the quoted string (P, SZ) that appears to be
   an option.
   Return the url suffix if found, or nullptr otherwise.  */

label_text
gcc_urlifier::get_url_suffix_for_option (const char *p, size_t sz) const
{
  /* Look up this option

     find_opt does a binary search, taking a 0-terminated string,
     and skipping the leading '-'.

     We have a (pointer,size) pair that doesn't necessarily have a
     terminator.
     Additionally, we could have one of the e.g. "-Wno-" variants of
     the option, which find_opt doesn't handle.

     Hence we need to create input for find_opt in a temporary buffer.  */
  char *option_buffer;

  const char *new_prefix;
  if (const char *old_prefix = get_option_prefix_remapping (p, sz, &new_prefix))
    {
      /* We have one of the variants; generate a buffer containing a copy
	 that maps from the old prefix to the new prefix
	 e.g. given "-Wno-suffix", generate "-Wsuffix".  */
      gcc_assert (old_prefix[0] == '-');
      gcc_assert (new_prefix);
      gcc_assert (new_prefix[0] == '-');

      const size_t old_prefix_len = strlen (old_prefix);
      gcc_assert (old_prefix_len <= sz);
      const size_t suffix_len = sz - old_prefix_len;
      const size_t new_prefix_len = strlen (new_prefix);
      const size_t new_sz = new_prefix_len + suffix_len + 1;

      option_buffer = (char *)xmalloc (new_sz);
      memcpy (option_buffer, new_prefix, new_prefix_len);
      /* Copy suffix.  */
      memcpy (option_buffer + new_prefix_len, p + old_prefix_len, suffix_len);
      /* Terminate.  */
      option_buffer[new_prefix_len + suffix_len] = '\0';
    }
  else
    {
      /* Otherwise we can simply create a 0-terminated clone of the string.  */
      gcc_assert (sz > 0);
      gcc_assert (p[0] == '-');
      option_buffer = xstrndup (p, sz);
    }

  size_t opt = find_opt (option_buffer + 1, m_lang_mask);
  free (option_buffer);

  if (opt >= N_OPTS)
    /* Option not recognized.  */
    return label_text ();

  return get_option_url_suffix (opt, m_lang_mask);
}

char *
gcc_urlifier::make_doc_url (const char *doc_url_suffix)
{
  if (!doc_url_suffix)
    return nullptr;

  return concat (DOCUMENTATION_ROOT_URL, doc_url_suffix, nullptr);
}

} // anonymous namespace

std::unique_ptr<urlifier>
make_gcc_urlifier (unsigned int lang_mask)
{
  return ::make_unique<gcc_urlifier> (lang_mask);
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

  gcc_urlifier u (0);

  ASSERT_EQ (u.get_url_suffix_for_quoted_text ("").get (), nullptr);
  ASSERT_EQ (u.get_url_suffix_for_quoted_text (")").get (), nullptr);

  ASSERT_STREQ (u.get_url_suffix_for_quoted_text ("#pragma message").get (),
		"gcc/Diagnostic-Pragmas.html");

  // Incomplete prefix of a quoted_text
  ASSERT_EQ (u.get_url_suffix_for_quoted_text ("#pragma mess").get (), nullptr);

  /* Check that every element is findable.  */
  for (size_t idx = 0; idx < ARRAY_SIZE (doc_urls); idx++)
    ASSERT_STREQ
      (u.get_url_suffix_for_quoted_text (doc_urls[idx].quoted_text).get (),
       doc_urls[idx].url_suffix);

  /* Check an option.  */
  ASSERT_STREQ (u.get_url_suffix_for_quoted_text ("-fpack-struct").get (),
		"gcc/Code-Gen-Options.html#index-fpack-struct");

  /* Check a "-fno-" variant of an option.  */
  ASSERT_STREQ (u.get_url_suffix_for_quoted_text ("-fno-inline").get (),
		"gcc/Optimize-Options.html#index-finline");
}

} // namespace selftest

#endif /* #if CHECKING_P */
