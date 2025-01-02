/* Automatic generation of links into GCC's documentation.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_GCC_URLIFIER_H
#define GCC_GCC_URLIFIER_H

#include "pretty-print-urlifier.h"
#include "label-text.h"

extern std::unique_ptr<urlifier> make_gcc_urlifier (unsigned int lang_mask);
extern char *make_doc_url (const char *doc_url_suffix);

/* RAII class to temporarily override global_dc's urlifier
   with another one (possibly nullptr).  */

class auto_override_urlifier
{
public:
  auto_override_urlifier (urlifier *new_urlifier);
  ~auto_override_urlifier ();

protected:
  urlifier * const m_old_urlifier;
};

/* Subclass of urlifier that attempts to add URLs to quoted strings
   containing names of attributes.  */

class attribute_urlifier : public urlifier
{
public:
  attribute_urlifier ();
  attribute_urlifier (const char *target_docs_name);

  char *
  get_url_for_quoted_text (const char *p, size_t sz) const final override;

  label_text
  get_url_suffix_for_quoted_text (const char *p, size_t sz) const;

  /* We use ATTRIBUTE_UNUSED as this helper is called only from ASSERTs.  */
  label_text
  get_url_suffix_for_quoted_text (const char *p) const ATTRIBUTE_UNUSED;

private:
  const char *m_target_docs_name;
};

/* RAII class: during the lifetime of instances, global_dc will attempt
   to auto-generate documentation links for any attributes mentioned in
   quotes in diagnostics .  */

class auto_urlify_attributes
{
public:
  auto_urlify_attributes ()
  : m_override (&m_urlifier)
  {
  }

private:
  attribute_urlifier m_urlifier;
  auto_override_urlifier m_override;
};

#endif /* GCC_GCC_URLIFIER_H */
