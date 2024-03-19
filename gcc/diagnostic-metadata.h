/* Additional metadata for a diagnostic.
   Copyright (C) 2019-2023 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>

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

#ifndef GCC_DIAGNOSTIC_METADATA_H
#define GCC_DIAGNOSTIC_METADATA_H

class sarif_object;

/* A bundle of additional metadata that can be associated with a
   diagnostic.

   This supports an optional CWE identifier, and zero or more
   "rules".  */

class diagnostic_metadata
{
 public:
  /* Abstract base class for referencing a rule that has been violated,
     such as within a coding standard, or within a specification.  */
  class rule
  {
  public:
    virtual char *make_description () const = 0;
    virtual char *make_url () const = 0;
  };

  /* Concrete subclass.  */
  class precanned_rule : public rule
  {
  public:
    precanned_rule (const char *desc, const char *url)
    : m_desc (desc), m_url (url)
    {}

    char *make_description () const final override
    {
      return m_desc ? xstrdup (m_desc) : NULL;
    }

    char *make_url () const final override
    {
      return m_url ? xstrdup (m_url) : NULL;
    }

  private:
    const char *m_desc;
    const char *m_url;
  };

  diagnostic_metadata () : m_cwe (0) {}
  virtual ~diagnostic_metadata () {}

  /* Hook for SARIF output to allow for adding diagnostic-specific
     properties to  the result object's property bag.  */
  virtual void
  maybe_add_sarif_properties (sarif_object &/*result_obj*/) const
  {
  }

  void add_cwe (int cwe) { m_cwe = cwe; }
  int get_cwe () const { return m_cwe; }

  /* Associate R with the diagnostic.  R must outlive
     the metadata.  */
  void add_rule (const rule &r)
  {
    m_rules.safe_push (&r);
  }

  unsigned get_num_rules () const { return m_rules.length (); }
  const rule &get_rule (unsigned idx) const { return *(m_rules[idx]); }

 private:
  int m_cwe;
  auto_vec<const rule *> m_rules;
};

#endif /* ! GCC_DIAGNOSTIC_METADATA_H */
