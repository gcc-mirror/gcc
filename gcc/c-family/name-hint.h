/* Support for offering suggestions for handling unrecognized names.
   Copyright (C) 2016-2024 Free Software Foundation, Inc.

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

#ifndef GCC_NAME_HINT_H
#define GCC_NAME_HINT_H

/* This header uses std::unique_ptr, but <memory> can't be directly
   included due to issues with macros.  Hence it must be included from
   system.h by defining INCLUDE_MEMORY in any source file using it.  */

#ifndef INCLUDE_MEMORY
# error "You must define INCLUDE_MEMORY before including system.h to use name-hint.h"
#endif

enum lookup_name_fuzzy_kind {
  /* Names of types.  */
  FUZZY_LOOKUP_TYPENAME,

  /* Names of function decls.  */
  FUZZY_LOOKUP_FUNCTION_NAME,

  /* Any name.  */
  FUZZY_LOOKUP_NAME
};

/* A deferred_diagnostic is a wrapper around optional extra diagnostics
   that we may want to bundle into a name_hint.

   The diagnostic is emitted by the subclass destructor, which should
   check that is_suppressed_p () is not true.  */

class deferred_diagnostic
{
 public:
  virtual ~deferred_diagnostic () {}

  location_t get_location () const { return m_loc; }

  /* Call this if the corresponding warning was not emitted,
     in which case we should also not emit the deferred_diagnostic.  */
  void suppress ()
  {
    m_suppress = true;
  }

  bool is_suppressed_p () const { return m_suppress; }

 protected:
  deferred_diagnostic (location_t loc)
  : m_loc (loc), m_suppress (false) {}

 private:
  location_t m_loc;
  bool m_suppress;
};

/* A name_hint is an optional string suggestion, along with an
   optional deferred_diagnostic.
   For example:

       error: unknown foo named 'bar'

   if the SUGGESTION is "baz", then one might print:

       error: unknown foo named 'bar'; did you mean 'baz'?

   and the deferred_diagnostic allows for additional (optional)
   diagnostics e.g.:

       note: did you check behind the couch?

   The deferred_diagnostic is emitted by its destructor, when the
   name_hint goes out of scope.  */

class name_hint
{
public:
  name_hint () : m_suggestion (NULL), m_deferred () {}

  name_hint (const char *suggestion, deferred_diagnostic *deferred)
  : m_suggestion (suggestion), m_deferred (deferred)
  {
  }

  const char *suggestion () const { return m_suggestion; }

  /* Does this name_hint have a suggestion or a deferred diagnostic?  */
  operator bool () const { return (m_suggestion != NULL
				   || m_deferred != NULL); }

  /* Take ownership of this name_hint's deferred_diagnostic, for use
     in chaining up deferred diagnostics.  */
  std::unique_ptr<deferred_diagnostic> take_deferred () { return std::move (m_deferred); }

  /* Call this on a name_hint if the corresponding warning was not emitted,
     in which case we should also not emit the deferred_diagnostic.  */

  void suppress ()
  {
    if (m_deferred)
      m_deferred->suppress ();
  }

private:
  const char *m_suggestion;
  std::unique_ptr<deferred_diagnostic> m_deferred;
};

extern name_hint lookup_name_fuzzy (tree, enum lookup_name_fuzzy_kind,
				    location_t);

#endif /* ! GCC_NAME_HINT_H */
