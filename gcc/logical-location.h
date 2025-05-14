/* Logical location support, without knowledge of "tree".
   Copyright (C) 2022-2025 Free Software Foundation, Inc.
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

#ifndef GCC_LOGICAL_LOCATION_H
#define GCC_LOGICAL_LOCATION_H

#include "label-text.h"

/* An enum for discriminating between different kinds of logical location
   for a diagnostic.

   Roughly corresponds to logicalLocation's "kind" property in SARIF v2.1.0
   (section 3.33.7).  */

enum logical_location_kind
{
  LOGICAL_LOCATION_KIND_UNKNOWN,

  /* Kinds within executable code.  */
  LOGICAL_LOCATION_KIND_FUNCTION,
  LOGICAL_LOCATION_KIND_MEMBER,
  LOGICAL_LOCATION_KIND_MODULE,
  LOGICAL_LOCATION_KIND_NAMESPACE,
  LOGICAL_LOCATION_KIND_TYPE,
  LOGICAL_LOCATION_KIND_RETURN_TYPE,
  LOGICAL_LOCATION_KIND_PARAMETER,
  LOGICAL_LOCATION_KIND_VARIABLE,

  /* Kinds within XML or HTML documents.  */
  LOGICAL_LOCATION_KIND_ELEMENT,
  LOGICAL_LOCATION_KIND_ATTRIBUTE,
  LOGICAL_LOCATION_KIND_TEXT,
  LOGICAL_LOCATION_KIND_COMMENT,
  LOGICAL_LOCATION_KIND_PROCESSING_INSTRUCTION,
  LOGICAL_LOCATION_KIND_DTD,
  LOGICAL_LOCATION_KIND_DECLARATION,

  /* Kinds within JSON documents.  */
  LOGICAL_LOCATION_KIND_OBJECT,
  LOGICAL_LOCATION_KIND_ARRAY,
  LOGICAL_LOCATION_KIND_PROPERTY,
  LOGICAL_LOCATION_KIND_VALUE
};

/* We want to efficiently support passing around logical locations in the
   diagnostics subsystem, such as:
   - "within function 'foo'", or
   - "within method 'bar'"

   However we want to do this *without* requiring knowledge of trees (or of
   libgdiagnostics internals), and without requiring heap allocation of an
   interface class when emitting a diagnostic.

   To do this, we split the implementation into logical_location, which is
   a wrapper around a (const void *), and logical_location_manager which
   is provided by the client and has vfunc hooks for interpreting
   logical_location instances.

   Every logical_location is associated with a logical_location_manager and
   only has meaning in relation to that manager.

   A "nullptr" within a logical_location means "no logical location".

   See tree-logical-location.h for concrete subclasses relating to trees,
   where the pointer is a const_tree.

   See selftest-logical-location.h for a concrete subclass for selftests.  */

/* Abstract base class for giving meaning to logical_location values.
   Typically there will just be one client-provided instance, of a
   client-specific subclass.  */

class logical_location_manager
{
public:
  /* Extrinsic state for identifying a specific logical location.
     This will be our logical_location type.
     This only makes sense with respect to a specific manager.
     e.g. for a tree-based one it's a wrapper around "tree".
     "nullptr" means "no logical location".  */
  class key
  {
  public:
    key () : m_ptr (nullptr) {}

    static key from_ptr (const void *ptr)
    {
      return key (ptr);
    }

    operator bool () const
    {
      return m_ptr != nullptr;
    }

    template <typename T>
    T cast_to () const { return static_cast<T> (m_ptr); }

    bool
    operator== (const key &other) const
    {
      return m_ptr == other.m_ptr;
    }

    bool
    operator!= (const key &other) const
    {
      return m_ptr != other.m_ptr;
    }

    bool
    operator< (const key &other) const
    {
      return m_ptr < other.m_ptr;
    }

  private:
    explicit key (const void *ptr) : m_ptr (ptr) {}

    const void *m_ptr;
  };

  virtual ~logical_location_manager () {}

  /* vfuncs for interpreting logical_location values.  */

  /* Get a string (or NULL) for K suitable for use by the SARIF logicalLocation
     "name" property (SARIF v2.1.0 section 3.33.4).  */
  virtual const char *get_short_name (key k) const = 0;

  /* Get a string (or NULL) for K suitable for use by the SARIF logicalLocation
     "fullyQualifiedName" property (SARIF v2.1.0 section 3.33.5).  */
  virtual const char *get_name_with_scope (key k) const = 0;

  /* Get a string (or NULL) for K suitable for use by the SARIF logicalLocation
     "decoratedName" property (SARIF v2.1.0 section 3.33.6).  */
  virtual const char *get_internal_name (key k) const = 0;

  /* Get what kind of SARIF logicalLocation K is (if any).  */
  virtual enum logical_location_kind get_kind (key k) const = 0;

  /* Get a string for location K in a form suitable for path output.  */
  virtual label_text get_name_for_path_output (key k) const = 0;

  /* Get the parent logical_logical of K, if any, or nullptr.  */
  virtual key get_parent (key k) const = 0;

  bool function_p (key k) const;
};

/* A logical location is a key for a given logical_location_manager.

   Note that there is no integration with GCC's garbage collector and thus
   logical_location instances can't be long-lived.  */

typedef logical_location_manager::key logical_location;

#endif /* GCC_LOGICAL_LOCATION_H.  */
