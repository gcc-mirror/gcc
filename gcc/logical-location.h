/* Logical location support, without knowledge of "tree".
   Copyright (C) 2022-2023 Free Software Foundation, Inc.
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

/* An enum for discriminating between different kinds of logical location
   for a diagnostic.

   Roughly corresponds to logicalLocation's "kind" property in SARIF v2.1.0
   (section 3.33.7).  */

enum logical_location_kind
{
  LOGICAL_LOCATION_KIND_UNKNOWN,

  LOGICAL_LOCATION_KIND_FUNCTION,
  LOGICAL_LOCATION_KIND_MEMBER,
  LOGICAL_LOCATION_KIND_MODULE,
  LOGICAL_LOCATION_KIND_NAMESPACE,
  LOGICAL_LOCATION_KIND_TYPE,
  LOGICAL_LOCATION_KIND_RETURN_TYPE,
  LOGICAL_LOCATION_KIND_PARAMETER,
  LOGICAL_LOCATION_KIND_VARIABLE
};

/* Abstract base class for passing around logical locations in the
   diagnostics subsystem, such as:
   - "within function 'foo'", or
   - "within method 'bar'",
   but *without* requiring knowledge of trees
   (see tree-logical-location.h for subclasses relating to trees).  */

class logical_location
{
public:
  virtual ~logical_location () {}

  /* Get a string (or NULL) suitable for use by the SARIF logicalLocation
     "name" property (SARIF v2.1.0 section 3.33.4).  */
  virtual const char *get_short_name () const = 0;

  /* Get a string (or NULL) suitable for use by the SARIF logicalLocation
     "fullyQualifiedName" property (SARIF v2.1.0 section 3.33.5).  */
  virtual const char *get_name_with_scope () const = 0;

  /* Get a string (or NULL) suitable for use by the SARIF logicalLocation
     "decoratedName" property (SARIF v2.1.0 section 3.33.6).  */
  virtual const char *get_internal_name () const = 0;

  /* Get what kind of SARIF logicalLocation this is (if any).  */
  virtual enum logical_location_kind get_kind () const = 0;
};

#endif /* GCC_LOGICAL_LOCATION_H.  */
