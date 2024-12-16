.. Copyright (C) 2024 Free Software Foundation, Inc.
   Originally contributed by David Malcolm <dmalcolm@redhat.com>

   This is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see
   <https://www.gnu.org/licenses/>.

.. default-domain:: c

Logical locations
=================

A "logical" location is a location expressed in terms of
construct in a programming language, such as ``within function 'foo'``
(as opposed to a :doc:`"physical" location <physical-locations>`, which
refers to a specific file, and line(s) and/or column(s))

Creating location information
*****************************

.. type:: diagnostic_logical_location

A :type:`diagnostic_logical_location` is an opaque type describing a "logical"
source location

.. function:: const diagnostic_logical_location * diagnostic_manager_new_logical_location (diagnostic_manager *diag_mgr, \
                                                                                           enum diagnostic_logical_location_kind_t kind, \
                                                                                           const diagnostic_logical_location *parent, \
                                                                                           const char *short_name, \
                                                                                           const char *fully_qualified_name, \
                                                                                           const char *decorated_name)

   Create a :type:`diagnostic_logical_location`.

   ``diag_mgr`` must be non-NULL.

   ``kind`` describes the kind of logical location:

   .. enum:: diagnostic_logical_location_kind_t

      This roughly corresponds to the ``kind`` property in SARIF v2.1.0
      (`§3.33.7 <https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790976>`_).

      .. macro:: DIAGNOSTIC_LOGICAL_LOCATION_KIND_FUNCTION

      .. macro:: DIAGNOSTIC_LOGICAL_LOCATION_KIND_MEMBER

      .. macro:: DIAGNOSTIC_LOGICAL_LOCATION_KIND_MODULE

      .. macro:: DIAGNOSTIC_LOGICAL_LOCATION_KIND_NAMESPACE

      .. macro:: DIAGNOSTIC_LOGICAL_LOCATION_KIND_TYPE

      .. macro:: DIAGNOSTIC_LOGICAL_LOCATION_KIND_RETURN_TYPE

      .. macro:: DIAGNOSTIC_LOGICAL_LOCATION_KIND_PARAMETER

      .. macro:: DIAGNOSTIC_LOGICAL_LOCATION_KIND_VARIABLE

   ``parent`` can be NULL; if non-NULL it can be used to express tree-like
   nesting of logical locations, such as in::

     namespace foo { namespace bar { class baz { baz (); }; } }

   where a diagnostic within ``baz``'s constructor could be reported
   as being within ``foo::bar::baz::baz`` where the logical locations
   are two namespaces, a type, and a member, respectively.

   ``short_name`` can be NULL, or else a string suitable for use by
   the SARIF logicalLocation ``name`` property
   (SARIF v2.1.0 `§3.33.4 <https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790973>`_).

   ``fully_qualified_name`` can be NULL or else a string  suitable for use by
   the SARIF logicalLocation ``fullyQualifiedName`` property
   (SARIF v2.1.0 `§3.33.5 <https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790974>`_).

   ``decorated_name`` can be NULL or else a string suitable for use by
   the SARIF logicalLocation ``decoratedName`` property
   (SARIF v2.1.0 `§3.33.6 <https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790975>`_).

   Repeated calls to :func:`diagnostic_manager_new_logical_location` with
   "equal" input values on the same :type:`diagnostic_manager` will return
   the same instance of :type:`diagnostic_logical_location`.  "Equal" here
   includes different string buffers that compare as equal with
   :func:``strcmp`.

.. function:: void diagnostic_manager_debug_dump_logical_location (const diagnostic_manager *diag_mgr, \
                                                                   const diagnostic_logical_location *loc, \
                                                                   FILE *out)

   Write a representation of ``file`` to ``out``, for debugging.
   Both ``diag_mgr`` and ``out`` must be non-NULL.
   ``file`` may be NULL.

   TODO: example of output

Associating diagnostics with locations
**************************************

.. function:: void diagnostic_set_logical_location (diagnostic *diag, \
                                                    const diagnostic_logical_location *logical_loc)

    Set the logical location of ``diag``.

    ``diag`` must be non-NULL; ``logical_loc`` can be NULL.
