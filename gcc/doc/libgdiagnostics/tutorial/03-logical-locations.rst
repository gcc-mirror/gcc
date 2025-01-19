.. Copyright (C) 2024-2025 Free Software Foundation, Inc.
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

Tutorial part 3: logical locations
==================================

Let's extend the previous example to add a
:doc:`logical location <../topics/logical-locations>` to the
:type:`diagnostic`.

First we create a :type:`diagnostic_logical_location` representing a
particular function::

  const diagnostic_logical_location *logical_loc
    = diagnostic_manager_new_logical_location (diag_mgr,
					       DIAGNOSTIC_LOGICAL_LOCATION_KIND_FUNCTION,
					       NULL, /* parent */
					       "foo",
					       NULL,
					       NULL);

In this simple example we specify that it is a function, and just give
it a name (``foo``).  For more complicated cases we can set up tree-like
hierarchies of logical locations, set qualified names, "mangled" names,
and so on; see :func:`diagnostic_manager_new_logical_location` for details.
    
Once we have :type:`diagnostic_logical_location` we can associate it with
a :type:`diagnostic` with :func:`diagnostic_set_logical_location`::
      
  diagnostic_set_logical_location (d, logical_loc);

The logical location will be printed by text output sinks like this::

  In function 'foo':

and will be captured in :doc:`SARIF <../topics/sarif>` output.


Find out more
*************

For more details on the above, see :doc:`../topics/logical-locations`.
Otherwise the :doc:`next part of the tutorial <04-notes>` covers adding
supplementary "notes" to a :type:`diagnostic`.
