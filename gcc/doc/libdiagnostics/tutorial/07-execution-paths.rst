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

Tutorial part 7: execution paths
================================

A :type:`diagnostic` can optionally have a :type:`diagnostic_execution_path`
describing a path of execution through code.

For example, let's pretend we're writing a static analyis tool for finding
bugs in `CPython extension code <https://docs.python.org/3/c-api/index.html>`_.

Let's say we're analyzing this code:

.. literalinclude:: ../../../testsuite/libdiagnostics.dg/test-warning-with-path.c
   :language: c
   :start-after: begin fake source
   :end-before:  end fake source

This code attempts to take an Python integer parameter and then build a
list of that length, containing random integers.  However, there are
**numerous bugs** in this code: a type mismatch, mistakes in
reference-counting, and an almost total lack of error-handling.

For example, ``PyList_Append`` requires a non-NULL first parameter (``list``),
but ``PyList_New`` can fail, returning NULL, and this isn't checked for,
which would lead to a segfault if ``PyList_New`` fails.

We can add a :type:`diagnostic_execution_path` to the :type:`diagnostic`
via :func:`diagnostic_add_execution_path`, and then add events to it
using :func:`diagnostic_execution_path_add_event`.

For example, with::

  diagnostic_event_id alloc_event_id
    = diagnostic_execution_path_add_event (path,
					   loc_call_to_PyList_New,
					   logical_loc, 0,
					   "when %qs fails, returning NULL",
					   "PyList_New");

we create an event that will be worded as::

  (1) when `PyList_New' fails, returning NULL

Note that :func:`diagnostic_execution_path_add_event` returns a
:type:`diagnostic_event_id`.  We can use this to refer to this event
in another event using the ``%@`` format code in its message, which
takes the address of a :type:`diagnostic_event_id`::

  diagnostic_execution_path_add_event (path,
				       loc_call_to_PyList_Append,
				       logical_loc, 0,
				       "when calling %qs, passing NULL from %@ as argument %i",
				       "PyList_Append", &alloc_event_id, 1);

where the latter event will be worded as::

  (2) when calling `PyList_Append', passing NULL from (1) as argument 1

where the ``%@`` reference to the other event has been printed as ``(1)``.
In SARIF output the text "(1)" will have a embedded link referring within the sarif
log to the ``threadFlowLocation`` object for the other event, via JSON
pointer (see `§3.10.3 "URIs that use the sarif scheme" <https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790707>`_).

Let's add an event between these describing control flow, creating three
events in all:

.. literalinclude:: ../../../testsuite/libdiagnostics.dg/test-warning-with-path.c
   :language: c
   :start-after: begin path creation
   :end-before:  end path creation

Assuming we also gave it :type:`diagnostic_logical_location` with:

.. literalinclude:: ../../../testsuite/libdiagnostics.dg/test-warning-with-path.c
   :language: c
   :start-after: begin create logical locs
   :end-before:  end create logical locs

and finish the :type:`diagnostic` with :func:`diagnostic_finish` like this::

  diagnostic_finish (d,
		     "passing NULL as argument %i to %qs"
		     " which requires a non-NULL parameter",
		     1, "PyList_Append");

then we should get output to text sinks similar to the following::

   In function 'make_a_list_of_random_ints_badly':
   test-warning-with-path.c:30:5: warning: passing NULL as argument 1 to 'PyList_Append' which requires a non-NULL parameter"
      30 |     PyList_Append(list, item);
         |     ^~~~~~~~~~~~~~~~~~~~~~~~~
   make_a_list_of_random_ints_badly': events 1-3
      26 |   list = PyList_New(0);
         |          ^~~~~~~~~~~~~
         |          |
         |          (1) when 'PyList_New' fails, returning NULL
      27 | 
      28 |   for (i = 0; i < count; i++) {
         |               ~~~~~~~~~
         |               |
         |               (2) when 'i < count'
      29 |     item = PyLong_FromLong(random());
      30 |     PyList_Append(list, item);
         |     ~~~~~~~~~~~~~~~~~~~~~~~~~
         |     |
         |     (3) when calling 'PyList_Append', passing NULL from (1) as argument 1

and for SARIF sinks the path will be added as a ``codeFlow`` object
(see SARIF 2.1.0 `3.36 codeFlow object <https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790990>`_).

Here's the above example in full:

.. literalinclude:: ../../../testsuite/libdiagnostics.dg/test-warning-with-path.c
   :language: c
   :start-after: begin full example
   :end-before:  end full example


Moving on
*********

That's the end of the tutorial.  For more information on libdiagnostics, see
the :doc:`topic guide <../topics/index>`.
