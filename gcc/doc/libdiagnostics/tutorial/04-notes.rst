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

Tutorial part 4: adding notes
=============================

Let's further extend the previous example to add a "note" to it.

We want to generate output like this::

   test-with-note.c:17:11: error: can't find 'foo'
   17 | #include <foo.h>
      |           ^~~~~
   test-with-note.c:17:11: note: have you looked behind the couch?

The "error" and "note" are both instances of :type:`diagnostic`.
We want to let libdiagnostics know that they are grouped together.
The way to do this is to use :func:`diagnostic_manager_begin_group`
and :func:`diagnostic_manager_end_group` around the "finish" calls
to the diagnostics.

.. literalinclude:: ../../../testsuite/libdiagnostics.dg/test-error-with-note.c
   :language: c
   :start-after: /* begin quoted source */
   :end-before:  /* end quoted source */

On compiling and running the program, we should get the desired output::

   test-with-note.c:17:11: error: can't find 'foo'
   17 | #include <foo.h>
      |           ^~~~~
   test-with-note.c:17:11: note: have you looked behind the couch?

The grouping doesn't affect text output sinks, but a
:doc:`SARIF sink <../topics/sarif>` will group the note within the error
(via the ``relatedLocations`` property of ``result`` objects; see SARIF v2.1.0
`§3.27.22 <https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790910>`_).

In the above, the note had the same physical location as the error
(``loc_range``).  This can be useful for splitting up a message into two
parts to make localization easier, but they could have different locations, such
as in::

  test.xml:10:2: error: 'foo' is not valid here
  test.xml:5:1: note: within element 'bar'

where each :type:`diagnostic` had its own :type:`diagnostic_physical_location`.

In :doc:`the next tutorial <05-warnings>` we'll look at issuing warnings,
rather than errors.
