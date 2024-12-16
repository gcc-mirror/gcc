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

Tutorial part 2: physical locations
===================================

libgdiagnostics has two kinds of location:

* *physical locations* expressed in terms of a specific file, and line(s)
  and perhaps column(s), such as ``some-file.c:3:1``, or a range of
  columns, such as in::

    test-typo.c:19:13: error: unknown field 'colour'
       19 |   return p->colour;
          |             ^~~~~~

  or even a range spanning multiple lines of a file.

  All of these are instances of :type:`diagnostic_physical_location`.

* *logical locations* which refers to semantic constructs
  in the input, such as ``within function 'foo'``, or within
  namespace ``foo``'s class ``bar``'s member function ``get_color``.

  These are instances of :type:`diagnostic_logical_location`,

A :type:`diagnostic` can have zero or more physical locations,
and optionally have a logical location.

Let's extend the previous example to add a physical location to the
:type:`diagnostic`; we'll cover logical locations in the
:doc:`next section <03-logical-locations>`.


Source files
************

Given these declarations::

  static diagnostic_manager *diag_mgr;
  static diagnostic_file *main_file;

we can create a :type:`diagnostic_file` describing an input file ``foo.c``
via :func:`diagnostic_manager_new_file`::

     foo_c = diagnostic_manager_new_file (diag_mgr,
                                          "foo.c",
                                          "c" /* source_language */);

You can use :func:`diagnostic_manager_debug_dump_file` to print a
representation of a :type:`diagnostic_file` for debugging.
For example::

   diagnostic_manager_debug_dump_file (diag_mgr, foo_c, stderr);

might lead to this output on ``stderr``::

   file(name="foo.c", sarif_source_language="c")

Once we have a :type:`diagnostic_file` we can use it to create instances
of :type:`diagnostic_physical_location` within the :type:`diagnostic_manager`.
These are owned by the :type:`diagnostic_manager` and cleaned up
automatically when :func:`diagnostic_manager_release` is called.

Instances of :type:`diagnostic_physical_location` can refer to

* a source line as a whole, created via
  :func:`diagnostic_manager_new_location_from_file_and_line`.

* a particular point within a source file (line/column), created via
  :func:`diagnostic_manager_new_location_from_file_line_column`.

* a range of text within of source file, created via
  :func:`diagnostic_manager_new_location_from_range`.


Diagnostics affecting a whole source line
*****************************************

If we want a diagnostic to refer to an entire source line,
we can use :func:`diagnostic_manager_new_location_from_file_and_line`.

For example, given this example input where the tool can't find the header::

   #include <foo.h>

we could complain about it via libgdiagnostics via:

.. literalinclude:: ../../../testsuite/libgdiagnostics.dg/test-no-column.c
   :language: c
   :start-after: /* begin quoted source */
   :end-before:  /* end quoted source */

leading to output like this::

   foo.c:17: error: can't find 'foo.h'"
      17 | #include <foo.h>

where libgdiagnostics will attempt to load the source file and
quote the pertinent line.

If libgdiagnostics cannot open the file, it will merely print::

   foo.c:17: error: can't find 'foo.h'

You can use :func:`diagnostic_manager_debug_dump_location` to dump a
:type:`diagnostic_physical_location`.  For the above example::

   diagnostic_manager_debug_dump_location (diag_mgr, loc, stderr);

might print::

   foo.c:17

to stderr.


Columns and ranges
******************

If we want to generate output like this::

   foo.c:17:11: error: can't find 'foo'"
      17 | #include <foo.h>
         |           ^~~~~

where the diagnostic is marked as relating to the above range of
characters in line 17, we need to express the range of characters
within the line of interest.

We can do this by creating a :type:`diagnostic_physical_location` for the
start of the range, another one for the end of the range, and then using
these two to create a :type:`diagnostic_physical_location` for the
range as a whole:

.. literalinclude:: ../../../testsuite/libgdiagnostics.dg/test-error.c
   :language: c
   :start-after: /* begin quoted source */
   :end-before:  /* end quoted source */

On compiling and running the program, we should get this output::

   foo.c:17:11: error: can't find 'foo.h'
      17 | #include <foo.h>
         |           ^~~~~

where libgdiagnostics will attempt to load the source file and
underling the pertinent part of the given line.

If libgdiagnostics cannot open the file, it will merely print::

   foo.c:17:8: error: can't find 'foo.h'

A range can span multiple lines within the same file.

As before, you can use :func:`diagnostic_manager_debug_dump_location` to
dump the locations.  For the above example::

   diagnostic_manager_debug_dump_location (diag_mgr, loc_start, stderr);

and::

   diagnostic_manager_debug_dump_location (diag_mgr, loc_range, stderr);

might print::

   foo.c:17:11

to stderr, whereas::

   diagnostic_manager_debug_dump_location (diag_mgr, loc_end, stderr);

might print::

   foo.c:17:15


Multiple locations
******************

As well as the primary physical location seen above, a :type:`diagnostic`
can have additional physical locations.  You can add these secondary
locations via :func:`diagnostic_add_location`.

For example, for this valid but suspicious-looking C code::

   const char *strs[3] = {"foo",
                          "bar"
                          "baz"};

the following :type:`diagnostic` has its primary location where the missing
comma should be, and secondary locations for each of the string literals
``"foo"``, ``"bar"``, and ``"baz"``, added via :func:`diagnostic_add_location`:

.. literalinclude:: ../../../testsuite/libgdiagnostics.dg/test-multiple-lines.c
   :language: c
   :start-after: /* begin quoted source */
   :end-before:  /* end quoted source */

where the text output might be::

   test-multiple-lines.c:23:29: warning: missing comma
      22 | const char *strs[3] = {"foo",
         |                        ~~~~~
      23 |                        "bar"
         |                        ~~~~~^
      24 |                        "baz"};
         |                        ~~~~~


Labelling locations
*******************

You can give the locations labels using
:func:`diagnostic_set_location_with_label` and
:func:`diagnostic_add_location_with_label`.

Consider emitting a "type mismatch" diagnostic for::

  42 + "foo"

where the primary location is on the ``+``, with secondary locations on the``42``
and the ``"foo"``:

.. literalinclude:: ../../../testsuite/libgdiagnostics.dg/test-labelled-ranges.c
   :language: c
   :start-after: /* begin quoted source */
   :end-before:  /* end quoted source */

giving this text output::

   test-labelled-ranges.c:9:6: error: mismatching types: 'int' and 'const char *'
      19 |   42 + "foo"
         |   ~~ ^ ~~~~~
         |   |    |
         |   int  const char *


More on locations
*****************

For more details on the above, see :doc:`../topics/physical-locations`.
Otherwise the :doc:`next part of the tutorial <03-logical-locations>`
covers logical locations.
