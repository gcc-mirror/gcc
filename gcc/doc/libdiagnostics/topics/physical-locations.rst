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

Physical locations
==================

A "physical" source location is a location expressed in terms of
a specific file, and line(s) and column(s) (as opposed to a
:doc:`"logical" location <logical-locations>`,
which refers to semantic constructs in a programming language).

Creating location information
*****************************

The :type:`diagnostic_manager` manages various objects relating to
locations.

.. type:: diagnostic_file

   A :type:`diagnostic_file` is an opaque type describing a particular input file.

.. function:: const diagnostic_file * diagnostic_manager_new_file (diagnostic_manager *diag_mgr, \
                                                                   const char *name, \
                                                                   const char *sarif_source_language)

   Create a new :type:`diagnostic_file` for file ``name``. Repeated calls
   with strings that match ``name`` will return the same object.

   Both ``diag_mgr`` and ``name`` must be non-NULL.

   If ``sarif_source_language`` is non-NULL, it specifies a
   ``sourceLanguage`` value for the file for use when writing
   :doc:`SARIF <sarif>`
   (`SARIF v2.1.0 §3.24.10 <https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790871>`_).
   See
   `SARIF v2.1.0 Appendix J <https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141791197>`_
   for suggested values for various programmming languages.

   For example, this creates a :type:`diagnostic_file` for ``foo.c``
   and identifies it as C source code::

     foo_c = diagnostic_manager_new_file (diag_mgr,
                                          "foo.c",
                                          "c" /* source_language */);

.. function::  void diagnostic_manager_debug_dump_file (diagnostic_manager *diag_mgr, \
                                                        const diagnostic_file *file, \
                                                        FILE *out)

   Write a representation of ``file`` to ``out``, for debugging.
   Both ``diag_mgr`` and ``out`` must be non-NULL.
   `file`` may be NULL.

   For example::

     diagnostic_manager_debug_dump_file (diag_mgr, foo_c, stderr);

   might lead to this output::

      file(name="foo.c", sarif_source_language="c")

.. type:: diagnostic_line_num_t

A :type:`diagnostic_line_num_t` is used for representing line numbers
within text files.  libdiagnostics treats the first line of a text file
as line 1.

.. type:: diagnostic_column_num_t

A :type:`diagnostic_column_num_t` is used for representing column numbers
within text files.  libdiagnostics treats the first column of a text line
as column 1, **not** column 0.

.. note::

   Both libdiagnostics and Emacs number source *lines* starting at 1, but
   they have differing conventions for *columns*.

   libdiagnostics uses a 1-based convention for source columns,
   whereas Emacs's ``M-x column-number-mode`` uses a 0-based convention.

   For example, an error in the initial, left-hand
   column of source line 3 is reported by libdiagnostics as::

     some-file.c:3:1: error: ...etc...

   On navigating to the location of that error in Emacs
   (e.g. via ``next-error``),
   the locus is reported in the Mode Line
   (assuming ``M-x column-number-mode``) as::

     some-file.c   10%   (3, 0)

   i.e. ``3:1:`` in libdiagnostics corresponds to ``(3, 0)`` in Emacs.

.. type:: diagnostic_physical_location

A :type:`diagnostic_physical_location` is an opaque type representing a
key into a database of source locations within a :type:`diagnostic_manager`.

:type:`diagnostic_physical_location` instances are created by various API
calls into the :type:`diagnostic_manager` expressing source code points
and ranges.

They persist until the :type:`diagnostic_manager` is released, which
cleans them up.

A ``NULL`` value means "unknown", and can be returned by the
:type:`diagnostic_manager` as a fallback when a problem occurs
(e.g. too many locations).

A :type:`diagnostic_physical_location` can be a single point within the
source code, such as here (at the the '"' at the start of the string literal)::

  int i = "foo";
          ^

or be a range with a start and finish, and a "caret" location::

   a = (foo && bar)
       ~~~~~^~~~~~~

where the caret here is at the first "&", and the start and finish
are at the parentheses.

.. function::  const diagnostic_physical_location *diagnostic_manager_new_location_from_file_and_line (diagnostic_manager *diag_mgr, \
                                                                                                       const diagnostic_file *file,  \
                                                                                                       diagnostic_line_num_t line_num)

   Attempt to create a :type:`diagnostic_physical_location` representing
   ``FILENAME:LINE_NUM``, with no column information (thus representing
   the whole of the given line.

   Both ``diag_mgr`` and ``file`` must be non-NULL.

.. function::  const diagnostic_physical_location * diagnostic_manager_new_location_from_file_line_column (diagnostic_manager *diag_mgr, \
                                                                                                           const diagnostic_file *file, \
                                                                                                           diagnostic_line_num_t line_num, \
                                                                                                           diagnostic_column_num_t column_num)

   Attempt to create a :type:`diagnostic_physical_location` for
   ``FILENAME:LINE_NUM:COLUMN_NUM`` representing a particular point
   in the source file.

   Both ``diag_mgr`` and ``file`` must be non-NULL.

.. function::  const diagnostic_physical_location *diagnostic_manager_new_location_from_range (diagnostic_manager *diag_mgr,\
                                                                                               const diagnostic_physical_location *loc_caret,\
                                                                                               const diagnostic_physical_location *loc_start,\
                                                                                               const diagnostic_physical_location *loc_end)

   Attempt to create a diagnostic_physical_location representing a
   range within a source file, with a highlighted "caret" location.

   All must be within the same file, but they can be on different lines.

   For example, consider the location of the binary expression below::

     ...|__________1111111112222222
     ...|12345678901234567890123456
     ...|
     521|int sum (int foo, int bar)
     522|{
     523|   return foo + bar;
     ...|          ~~~~^~~~~
     524|}

   The location's caret is at the "+", line 523 column 15, but starts
   earlier, at the "f" of "foo" at column 11.  The finish is at the "r"
   of "bar" at column 19.

   ``diag_mgr`` must be non-NULL.

.. function::  void diagnostic_manager_debug_dump_location (const diagnostic_manager *diag_mgr,\
                                                            const diagnostic_physical_location *loc, \
                                                            FILE *out)

   Write a representation of ``loc`` to ``out``, for debugging.

   Both ``diag_mgr`` and ``out`` must be non-NULL.
   `loc`` may be NULL.

   TODO: example of output

Associating diagnostics with locations
**************************************

A :type:`diagnostic` has an optional primary physical location
and zero or more secondary physical locations.  For example::

   a = (foo && bar)
       ~~~~~^~~~~~~

This diagnostic has a single :type:`diagnostic_physical_location`,
with the caret at the first "&", and the start/finish at the parentheses.

Contrast with::

   a = (foo && bar)
        ~~~ ^~ ~~~

This diagnostic has three locations

* The primary location (at "&&") has its caret and start location at
  the first "&" and end at the second "&.

* The secondary location for "foo" has its start and finish at the "f"
  and "o" of "foo"; the caret is not displayed, but is perhaps at
  the "f" of "foo".

* Similarly, the other secondary location (for "bar") has its start and
  finish at the "b" and "r" of "bar"; the caret is not displayed, but
  is perhaps at the"b" of "bar".

.. function::  void diagnostic_set_location (diagnostic *diag, \
                                             const diagnostic_physical_location * loc)

   Set the primary location of ``diag``.

   ``diag`` must be non-NULL; ``loc`` can be NULL.

.. function:: void diagnostic_set_location_with_label (diagnostic *diag, \
                                                       const diagnostic_physical_location *loc, \
                                                       const char *fmt, ...)

   Set the primary location of ``diag``, with a label.  The label is
   formatted as per the rules FIXME

   ``diag`` and ``fmt`` must be non-NULL; ``loc`` can be NULL.

   See :doc:`message-formatting` for details of how to use ``fmt``.

   TODO: example of use

.. function:: void diagnostic_add_location (diagnostic *diag, \
                                            const diagnostic_physical_location * loc)

   Add a secondary location to ``diag``.

   ``diag`` must be non-NULL; ``loc`` can be NULL.


.. function:: void diagnostic_add_location_with_label (diagnostic *diag, \
                                                       const diagnostic_physical_location *loc, \
                                                       const char *text)

   Add a secondary location to ``diag``, with a label.  The label is
   formatted as per the rules FIXME

   ``diag`` and ``fmt`` must be non-NULL; ``loc`` can be NULL.

   For example,

      .. literalinclude:: ../../../testsuite/libdiagnostics.dg/test-labelled-ranges.c
	 :language: c
	 :start-after: /* begin quoted source */
	 :end-before:  /* end quoted source */

   might give this text output::

      test-labelled-ranges.c:9:6: error: mismatching types: 'int' and 'const char *'
         19 |   42 + "foo"
            |   ~~ ^ ~~~~~
            |   |    |
            |   int  const char *
