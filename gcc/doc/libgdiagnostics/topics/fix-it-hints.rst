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

Fix-it hints
============

Adding fix-it hints to a diagnostic
***********************************

A :type:`diagnostic` can contain "fix-it hints", giving suggestions
for the user on how to edit their code to fix a problem.  These
can be expressed as insertions, replacements, and removals of text.

There is only limited support for newline characters in fix-it hints:
only hints with newlines which insert an entire new line are permitted,
inserting at the start of a line, and finishing with a newline
(with no interior newline characters).  Other attempts to add
fix-it hints containing newline characters will fail.
Similarly, attempts to delete or replace a range *affecting* multiple
lines will fail.

The API handles these failures gracefully, so that diagnostics can attempt
to add fix-it hints without each needing extensive checking.

Fix-it hints are printed to text sinks, and are emitted by SARIF sinks
as ``fix`` objects (see SARIF 2.1.0
`§3.55 fix object <https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141791131>`_).

Fix-it hints within a :type:`diagnostic` are "atomic": if any hints can't
be applied, none of them will be, and no fix-its hints will be displayed
for that diagnostic.  This implies that diagnostic messages need to be worded
in such a way that they make sense whether or not the fix-it hints
are displayed.

All fix-it hints within one :type:`diagnostic` must affect the same
:type:`diagnostic_file`.

.. function:: void diagnostic_add_fix_it_hint_insert_before (diagnostic *diag, \
                                                             const diagnostic_physical_location *loc, \
                                                             const char *addition)

   Add a fix-it hint to ``diag`` suggesting the insertion of the string
   ``addition`` before ``LOC``.

   For example::

     ptr = arr[0];
           ^~~~~~
           &

   This :type:`diagnostic` has a single location covering ``arr[0]``,
   with the caret at the start.  It has a single insertion fix-it hint,
   inserting ``&`` before the start of ``loc``.

.. function:: void diagnostic_add_fix_it_hint_insert_after (diagnostic *diag, \
                                                            const diagnostic_physical_location *loc, \
                                                            const char *addition)

   Add a fix-it hint to ``diag`` suggesting the insertion of the string
   ``addition`` after the end of ``LOC``.

   For example, in::

      #define FN(ARG0, ARG1, ARG2) fn(ARG0, ARG1, ARG2)
                                      ^~~~  ^~~~  ^~~~
                                      (   ) (   ) (   )


   the :type:`diagnostic` has three physical locations, covering ``ARG0``,
   ``ARG1``, and ``ARG2``, and 6 insertion fix-it hints: each arg
   has a pair of insertion fix-it hints, suggesting wrapping
   them with parentheses: one a '(' inserted before,
   the other a ')' inserted after.

.. function:: void diagnostic_add_fix_it_hint_replace (diagnostic *diag, \
                                                       const diagnostic_physical_location *loc, \
                                                       const char *replacement)

   Add a fix-it hint to ``diag`` suggesting the replacement of the text
   at ``LOC`` with the string ``replacement``.

   For example, in::

      c = s.colour;
	    ^~~~~~
	    color

   This :type:`diagnostic` has a single physical location covering ``colour``,
   and a single "replace" fix-it hint, covering the same range, suggesting
   replacing it with ``color``.

.. function:: void diagnostic_add_fix_it_hint_delete (diagnostic *diag, \
                                                      const diagnostic_physical_location *loc)

   Add a fix-it hint to ``diag`` suggesting the deletion of the text
   at ``LOC``.


   For example, in::

     struct s {int i};;
		      ^
		      -

   This :type:`diagnostic` has a single physical location at the stray
   trailing semicolon, along with a single removal fix-it hint, covering
   the same location.


Generating patches
******************

.. function:: void diagnostic_manager_write_patch (diagnostic_manager *diag_mgr, \
                                                   FILE *dst_stream)

   Write a patch to ``dst_stream`` consisting of the effect of all fix-it hints
   on all diagnostics that have been finished on ``diag_mgr``.

   Both parameters must be non-NULL.
