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

Tutorial part 5: warnings
=========================

So far we've only emitted errors, but other kinds of diagnostic are possible,
such as warnings.

We can select different kinds of diagnostic via :enum:`diagnostic_level`
when calling :func:`diagnostic_begin`:

.. literalinclude:: ../../../testsuite/libgdiagnostics.dg/test-warning.c
   :language: c
   :start-after: /* begin quoted source */
   :end-before:  /* end quoted source */

On compiling and running the program, we should get output similar to::

   test-warning.c:17:11: warning: this is a warning
   17 | #include <foo.h>
      |           ^~~~~

Various severities are possible, see  :enum:`diagnostic_level` for more
information.

In :doc:`the next section of the tutorial <06-fix-it-hints>` we'll look
at adding fix-it hints to diagnostics.
