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

Tutorial part 6: fix-it hints
=============================

libgdiagnostics supports adding "fix-it hints" to a :type:`diagnostic`:
suggestions for the user on how to edit their code to fix a problem.  These
can be expressed as insertions, replacements, and removals of text.

For example, here we use :func:`diagnostic_add_fix_it_hint_replace` to add
a replacement fix-it hint to a diagnostic:

.. literalinclude:: ../../../testsuite/libgdiagnostics.dg/test-fix-it-hint.c
   :language: c
   :start-after: /* begin quoted source */
   :end-before:  /* end quoted source */

On compiling and running the program, we should get output similar to::

   test-fix-it-hint.c:19:13: error: unknown field 'colour'; did you mean 'color'
      19 |   return p->colour;
         |             ^~~~~~
         |             color

We can also add a call to :func:`diagnostic_manager_write_patch` to the
program cleanup code::

  diagnostic_manager_write_patch (diag_mgr, stderr);

This will write a patch to the stream (here ``stderr``) giving the effect
of all fix-it hints on all diagnostics emitted by the
:type:`diagnostic_manager`, giving something like::

   @@ -16,7 +16,7 @@
    struct rgb
    get_color (struct object *p)
    {
   -  return p->colour;
   +  return p->color;
    }
    

See the :doc:`guide to fix-it hints <../topics/fix-it-hints>`
for more information, or go on to
:doc:`the next section of the tutorial <07-execution-paths>`.
