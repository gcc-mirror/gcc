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

libdiagnostics
==============

This document describes `libdiagnostics <https://gcc.gnu.org/wiki/libdiagnostics>`_,
an API for programs to use to emit diagnostics (such as for "lint"-style checker
tools), supporting:

* text output similar to GCC's errors and warnings::

    test-typo.c:19:13: error: unknown field 'colour'
       19 |   return p->colour;
          |             ^~~~~~

  quoting pertinent source code (with a cache), and underlining
  :doc:`points and ranges in the files being tested <tutorial/02-physical-locations>`,
  possibly with labels::

   test-labelled-ranges.c:9:6: error: mismatching types: 'int' and 'const char *'
      19 |   42 + "foo"
         |   ~~ ^ ~~~~~
         |   |    |
         |   int  const char *

* emitting :doc:`fix-it hints <tutorial/06-fix-it-hints>`::

   test-fix-it-hint.c:19:13: error: unknown field 'colour'; did you mean 'color'
      19 |   return p->colour;
         |             ^~~~~~
         |             color

  and generating patches from them::

   @@ -16,7 +16,7 @@
    struct rgb
    get_color (struct object *p)
    {
   -  return p->colour;
   +  return p->color;
    }

* capturing :doc:`execution paths<tutorial/07-execution-paths>` through code::

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

* support for emitting machine-readable representations of the above
  using the :doc:`SARIF file format <topics/sarif>`

There are actually two APIs for the library:

* a pure C API: ``libdiagnostics.h``

* a C++ wrapper API: ``libdiagnostics+.h``.  This is a header-only
  collection of wrapper classes around the C API to give a less
  verbose API.

This documentation covers the C API.

Contents
********

.. toctree::
   :maxdepth: 2

   tutorial/index.rst
   topics/index.rst

libdiagnostics is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.


Indices and tables
******************

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
