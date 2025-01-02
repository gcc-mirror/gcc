.. Copyright (C) 2014-2025 Free Software Foundation, Inc.
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

libgccjit
=========

This document describes `libgccjit <https://gcc.gnu.org/wiki/JIT>`_, an API
for embedding GCC inside programs and libraries.

There are actually two APIs for the library:

* a pure C API: ``libgccjit.h``

* a C++ wrapper API: ``libgccjit++.h``.  This is a collection of "thin"
  wrapper classes around the C API, to save typing.

Contents:

.. toctree::
   :maxdepth: 2

   intro/index.rst
   topics/index.rst
   cp/index.rst
   internals/index.rst


Indices and tables
==================

* :ref:`genindex`
* :ref:`search`
