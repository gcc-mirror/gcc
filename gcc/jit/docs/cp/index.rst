.. Copyright (C) 2014-2020 Free Software Foundation, Inc.
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
   <http://www.gnu.org/licenses/>.

C++ bindings for libgccjit
==========================

This document describes the C++ bindings to
`libgccjit <http://gcc.gnu.org/wiki/JIT>`_, an API for embedding GCC
inside programs and libraries.

The C++ bindings consist of a single header file ``libgccjit++.h``.

This is a collection of "thin" wrapper classes around the C API.
Everything is an inline function, implemented in terms of the C API,
so there is nothing extra to link against.

Note that libgccjit is currently of "Alpha" quality;
the APIs are not yet set in stone, and they shouldn't be used in
production yet.

Contents:

.. toctree::
   :maxdepth: 2

   intro/index.rst
   topics/index.rst
