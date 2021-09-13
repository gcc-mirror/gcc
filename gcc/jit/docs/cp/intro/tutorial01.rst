.. Copyright (C) 2014-2021 Free Software Foundation, Inc.
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

.. default-domain:: cpp

Tutorial part 1: "Hello world"
==============================

Before we look at the details of the API, let's look at building and
running programs that use the library.

Here's a toy "hello world" program that uses the library's C++ API to
synthesize a call to `printf` and uses it to write a message to stdout.

Don't worry about the content of the program for now; we'll cover
the details in later parts of this tutorial.

   .. literalinclude:: ../../examples/tut01-hello-world.cc
    :language: c++

Copy the above to `tut01-hello-world.cc`.

Assuming you have the jit library installed, build the test program
using:

.. code-block:: console

  $ gcc \
      tut01-hello-world.cc \
      -o tut01-hello-world \
      -lgccjit

You should then be able to run the built program:

.. code-block:: console

  $ ./tut01-hello-world
  hello world
