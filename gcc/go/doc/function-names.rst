..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: extern, external names

.. _function-names:

Function Names
**************

Go code can call C functions directly using the ``//extern`` or
``//go:linkname`` compiler directives.  An ``//extern``
directive must be at the beginning of the line and must start with
``//extern``.  This must be followed by a space and then the
external name of the function.  The function declaration must be on
the line immediately after the comment.  For example, here is how the
C function ``open`` can be declared in Go:

.. code-block:: c++

  //extern open
  func c_open(name *byte, mode int, perm int) int

You can do the same thing using the ``//go:linkname`` compiler
directive.  The ``//go:linkname`` directive must be at the start of
the line.  It is followed by whitespace, the name of the Go function,
more whitespace, and the external name of the function.  Unlike
``//extern``, ``//go:linkname`` does not need to appear
immediately adjacent to the function definition or declaration.

.. code-block:: c++

  //go:linkname c_open open
  func c_open(name *byte, mode int, perm int) int

The C function naturally expects a nul terminated string, which in Go
is equivalent to a pointer to an array (not a slice!) of ``byte``
with a terminating zero byte.  So a sample call from Go would look
like (after importing the ``os`` package):

.. code-block:: c++

  var name = [4]byte{'f', 'o', 'o', 0};
  i := c_open(&name[0], os.O_RDONLY, 0);

Note that this serves as an example only.  To open a file in Go please
use Go's ``os.Open`` function instead.

The name of Go functions accessed from C is subject to change.  At
present the name of a Go function that does not have a receiver is
``pkgpath.Functionname``.  The :samp:`{pkgpath}` is set by the
:option:`-fgo-pkgpath` option used when the package is compiled; if the
option is not used, the default is ``go.packagename``.  To
call the function from C you must set the name using the :command:`gcc`
``__asm__`` extension.

.. code-block:: c++

  extern int go_function(int) __asm__ ("mypkgpath.Function");
