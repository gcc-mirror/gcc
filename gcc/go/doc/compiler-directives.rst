..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _compiler-directives:

Compiler Directives
-------------------

The Go compiler supports a few compiler directives.  A compiler
directive uses a ``//`` comment at the start of a line.  There must
be no space between the ``//`` and the name of the directive.

:samp:`//line {file}:{line}`
  The ``//line`` directive specifies that the source line that
  follows should be recorded as having come from the given file path and
  line number.  Successive lines are recorded using increasing line
  numbers, until the next directive.  This directive typically appears
  in machine-generated code, so that compilers and debuggers will show
  lines in the original input to the generator.

:samp:`//extern {extern_name}`
  The ``extern`` directive sets the externally visible name of the
  next function declaration.  See :ref:`function-names`.

:samp:`//go:compile {go_name}{extern_name}`
  The ``go:compile`` directives sets the externally visible name of a
  function definition or declaration.  See :ref:`function-names`.

``//go:noescape``
  The ``//go:noescape`` directive specifies that the next declaration
  in the file, which must be a func without a body (meaning that it has
  an implementation not written in Go) does not allow any of the
  pointers passed as arguments to escape into the heap or into the
  values returned from the function. This information can be used during
  the compiler's escape analysis of Go code calling the function.

``//go:nosplit``
  The ``//go:nosplit`` directive specifies that the next function
  declared in the file must not include a stack overflow check. This is
  most commonly used by low-level runtime sources invoked at times when
  it is unsafe for the calling goroutine to be preempted.

``//go:noinline``
  The ``//go:noinline`` directive specifies that the next function
  defined in the file may not be inlined.
