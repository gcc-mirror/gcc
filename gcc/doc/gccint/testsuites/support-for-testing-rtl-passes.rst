..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _rtl-tests:

Support for testing RTL passes
******************************

As of gcc 7, C functions can be tagged with ``__RTL`` to indicate that the
function body will be RTL, rather than C.  For example:

.. code-block:: c++

  double __RTL (startwith ("ira")) test (struct foo *f, const struct bar *b)
  {
    (function "test"
       [...snip; various directives go in here...]
    ) ;; function "test"
  }

The ``startwith`` argument indicates at which pass to begin.

The parser expects the RTL body to be in the format emitted by this
dumping function:

.. code-block:: c++

  DEBUG_FUNCTION void
  print_rtx_function (FILE *outfile, function *fn, bool compact);

when "compact" is true.  So you can capture RTL in the correct format
from the debugger using:

.. code-block:: c++

  (gdb) print_rtx_function (stderr, cfun, true);

and copy and paste the output into the body of the C function.

Example DejaGnu tests of RTL can be seen in the source tree under
:samp:`gcc/testsuite/gcc.dg/rtl`.

The ``__RTL`` parser is not integrated with the C tokenizer or
preprocessor, and works simply by reading the relevant lines within
the braces.  In particular, the RTL body must be on separate lines from
the enclosing braces, and the preprocessor is not usable within it.
