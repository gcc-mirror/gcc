..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _include-operation:

Include Operation
*****************

The :samp:`#include` directive works by directing the C preprocessor to
scan the specified file as input before continuing with the rest of the
current file.  The output from the preprocessor contains the output
already generated, followed by the output resulting from the included
file, followed by the output that comes from the text after the
:samp:`#include` directive.  For example, if you have a header file
:samp:`header.h` as follows,

.. code-block:: c++

  char *test (void);

and a main program called :samp:`program.c` that uses the header file,
like this,

.. code-block:: c++

  int x;
  #include "header.h"

  int
  main (void)
  {
    puts (test ());
  }

the compiler will see the same token stream as it would if
:samp:`program.c` read

.. code-block:: c++

  int x;
  char *test (void);

  int
  main (void)
  {
    puts (test ());
  }

Included files are not limited to declarations and macro definitions;
those are merely the typical uses.  Any fragment of a C program can be
included from another file.  The include file could even contain the
beginning of a statement that is concluded in the containing file, or
the end of a statement that was started in the including file.  However,
an included file must consist of complete tokens.  Comments and string
literals which have not been closed by the end of an included file are
invalid.  For error recovery, they are considered to end at the end of
the file.

To avoid confusion, it is best if header files contain only complete
syntactic units---function declarations or definitions, type
declarations, etc.

The line following the :samp:`#include` directive is always treated as a
separate line by the C preprocessor, even if the included file lacks a
final newline.
