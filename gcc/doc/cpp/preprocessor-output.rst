..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _preprocessor-output:

Preprocessor Output
-------------------

When the C preprocessor is used with the C, C++, or Objective-C
compilers, it is integrated into the compiler and communicates a stream
of binary tokens directly to the compiler's parser.  However, it can
also be used in the more conventional standalone mode, where it produces
textual output.

.. todo:: Document the library interface.

.. index:: output format

The output from the C preprocessor looks much like the input, except
that all preprocessing directive lines have been replaced with blank
lines and all comments with spaces.  Long runs of blank lines are
discarded.

The ISO standard specifies that it is implementation defined whether a
preprocessor preserves whitespace between tokens, or replaces it with
e.g. a single space.  In GNU CPP, whitespace between tokens is collapsed
to become a single space, with the exception that the first token on a
non-directive line is preceded with sufficient spaces that it appears in
the same column in the preprocessed output that it appeared in the
original source file.  This is so the output is easy to read.
CPP does not insert any
whitespace where there was none in the original source, except where
necessary to prevent an accidental token paste.

.. index:: linemarkers

Source file name and line number information is conveyed by lines
of the form

.. code-block:: c++

  # linenum filename flags

These are called :dfn:`linemarkers`.  They are inserted as needed into
the output (but never within a string or character constant).  They mean
that the following line originated in file :samp:`{filename}` at line
:samp:`{linenum}`.  :samp:`{filename}` will never contain any non-printing
characters; they are replaced with octal escape sequences.

After the file name comes zero or more flags, which are :samp:`1`,
:samp:`2`, :samp:`3`, or :samp:`4`.  If there are multiple flags, spaces
separate them.  Here is what the flags mean:

:samp:`1`
  This indicates the start of a new file.

:samp:`2`
  This indicates returning to a file (after having included another file).

:samp:`3`
  This indicates that the following text comes from a system header file,
  so certain warnings should be suppressed.

:samp:`4`
  This indicates that the following text should be treated as being
  wrapped in an implicit ``extern "C"`` block.

  .. maybe cross reference SYSTEM_IMPLICIT_EXTERN_C

As an extension, the preprocessor accepts linemarkers in non-assembler
input files.  They are treated like the corresponding :samp:`#line`
directive, (see :ref:`line-control`), except that trailing flags are
permitted, and are interpreted with the meanings described above.  If
multiple flags are given, they must be in ascending order.

Some directives may be duplicated in the output of the preprocessor.
These are :samp:`#ident` (always), :samp:`#pragma` (only if the
preprocessor does not handle the pragma itself), and :samp:`#define` and
:samp:`#undef` (with certain debugging options).  If this happens, the
:samp:`#` of the directive will always be in the first column, and there
will be no space between the :samp:`#` and the directive name.  If macro
expansion happens to generate tokens which might be mistaken for a
duplicated directive, a space will be inserted between the :samp:`#` and
the directive name.