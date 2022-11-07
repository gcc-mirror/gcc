..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: macro representation (internal)

Internal representation of macros
*********************************

The preprocessor stores macro expansions in tokenized form.  This
saves repeated lexing passes during expansion, at the cost of a small
increase in memory consumption on average.  The tokens are stored
contiguously in memory, so a pointer to the first one and a token
count is all you need to get the replacement list of a macro.

If the macro is a function-like macro the preprocessor also stores its
parameters, in the form of an ordered list of pointers to the hash
table entry of each parameter's identifier.  Further, in the macro's
stored expansion each occurrence of a parameter is replaced with a
special token of type ``CPP_MACRO_ARG``.  Each such token holds the
index of the parameter it represents in the parameter list, which
allows rapid replacement of parameters with their arguments during
expansion.  Despite this optimization it is still necessary to store
the original parameters to the macro, both for dumping with e.g.,
:option:`-dD`, and to warn about non-trivial macro redefinitions when
the parameter names have changed.