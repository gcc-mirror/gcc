..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: commenting out code

.. _deleted-code:

Deleted Code
************

If you replace or delete a part of the program but want to keep the old
code around for future reference, you often cannot simply comment it
out.  Block comments do not nest, so the first comment inside the old
code will end the commenting-out.  The probable result is a flood of
syntax errors.

One way to avoid this problem is to use an always-false conditional
instead.  For instance, put ``#if 0`` before the deleted code and
``#endif`` after it.  This works even if the code being turned
off contains conditionals, but they must be entire conditionals
(balanced :samp:`#if` and :samp:`#endif`).

Some people use ``#ifdef notdef`` instead.  This is risky, because
``notdef`` might be accidentally defined as a macro, and then the
conditional would succeed.  ``#if 0`` can be counted on to fail.

Do not use ``#if 0`` for comments which are not C code.  Use a real
comment, instead.  The interior of ``#if 0`` must consist of complete
tokens; in particular, single-quote characters must balance.  Comments
often contain unbalanced single-quote characters (known in English as
apostrophes).  These confuse ``#if 0``.  They don't confuse
:samp:`/*`.
