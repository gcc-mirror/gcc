..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _gimple-tests:

Support for testing GIMPLE passes
*********************************

As of gcc 7, C functions can be tagged with ``__GIMPLE`` to indicate
that the function body will be GIMPLE, rather than C.  The compiler requires
the option :option:`-fgimple` to enable this functionality.  For example:

.. code-block:: c++

  /* { dg-do compile } */
  /* { dg-options "-O -fgimple" } */

  void __GIMPLE (startwith ("dse2")) foo ()
  {
    int a;

  bb_2:
    if (a > 4)
      goto bb_3;
    else
      goto bb_4;

  bb_3:
    a_2 = 10;
    goto bb_5;

  bb_4:
    a_3 = 20;

  bb_5:
    a_1 = __PHI (bb_3: a_2, bb_4: a_3);
    a_4 = a_1 + 4;

    return;
  }

The ``startwith`` argument indicates at which pass to begin.

Use the dump modifier ``-gimple`` (e.g. :option:`-fdump-tree-all-gimple`)
to make tree dumps more closely follow the format accepted by the GIMPLE
parser.

Example DejaGnu tests of GIMPLE can be seen in the source tree at
:samp:`gcc/testsuite/gcc.dg/gimplefe-*.c`.

The ``__GIMPLE`` parser is integrated with the C tokenizer and
preprocessor, so it should be possible to use macros to build out
test coverage.
