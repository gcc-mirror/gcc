..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: IV analysis on RTL

.. _loop-iv:

IV analysis on RTL
******************

The induction variable on RTL is simple and only allows analysis of
affine induction variables, and only in one loop at once.  The interface
is declared in :samp:`cfgloop.h`.  Before analyzing induction variables
in a loop L, ``iv_analysis_loop_init`` function must be called on L.
After the analysis (possibly calling ``iv_analysis_loop_init`` for
several loops) is finished, ``iv_analysis_done`` should be called.
The following functions can be used to access the results of the
analysis:

* ``iv_analyze`` : Analyzes a single register used in the given
  insn.  If no use of the register in this insn is found, the following
  insns are scanned, so that this function can be called on the insn
  returned by get_condition.

* ``iv_analyze_result`` : Analyzes result of the assignment in the
  given insn.

* ``iv_analyze_expr`` : Analyzes a more complicated expression.
  All its operands are analyzed by ``iv_analyze``, and hence they must
  be used in the specified insn or one of the following insns.

The description of the induction variable is provided in ``struct
rtx_iv``.  In order to handle subregs, the representation is a bit
complicated; if the value of the ``extend`` field is not
``UNKNOWN``, the value of the induction variable in the i-th
iteration is

.. code-block:: c++

  delta + mult * extend_{extend_mode} (subreg_{mode} (base + i * step)),

with the following exception:  if ``first_special`` is true, then the
value in the first iteration (when ``i`` is zero) is ``delta +
mult * base``.  However, if ``extend`` is equal to ``UNKNOWN``,
then ``first_special`` must be false, ``delta`` 0, ``mult`` 1
and the value in the i-th iteration is

.. code-block:: c++

  subreg_{mode} (base + i * step)

The function ``get_iv_value`` can be used to perform these
calculations.
