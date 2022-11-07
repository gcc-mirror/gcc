..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Number of iterations analysis

.. _number-of-iterations:

Number of iterations analysis
*****************************

Both on GIMPLE and on RTL, there are functions available to determine
the number of iterations of a loop, with a similar interface.  The
number of iterations of a loop in GCC is defined as the number of
executions of the loop latch.  In many cases, it is not possible to
determine the number of iterations unconditionally -- the determined
number is correct only if some assumptions are satisfied.  The analysis
tries to verify these conditions using the information contained in the
program; if it fails, the conditions are returned together with the
result.  The following information and conditions are provided by the
analysis:

* ``assumptions`` : If this condition is false, the rest of
  the information is invalid.

* ``noloop_assumptions`` on RTL, ``may_be_zero`` on GIMPLE: If
  this condition is true, the loop exits in the first iteration.

* ``infinite`` : If this condition is true, the loop is infinite.
  This condition is only available on RTL.  On GIMPLE, conditions for
  finiteness of the loop are included in ``assumptions``.

* ``niter_expr`` on RTL, ``niter`` on GIMPLE: The expression
  that gives number of iterations.  The number of iterations is defined as
  the number of executions of the loop latch.

Both on GIMPLE and on RTL, it necessary for the induction variable
analysis framework to be initialized (SCEV on GIMPLE, loop-iv on RTL).
On GIMPLE, the results are stored to ``struct tree_niter_desc``
structure.  Number of iterations before the loop is exited through a
given exit can be determined using ``number_of_iterations_exit``
function.  On RTL, the results are returned in ``struct niter_desc``
structure.  The corresponding function is named
``check_simple_exit``.  There are also functions that pass through
all the exits of a loop and try to find one with easy to determine
number of iterations -- ``find_loop_niter`` on GIMPLE and
``find_simple_exit`` on RTL.  Finally, there are functions that
provide the same information, but additionally cache it, so that
repeated calls to number of iterations are not so costly --
``number_of_latch_executions`` on GIMPLE and ``get_simple_loop_desc``
on RTL.

Note that some of these functions may behave slightly differently than
others -- some of them return only the expression for the number of
iterations, and fail if there are some assumptions.  The function
``number_of_latch_executions`` works only for single-exit loops.
The function ``number_of_cond_exit_executions`` can be used to
determine number of executions of the exit condition of a single-exit
loop (i.e., the ``number_of_latch_executions`` increased by one).

On GIMPLE, below constraint flags affect semantics of some APIs of number
of iterations analyzer:

* ``LOOP_C_INFINITE`` : If this constraint flag is set, the loop
  is known to be infinite.  APIs like ``number_of_iterations_exit`` can
  return false directly without doing any analysis.

* ``LOOP_C_FINITE`` : If this constraint flag is set, the loop is
  known to be finite, in other words, loop's number of iterations can be
  computed with ``assumptions`` be true.

Generally, the constraint flags are set/cleared by consumers which are
loop optimizers.  It's also the consumers' responsibility to set/clear
constraints correctly.  Failing to do that might result in hard to track
down bugs in scev/niter consumers.  One typical use case is vectorizer:
it drives number of iterations analyzer by setting ``LOOP_C_FINITE``
and vectorizes possibly infinite loop by versioning loop with analysis
result.  In return, constraints set by consumers can also help number of
iterations analyzer in following optimizers.  For example, ``niter``
of a loop versioned under ``assumptions`` is valid unconditionally.

Other constraints may be added in the future, for example, a constraint
indicating that loops' latch must roll thus ``may_be_zero`` would be
false unconditionally.