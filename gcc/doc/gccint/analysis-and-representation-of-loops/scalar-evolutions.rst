..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Scalar evolutions, IV analysis on GIMPLE

.. _scalar-evolutions:

Scalar evolutions
*****************

Scalar evolutions (SCEV) are used to represent results of induction
variable analysis on GIMPLE.  They enable us to represent variables with
complicated behavior in a simple and consistent way (we only use it to
express values of polynomial induction variables, but it is possible to
extend it).  The interfaces to SCEV analysis are declared in
:samp:`tree-scalar-evolution.h`.  To use scalar evolutions analysis,
``scev_initialize`` must be used.  To stop using SCEV,
``scev_finalize`` should be used.  SCEV analysis caches results in
order to save time and memory.  This cache however is made invalid by
most of the loop transformations, including removal of code.  If such a
transformation is performed, ``scev_reset`` must be called to clean
the caches.

Given an SSA name, its behavior in loops can be analyzed using the
``analyze_scalar_evolution`` function.  The returned SCEV however
does not have to be fully analyzed and it may contain references to
other SSA names defined in the loop.  To resolve these (potentially
recursive) references, ``instantiate_parameters`` or
``resolve_mixers`` functions must be used.
``instantiate_parameters`` is useful when you use the results of SCEV
only for some analysis, and when you work with whole nest of loops at
once.  It will try replacing all SSA names by their SCEV in all loops,
including the super-loops of the current loop, thus providing a complete
information about the behavior of the variable in the loop nest.
``resolve_mixers`` is useful if you work with only one loop at a
time, and if you possibly need to create code based on the value of the
induction variable.  It will only resolve the SSA names defined in the
current loop, leaving the SSA names defined outside unchanged, even if
their evolution in the outer loops is known.

The SCEV is a normal tree expression, except for the fact that it may
contain several special tree nodes.  One of them is
``SCEV_NOT_KNOWN``, used for SSA names whose value cannot be
expressed.  The other one is ``POLYNOMIAL_CHREC``.  Polynomial chrec
has three arguments -- base, step and loop (both base and step may
contain further polynomial chrecs).  Type of the expression and of base
and step must be the same.  A variable has evolution
``POLYNOMIAL_CHREC(base, step, loop)`` if it is (in the specified
loop) equivalent to ``x_1`` in the following example

.. code-block:: c++

  while (...)
    {
      x_1 = phi (base, x_2);
      x_2 = x_1 + step;
    }

Note that this includes the language restrictions on the operations.
For example, if we compile C code and ``x`` has signed type, then the
overflow in addition would cause undefined behavior, and we may assume
that this does not happen.  Hence, the value with this SCEV cannot
overflow (which restricts the number of iterations of such a loop).

In many cases, one wants to restrict the attention just to affine
induction variables.  In this case, the extra expressive power of SCEV
is not useful, and may complicate the optimizations.  In this case,
``simple_iv`` function may be used to analyze a value -- the result
is a loop-invariant base and step.
