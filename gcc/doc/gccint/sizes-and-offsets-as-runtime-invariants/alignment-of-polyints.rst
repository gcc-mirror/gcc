..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _alignment-of-poly_ints:

Alignment of poly_ints
**********************

``poly_int`` provides various routines for aligning values and for querying
misalignments.  In each case the alignment must be a power of 2.

:samp:`can_align_p ({value}, {align})`
  Return true if we can align :samp:`{value}` up or down to the nearest multiple
  of :samp:`{align}` at compile time.  The answer is the same for both directions.

:samp:`can_align_down ({value}, {align}, &{aligned})`
  Return true if ``can_align_p`` ; if so, set :samp:`{aligned}` to the greatest
  aligned value that is less than or equal to :samp:`{value}`.

:samp:`can_align_up ({value}, {align}, &{aligned})`
  Return true if ``can_align_p`` ; if so, set :samp:`{aligned}` to the lowest
  aligned value that is greater than or equal to :samp:`{value}`.

:samp:`known_equal_after_align_down ({a}, {b}, {align})`
  Return true if we can align :samp:`{a}` and :samp:`{b}` down to the nearest
  :samp:`{align}` boundary at compile time and if the two results are equal.

:samp:`known_equal_after_align_up ({a}, {b}, {align})`
  Return true if we can align :samp:`{a}` and :samp:`{b}` up to the nearest
  :samp:`{align}` boundary at compile time and if the two results are equal.

:samp:`aligned_lower_bound ({value}, {align})`
  Return a result that is no greater than :samp:`{value}` and that is aligned
  to :samp:`{align}`.  The result will the closest aligned value for some
  indeterminate values but not necessarily for all.

  For example, suppose we are allocating an object of :samp:`{size}` bytes
  in a downward-growing stack whose current limit is given by :samp:`{limit}`.
  If the object requires :samp:`{align}` bytes of alignment, the new stack
  limit is given by:

  .. code-block:: c++

    aligned_lower_bound (limit - size, align)

:samp:`aligned_upper_bound ({value}, {align})`
  Likewise return a result that is no less than :samp:`{value}` and that is
  aligned to :samp:`{align}`.  This is the routine that would be used for
  upward-growing stacks in the scenario just described.

:samp:`known_misalignment ({value}, {align}, &{misalign})`
  Return true if we can calculate the misalignment of :samp:`{value}`
  with respect to :samp:`{align}` at compile time, storing the result in
  :samp:`{misalign}` if so.

:samp:`known_alignment ({value})`
  Return the minimum alignment that :samp:`{value}` is known to have
  (in other words, the largest alignment that can be guaranteed
  whatever the values of the indeterminates turn out to be).
  Return 0 if :samp:`{value}` is known to be 0.

:samp:`force_align_down ({value}, {align})`
  Assert that :samp:`{value}` can be aligned down to :samp:`{align}` at compile
  time and return the result.  When using this routine, please add a
  comment explaining why the assertion is known to hold.

:samp:`force_align_up ({value}, {align})`
  Likewise, but aligning up.

:samp:`force_align_down_and_div ({value}, {align})`
  Divide the result of ``force_align_down`` by :samp:`{align}`.  Again,
  please add a comment explaining why the assertion in ``force_align_down``
  is known to hold.

:samp:`force_align_up_and_div ({value}, {align})`
  Likewise for ``force_align_up``.

:samp:`force_get_misalignment ({value}, {align})`
  Assert that we can calculate the misalignment of :samp:`{value}` with
  respect to :samp:`{align}` at compile time and return the misalignment.
  When using this function, please add a comment explaining why
  the assertion is known to hold.
