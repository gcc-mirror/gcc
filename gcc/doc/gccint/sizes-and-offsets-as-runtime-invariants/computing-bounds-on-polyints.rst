..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Computing bounds on poly_ints
*****************************

``poly_int`` also provides routines for calculating lower and upper bounds:

:samp:`constant_lower_bound ({a})`
  Assert that :samp:`{a}` is nonnegative and return the smallest value it can have.

:samp:`constant_lower_bound_with_limit ({a}, {b})`
  Return the least value :samp:`{a}` can have, given that the context in
  which :samp:`{a}` appears guarantees that the answer is no less than :samp:`{b}`.
  In other words, the caller is asserting that :samp:`{a}` is greater than or
  equal to :samp:`{b}` even if :samp:`known_ge ({a}, {b})` doesn't hold.

:samp:`constant_upper_bound_with_limit ({a}, {b})`
  Return the greatest value :samp:`{a}` can have, given that the context in
  which :samp:`{a}` appears guarantees that the answer is no greater than :samp:`{b}`.
  In other words, the caller is asserting that :samp:`{a}` is less than or equal
  to :samp:`{b}` even if :samp:`known_le ({a}, {b})` doesn't hold.

:samp:`lower_bound ({a}, {b})`
  Return a value that is always less than or equal to both :samp:`{a}` and :samp:`{b}`.
  It will be the greatest such value for some indeterminate values
  but necessarily for all.

:samp:`upper_bound ({a}, {b})`
  Return a value that is always greater than or equal to both :samp:`{a}` and
  :samp:`{b}`.  It will be the least such value for some indeterminate values
  but necessarily for all.
