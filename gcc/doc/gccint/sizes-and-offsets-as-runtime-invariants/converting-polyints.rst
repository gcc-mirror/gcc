..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Converting poly_ints
********************

A ``poly_int<n, T>`` can be constructed from up to
:samp:`{n}` individual :samp:`{T}` coefficients, with the remaining coefficients
being implicitly zero.  In particular, this means that every
``poly_int<n, T>`` can be constructed from a single
scalar :samp:`{T}`, or something compatible with :samp:`{T}`.

Also, a ``poly_int<n, T>`` can be constructed from
a ``poly_int<n, U>`` if :samp:`{T}` can be constructed
from :samp:`{U}`.

The following functions provide other forms of conversion,
or test whether such a conversion would succeed.

:samp:`{value}.is_constant ()`
  Return true if ``poly_int`` :samp:`{value}` is a compile-time constant.

:samp:`{value}.is_constant (&{c1})`
  Return true if ``poly_int`` :samp:`{value}` is a compile-time constant,
  storing it in :samp:`{c1}` if so.  :samp:`{c1}` must be able to hold all
  constant values of :samp:`{value}` without loss of precision.

:samp:`{value}.to_constant ()`
  Assert that :samp:`{value}` is a compile-time constant and return its value.
  When using this function, please add a comment explaining why the
  condition is known to hold (for example, because an earlier phase
  of analysis rejected non-constants).

:samp:`{value}.to_shwi (&{p2})`
  Return true if :samp:`poly_int<{N}, {T}>` :samp:`{value}` can be
  represented without loss of precision as a
  :samp:`poly_int<{N}, ``HOST_WIDE_INT`` >`, storing it in that
  form in :samp:`{p2}` if so.

:samp:`{value}.to_uhwi (&{p2})`
  Return true if :samp:`poly_int<{N}, {T}>` :samp:`{value}` can be
  represented without loss of precision as a
  :samp:`poly_int<{N}, ``unsigned HOST_WIDE_INT`` >`, storing it in that
  form in :samp:`{p2}` if so.

:samp:`{value}.force_shwi ()`
  Forcibly convert each coefficient of :samp:`poly_int<{N}, {T}>`
  :samp:`{value}` to ``HOST_WIDE_INT``, truncating any that are out of range.
  Return the result as a :samp:`poly_int<{N}, ``HOST_WIDE_INT`` >`.

:samp:`{value}.force_uhwi ()`
  Forcibly convert each coefficient of :samp:`poly_int<{N}, {T}>`
  :samp:`{value}` to ``unsigned HOST_WIDE_INT``, truncating any that are
  out of range.  Return the result as a
  :samp:`poly_int<{N}, ``unsigned HOST_WIDE_INT`` >`.

:samp:`wi::shwi ({value}, {precision})`
  Return a ``poly_int`` with the same value as :samp:`{value}`, but with
  the coefficients converted from ``HOST_WIDE_INT`` to ``wide_int``.
  :samp:`{precision}` specifies the precision of the ``wide_int`` cofficients;
  if this is wider than a ``HOST_WIDE_INT``, the coefficients of
  :samp:`{value}` will be sign-extended to fit.

:samp:`wi::uhwi ({value}, {precision})`
  Like ``wi::shwi``, except that :samp:`{value}` has coefficients of
  type ``unsigned HOST_WIDE_INT``.  If :samp:`{precision}` is wider than
  a ``HOST_WIDE_INT``, the coefficients of :samp:`{value}` will be
  zero-extended to fit.

:samp:`wi::sext ({value}, {precision})`
  Return a ``poly_int`` of the same type as :samp:`{value}`, sign-extending
  every coefficient from the low :samp:`{precision}` bits.  This in effect
  applies ``wi::sext`` to each coefficient individually.

:samp:`wi::zext ({value}, {precision})`
  Like ``wi::sext``, but for zero extension.

:samp:`poly_wide_int::from ({value}, {precision}, {sign})`
  Convert :samp:`{value}` to a ``poly_wide_int`` in which each coefficient
  has :samp:`{precision}` bits.  Extend the coefficients according to
  :samp:`{sign}` if the coefficients have fewer bits.

:samp:`poly_offset_int::from ({value}, {sign})`
  Convert :samp:`{value}` to a ``poly_offset_int``, extending its coefficients
  according to :samp:`{sign}` if they have fewer bits than ``offset_int``.

:samp:`poly_widest_int::from ({value}, {sign})`
  Convert :samp:`{value}` to a ``poly_widest_int``, extending its coefficients
  according to :samp:`{sign}` if they have fewer bits than ``widest_int``.
