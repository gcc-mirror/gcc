..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Comparisons involving poly_int
******************************

In general we need to compare sizes and offsets in two situations:
those in which the values need to be ordered, and those in which
the values can be unordered.  More loosely, the distinction is often
between values that have a definite link (usually because they refer to the
same underlying register or memory location) and values that have
no definite link.  An example of the former is the relationship between
the inner and outer sizes of a subreg, where we must know at compile time
whether the subreg is paradoxical, partial, or complete.  An example of
the latter is alias analysis: we might want to check whether two
arbitrary memory references overlap.

Referring back to the examples in the previous section, it makes sense
to ask whether a memory reference of size :samp:`3 + 4{x}` overlaps
one of size :samp:`1 + 5{x}`, but it does not make sense to have a
subreg in which the outer mode has :samp:`3 + 4{x}` bytes and the
inner mode has :samp:`1 + 5{x}` bytes (or vice versa).  Such subregs
are always invalid and should trigger an internal compiler error
if formed.

The underlying operators are the same in both cases, but the distinction
affects how they are used.

.. toctree::
  :maxdepth: 2

.. _comparison-functions-for-poly_int:

Comparison functions for poly_int
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``poly_int`` provides the following routines for checking whether
a particular condition 'may be' (might be) true:

.. code-block:: c++

  maybe_lt maybe_le maybe_eq maybe_ge maybe_gt
                    maybe_ne

The functions have their natural meaning:

:samp:`maybe_lt({a}, {b})`
  Return true if :samp:`{a}` might be less than :samp:`{b}`.

:samp:`maybe_le({a}, {b})`
  Return true if :samp:`{a}` might be less than or equal to :samp:`{b}`.

:samp:`maybe_eq({a}, {b})`
  Return true if :samp:`{a}` might be equal to :samp:`{b}`.

:samp:`maybe_ne({a}, {b})`
  Return true if :samp:`{a}` might not be equal to :samp:`{b}`.

:samp:`maybe_ge({a}, {b})`
  Return true if :samp:`{a}` might be greater than or equal to :samp:`{b}`.

:samp:`maybe_gt({a}, {b})`
  Return true if :samp:`{a}` might be greater than :samp:`{b}`.

  For readability, ``poly_int`` also provides 'known' inverses of these functions:

.. code-block:: c++

  known_lt (a, b) == !maybe_ge (a, b)
  known_le (a, b) == !maybe_gt (a, b)
  known_eq (a, b) == !maybe_ne (a, b)
  known_ge (a, b) == !maybe_lt (a, b)
  known_gt (a, b) == !maybe_le (a, b)
  known_ne (a, b) == !maybe_eq (a, b)

Properties of the poly_int comparisons
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

All 'maybe' relations except ``maybe_ne`` are transitive, so for example:

.. code-block:: c++

  maybe_lt (a, b) && maybe_lt (b, c) implies maybe_lt (a, c)

for all :samp:`{a}`, :samp:`{b}` and :samp:`{c}`.  ``maybe_lt``, ``maybe_gt``
and ``maybe_ne`` are irreflexive, so for example:

.. code-block:: c++

  !maybe_lt (a, a)

is true for all :samp:`{a}`.  ``maybe_le``, ``maybe_eq`` and ``maybe_ge``
are reflexive, so for example:

.. code-block:: c++

  maybe_le (a, a)

is true for all :samp:`{a}`.  ``maybe_eq`` and ``maybe_ne`` are symmetric, so:

.. code-block:: c++

  maybe_eq (a, b) == maybe_eq (b, a)
  maybe_ne (a, b) == maybe_ne (b, a)

for all :samp:`{a}` and :samp:`{b}`.  In addition:

.. code-block:: c++

  maybe_le (a, b) == maybe_lt (a, b) || maybe_eq (a, b)
  maybe_ge (a, b) == maybe_gt (a, b) || maybe_eq (a, b)
  maybe_lt (a, b) == maybe_gt (b, a)
  maybe_le (a, b) == maybe_ge (b, a)

However:

.. code-block:: c++

  maybe_le (a, b) && maybe_le (b, a) does not imply !maybe_ne (a, b) [== known_eq (a, b)]
  maybe_ge (a, b) && maybe_ge (b, a) does not imply !maybe_ne (a, b) [== known_eq (a, b)]

One example is again :samp:`{a} == 3 + 4{x}`
and :samp:`{b} == 1 + 5{x}`, where :samp:`maybe_le ({a}, {b})`,
:samp:`maybe_ge ({a}, {b})` and :samp:`maybe_ne ({a}, {b})`
all hold.  ``maybe_le`` and ``maybe_ge`` are therefore not antisymetric
and do not form a partial order.

From the above, it follows that:

* All 'known' relations except ``known_ne`` are transitive.

* ``known_lt``, ``known_ne`` and ``known_gt`` are irreflexive.

* ``known_le``, ``known_eq`` and ``known_ge`` are reflexive.

Also:

.. code-block:: c++

  known_lt (a, b) == known_gt (b, a)
  known_le (a, b) == known_ge (b, a)
  known_lt (a, b) implies !known_lt (b, a)  [asymmetry]
  known_gt (a, b) implies !known_gt (b, a)
  known_le (a, b) && known_le (b, a) == known_eq (a, b) [== !maybe_ne (a, b)]
  known_ge (a, b) && known_ge (b, a) == known_eq (a, b) [== !maybe_ne (a, b)]

``known_le`` and ``known_ge`` are therefore antisymmetric and are
partial orders.  However:

.. code-block:: c++

  known_le (a, b) does not imply known_lt (a, b) || known_eq (a, b)
  known_ge (a, b) does not imply known_gt (a, b) || known_eq (a, b)

For example, :samp:`known_le (4, 4 + 4{x})` holds because the runtime
indeterminate :samp:`{x}` is a nonnegative integer, but neither
``known_lt (4, 4 + 4x)`` nor ``known_eq (4, 4 + 4x)`` hold.

Comparing potentially-unordered poly_ints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In cases where there is no definite link between two ``poly_int`` s,
we can usually make a conservatively-correct assumption.  For example,
the conservative assumption for alias analysis is that two references
*might* alias.

One way of checking whether [ :samp:`{begin1}`, :samp:`{end1}`) might overlap
[ :samp:`{begin2}`, :samp:`{end2}`) using the ``poly_int`` comparisons is:

.. code-block:: c++

  maybe_gt (end1, begin2) && maybe_gt (end2, begin1)

and another (equivalent) way is:

.. code-block:: c++

  !(known_le (end1, begin2) || known_le (end2, begin1))

However, in this particular example, it is better to use the range helper
functions instead.  See :ref:`range-checks-on-poly_ints`.

Comparing ordered poly_ints
^^^^^^^^^^^^^^^^^^^^^^^^^^^

In cases where there is a definite link between two ``poly_int`` s,
such as the outer and inner sizes of subregs, we usually require the sizes
to be ordered by the ``known_le`` partial order.  ``poly_int`` provides
the following utility functions for ordered values:

:samp:`ordered_p ({a}, {b})`
  Return true if :samp:`{a}` and :samp:`{b}` are ordered by the ``known_le``
  partial order.

:samp:`ordered_min ({a}, {b})`
  Assert that :samp:`{a}` and :samp:`{b}` are ordered by ``known_le`` and return the
  minimum of the two.  When using this function, please add a comment explaining
  why the values are known to be ordered.

:samp:`ordered_max ({a}, {b})`
  Assert that :samp:`{a}` and :samp:`{b}` are ordered by ``known_le`` and return the
  maximum of the two.  When using this function, please add a comment explaining
  why the values are known to be ordered.

  For example, if a subreg has an outer mode of size :samp:`{outer}` and an
  inner mode of size :samp:`{inner}` :

* the subreg is complete if known_eq (:samp:`{inner}`, :samp:`{outer}`)

* otherwise, the subreg is paradoxical if known_le (:samp:`{inner}`, :samp:`{outer}`)

* otherwise, the subreg is partial if known_le (:samp:`{outer}`, :samp:`{inner}`)

* otherwise, the subreg is ill-formed

Thus the subreg is only valid if
:samp:`ordered_p ({outer}, {inner})` is true.  If this condition
is already known to be true then:

* the subreg is complete if known_eq (:samp:`{inner}`, :samp:`{outer}`)

* the subreg is paradoxical if maybe_lt (:samp:`{inner}`, :samp:`{outer}`)

* the subreg is partial if maybe_lt (:samp:`{outer}`, :samp:`{inner}`)

with the three conditions being mutually exclusive.

Code that checks whether a subreg is valid would therefore generally
check whether ``ordered_p`` holds (in addition to whatever other
checks are required for subreg validity).  Code that is dealing
with existing subregs can assert that ``ordered_p`` holds
and use either of the classifications above.

Checking for a poly_int marker value
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is sometimes useful to have a special 'marker value' that is not
meant to be taken literally.  For example, some code uses a size
of -1 to represent an unknown size, rather than having to carry around
a separate boolean to say whether the size is known.

The best way of checking whether something is a marker value is
``known_eq``.  Conversely the best way of checking whether something
is *not* a marker value is ``maybe_ne``.

Thus in the size example just mentioned, :samp:`known_eq (size, -1)` would
check for an unknown size and :samp:`maybe_ne (size, -1)` would check for a
known size.

.. _range-checks-on-poly_ints:

Range checks on poly_ints
^^^^^^^^^^^^^^^^^^^^^^^^^

As well as the core comparisons
(see :ref:`comparison-functions-for-poly_int`), ``poly_int`` provides
utilities for various kinds of range check.  In each case the range
is represented by a start position and a size rather than a start
position and an end position; this is because the former is used
much more often than the latter in GCC.  Also, the sizes can be
-1 (or all ones for unsigned sizes) to indicate a range with a known
start position but an unknown size.  All other sizes must be nonnegative.
A range of size 0 does not contain anything or overlap anything.

:samp:`known_size_p ({size})`
  Return true if :samp:`{size}` represents a known range size, false if it
  is -1 or all ones (for signed and unsigned types respectively).

:samp:`ranges_maybe_overlap_p ({pos1}, {size1}, {pos2}, {size2})`
  Return true if the range described by :samp:`{pos1}` and :samp:`{size1}` *might*
  overlap the range described by :samp:`{pos2}` and :samp:`{size2}` (in other words,
  return true if we cannot prove that the ranges are disjoint).

:samp:`ranges_known_overlap_p ({pos1}, {size1}, {pos2}, {size2})`
  Return true if the range described by :samp:`{pos1}` and :samp:`{size1}` is known to
  overlap the range described by :samp:`{pos2}` and :samp:`{size2}`.

:samp:`known_subrange_p ({pos1}, {size1}, {pos2}, {size2})`
  Return true if the range described by :samp:`{pos1}` and :samp:`{size1}` is known to
  be contained in the range described by :samp:`{pos2}` and :samp:`{size2}`.

:samp:`maybe_in_range_p ({value}, {pos}, {size})`
  Return true if :samp:`{value}` *might* be in the range described by
  :samp:`{pos}` and :samp:`{size}` (in other words, return true if we cannot
  prove that :samp:`{value}` is outside that range).

:samp:`known_in_range_p ({value}, {pos}, {size})`
  Return true if :samp:`{value}` is known to be in the range described
  by :samp:`{pos}` and :samp:`{size}`.

:samp:`endpoint_representable_p ({pos}, {size})`
  Return true if the range described by :samp:`{pos}` and :samp:`{size}` is
  open-ended or if the endpoint (:samp:`{pos}` + :samp:`{size}`) is representable
  in the same type as :samp:`{pos}` and :samp:`{size}`.  The function returns false
  if adding :samp:`{size}` to :samp:`{pos}` makes conceptual sense but could overflow.

  There is also a ``poly_int`` version of the ``IN_RANGE_P`` macro:

:samp:`coeffs_in_range_p ({x}, {lower}, {upper})`
  Return true if every coefficient of :samp:`{x}` is in the inclusive range
  [ :samp:`{lower}`, :samp:`{upper}` ].  This function can be useful when testing
  whether an operation would cause the values of coefficients to
  overflow.

  Note that the function does not indicate whether :samp:`{x}` itself is in the
  given range.  :samp:`{x}` can be either a constant or a ``poly_int``.

Sorting poly_ints
^^^^^^^^^^^^^^^^^

``poly_int`` provides the following routine for sorting:

:samp:`compare_sizes_for_sort ({a}, {b})`
  Compare :samp:`{a}` and :samp:`{b}` in reverse lexicographical order (that is,
  compare the highest-indexed coefficients first).  This can be useful when
  sorting data structures, since it has the effect of separating constant
  and non-constant values.  If all values are nonnegative, the constant
  values come first.

  Note that the values do not necessarily end up in numerical order.
  For example, :samp:`1 + 1{x}` would come after :samp:`100` in the sort order,
  but may well be less than :samp:`100` at run time.
