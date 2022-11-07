..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Guidelines for using poly_int
*****************************

One of the main design goals of ``poly_int`` was to make it easy
to write target-independent code that handles variable-sized registers
even when the current target has fixed-sized registers.  There are two
aspects to this:

* The set of ``poly_int`` operations should be complete enough that
  the question in most cases becomes 'Can we do this operation on these
  particular ``poly_int`` values?  If not, bail out' rather than
  'Are these ``poly_int`` values constant?  If so, do the operation,
  otherwise bail out'.

* If target-independent code compiles and runs correctly on a target
  with one value of ``NUM_POLY_INT_COEFFS``, and if the code does not
  use asserting functions like ``to_constant``, it is reasonable to
  assume that the code also works on targets with other values of
  ``NUM_POLY_INT_COEFFS``.  There is no need to check this during
  everyday development.

So the general principle is: if target-independent code is dealing
with a ``poly_int`` value, it is better to operate on it as a
``poly_int`` if at all possible, choosing conservatively-correct
behavior if a particular operation fails.  For example, the following
code handles an index ``pos`` into a sequence of vectors that each
have ``nunits`` elements:

.. code-block:: c++

  /* Calculate which vector contains the result, and which lane of
     that vector we need.  */
  if (!can_div_trunc_p (pos, nunits, &vec_entry, &vec_index))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "Cannot determine which vector holds the"
                         " final result.\n");
      return false;
    }

However, there are some contexts in which operating on a
``poly_int`` is not possible or does not make sense.  One example
is when handling static initializers, since no current target supports
the concept of a variable-length static initializer.  In these
situations, a reasonable fallback is:

.. code-block:: c++

  if (poly_value.is_constant (&const_value))
    {
      ...
      /* Operate on const_value.  */
      ...
    }
  else
    {
      ...
      /* Conservatively correct fallback.  */
      ...
    }

``poly_int`` also provides some asserting functions like
``to_constant``.  Please only use these functions if there is a
good theoretical reason to believe that the assertion cannot fire.
For example, if some work is divided into an analysis phase and an
implementation phase, the analysis phase might reject inputs that are
not ``is_constant``, in which case the implementation phase can
reasonably use ``to_constant`` on the remaining inputs.  The assertions
should not be used to discover whether a condition ever occurs 'in the
field'; in other words, they should not be used to restrict code to
constants at first, with the intention of only implementing a
``poly_int`` version if a user hits the assertion.

If a particular asserting function like ``to_constant`` is needed
more than once for the same reason, it is probably worth adding a
helper function or macro for that situation, so that the justification
only needs to be given once.  For example:

.. code-block:: c++

  /* Return the size of an element in a vector of size SIZE, given that
     the vector has NELTS elements.  The return value is in the same units
     as SIZE (either bits or bytes).

     to_constant () is safe in this situation because vector elements are
     always constant-sized scalars.  */
  #define vector_element_size(SIZE, NELTS) \
    (exact_div (SIZE, NELTS).to_constant ())

Target-specific code in :samp:`config/{cpu}` only needs to handle
non-constant ``poly_int`` s if ``NUM_POLY_INT_COEFFS`` is greater
than one.  For other targets, ``poly_int`` degenerates to a compile-time
constant and is often interchangable with a normal scalar integer.
There are two main exceptions:

* Sometimes an explicit cast to an integer type might be needed, such as to
  resolve ambiguities in a ``?:`` expression, or when passing values
  through ``...`` to things like print functions.

* Target macros are included in target-independent code and so do not
  have access to the implicit conversion to a scalar integer.
  If this becomes a problem for a particular target macro, the
  possible solutions, in order of preference, are:

  * Convert the target macro to a target hook (for all targets).

  * Put the target's implementation of the target macro in its
    :samp:`{cpu}.c` file and call it from the target macro in the
    :samp:`{cpu}.h` file.

  * Add ``to_constant ()`` calls where necessary.  The previous option
    is preferable because it will help with any future conversion of the
    macro to a hook.