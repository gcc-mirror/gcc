..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Pattern Ordering, Ordering of Patterns

.. _pattern-ordering:

When the Order of Patterns Matters
**********************************

Sometimes an insn can match more than one instruction pattern.  Then the
pattern that appears first in the machine description is the one used.
Therefore, more specific patterns (patterns that will match fewer things)
and faster instructions (those that will produce better code when they
do match) should usually go first in the description.

In some cases the effect of ordering the patterns can be used to hide
a pattern when it is not valid.  For example, the 68000 has an
instruction for converting a fullword to floating point and another
for converting a byte to floating point.  An instruction converting
an integer to floating point could match either one.  We put the
pattern to convert the fullword first to make sure that one will
be used rather than the other.  (Otherwise a large integer might
be generated as a single-byte immediate quantity, which would not work.)
Instead of using this pattern ordering it would be possible to make the
pattern for convert-a-byte smart enough to deal properly with any
constant value.