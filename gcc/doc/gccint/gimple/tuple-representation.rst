..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: tuples

.. _tuple-representation:

Tuple representation
********************

GIMPLE instructions are tuples of variable size divided in two
groups: a header describing the instruction and its locations,
and a variable length body with all the operands. Tuples are
organized into a hierarchy with 3 main classes of tuples.

.. index:: gimple

gimple (gsbase)
^^^^^^^^^^^^^^^

This is the root of the hierarchy, it holds basic information
needed by most GIMPLE statements. There are some fields that
may not be relevant to every GIMPLE statement, but those were
moved into the base structure to take advantage of holes left by
other fields (thus making the structure more compact).  The
structure takes 4 words (32 bytes) on 64 bit hosts:

.. list-table::

   * - Field
     - Size (bits)
   * - ``code``
     - 8
   * - ``subcode``
     - 16
   * - ``no_warning``
     - 1
   * - ``visited``
     - 1
   * - ``nontemporal_move``
     - 1
   * - ``plf``
     - 2
   * - ``modified``
     - 1
   * - ``has_volatile_ops``
     - 1
   * - ``references_memory_p``
     - 1
   * - ``uid``
     - 32
   * - ``location``
     - 32
   * - ``num_ops``
     - 32
   * - ``bb``
     - 64
   * - ``block``
     - 63
   * - Total size
     - 32 bytes

* ``code``
  Main identifier for a GIMPLE instruction.

* ``subcode``
  Used to distinguish different variants of the same basic
  instruction or provide flags applicable to a given code. The
  ``subcode`` flags field has different uses depending on the code of
  the instruction, but mostly it distinguishes instructions of the
  same family. The most prominent use of this field is in
  assignments, where subcode indicates the operation done on the
  RHS of the assignment. For example, a = b + c is encoded as
  ``GIMPLE_ASSIGN <PLUS_EXPR, a, b, c>``.

* ``no_warning``
  Bitflag to indicate whether a warning has already been issued on
  this statement.

* ``visited``
  General purpose 'visited' marker. Set and cleared by each pass
  when needed.

* ``nontemporal_move``
  Bitflag used in assignments that represent non-temporal moves.
  Although this bitflag is only used in assignments, it was moved
  into the base to take advantage of the bit holes left by the
  previous fields.

* ``plf``
  Pass Local Flags. This 2-bit mask can be used as general purpose
  markers by any pass. Passes are responsible for clearing and
  setting these two flags accordingly.

* ``modified``
  Bitflag to indicate whether the statement has been modified.
  Used mainly by the operand scanner to determine when to re-scan a
  statement for operands.

* ``has_volatile_ops``
  Bitflag to indicate whether this statement contains operands that
  have been marked volatile.

* ``references_memory_p``
  Bitflag to indicate whether this statement contains memory
  references (i.e., its operands are either global variables, or
  pointer dereferences or anything that must reside in memory).

* ``uid``
  This is an unsigned integer used by passes that want to assign
  IDs to every statement. These IDs must be assigned and used by
  each pass.

* ``location``
  This is a ``location_t`` identifier to specify source code
  location for this statement. It is inherited from the front
  end.

* ``num_ops``
  Number of operands that this statement has. This specifies the
  size of the operand vector embedded in the tuple. Only used in
  some tuples, but it is declared in the base tuple to take
  advantage of the 32-bit hole left by the previous fields.

* ``bb``
  Basic block holding the instruction.

* ``block``
  Lexical block holding this statement.  Also used for debug
  information generation.

.. index:: gimple_statement_with_ops

gimple_statement_with_ops
^^^^^^^^^^^^^^^^^^^^^^^^^

This tuple is actually split in two:
``gimple_statement_with_ops_base`` and
``gimple_statement_with_ops``. This is needed to accommodate the
way the operand vector is allocated. The operand vector is
defined to be an array of 1 element. So, to allocate a dynamic
number of operands, the memory allocator (``gimple_alloc``) simply
allocates enough memory to hold the structure itself plus ``N
- 1`` operands which run 'off the end' of the structure. For
example, to allocate space for a tuple with 3 operands,
``gimple_alloc`` reserves ``sizeof (struct
gimple_statement_with_ops) + 2 * sizeof (tree)`` bytes.

On the other hand, several fields in this tuple need to be shared
with the ``gimple_statement_with_memory_ops`` tuple. So, these
common fields are placed in ``gimple_statement_with_ops_base`` which
is then inherited from the other two tuples.

.. list-table::

   * - ``gsbase``
     - 256
   * - ``def_ops``
     - 64
   * - ``use_ops``
     - 64
   * - ``op``
     - ``num_ops`` \* 64
   * - Total size
     - 48 + 8 \* ``num_ops`` bytes

* ``gsbase``
  Inherited from ``struct gimple``.

* ``def_ops``
  Array of pointers into the operand array indicating all the slots that
  contain a variable written-to by the statement. This array is
  also used for immediate use chaining. Note that it would be
  possible to not rely on this array, but the changes required to
  implement this are pretty invasive.

* ``use_ops``
  Similar to ``def_ops`` but for variables read by the statement.

* ``op``
  Array of trees with ``num_ops`` slots.

gimple_statement_with_memory_ops
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This tuple is essentially identical to ``gimple_statement_with_ops``,
except that it contains 4 additional fields to hold vectors
related memory stores and loads.  Similar to the previous case,
the structure is split in two to accommodate for the operand
vector (``gimple_statement_with_memory_ops_base`` and
``gimple_statement_with_memory_ops``).

.. list-table::

   * - Field
     - Size (bits)
   * - ``gsbase``
     - 256
   * - ``def_ops``
     - 64
   * - ``use_ops``
     - 64
   * - ``vdef_ops``
     - 64
   * - ``vuse_ops``
     - 64
   * - ``stores``
     - 64
   * - ``loads``
     - 64
   * - ``op``
     - ``num_ops`` \* 64
   * - Total size
     - 80 + 8 \* ``num_ops`` bytes

* ``vdef_ops``
  Similar to ``def_ops`` but for ``VDEF`` operators. There is
  one entry per memory symbol written by this statement. This is
  used to maintain the memory SSA use-def and def-def chains.

* ``vuse_ops``
  Similar to ``use_ops`` but for ``VUSE`` operators. There is
  one entry per memory symbol loaded by this statement. This is
  used to maintain the memory SSA use-def chains.

* ``stores``
  Bitset with all the UIDs for the symbols written-to by the
  statement.  This is different than ``vdef_ops`` in that all the
  affected symbols are mentioned in this set.  If memory
  partitioning is enabled, the ``vdef_ops`` vector will refer to memory
  partitions. Furthermore, no SSA information is stored in this
  set.

* ``loads``
  Similar to ``stores``, but for memory loads. (Note that there
  is some amount of redundancy here, it should be possible to
  reduce memory utilization further by removing these sets).

All the other tuples are defined in terms of these three basic
ones. Each tuple will add some fields.