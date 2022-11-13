..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: statement chaining

.. _gfc_code:

gfc_code
********

.. index:: gfc_code, struct gfc_code

The executable statements in a program unit are represented by a
nested chain of ``gfc_code`` structures.  The type of statement is
identified by the ``op`` member of the structure, the different
possible values are enumerated in ``gfc_exec_op``.  A special
member of this ``enum`` is ``EXEC_NOP`` which is used to
represent the various ``END`` statements if they carry a label.
Depending on the type of statement some of the other fields will be
filled in.  Fields that are generally applicable are the ``next``
and ``here`` fields.  The former points to the next statement in
the current block or is ``NULL`` if the current statement is the
last in a block, ``here`` points to the statement label of the
current statement.

If the current statement is one of ``IF``, ``DO``, ``SELECT``
it starts a block, i.e. a nested level in the program.  In order to
represent this, the ``block`` member is set to point to a
``gfc_code`` structure whose ``next`` member starts the chain of
statements inside the block; this structure's ``op`` member should be set to
the same value as the parent structure's ``op`` member.  The ``SELECT``
and ``IF`` statements may contain various blocks (the chain of ``ELSE IF``
and ``ELSE`` blocks or the various ``CASE`` s, respectively).  These chains
are linked-lists formed by the ``block`` members.

Consider the following example code:

.. code-block:: fortran

  IF (foo < 20) THEN
    PRINT *, "Too small"
    foo = 20
  ELSEIF (foo > 50) THEN
    PRINT *, "Too large"
    foo = 50
  ELSE
    PRINT *, "Good"
  END IF

This statement-block will be represented in the internal gfortran tree as
follows, were the horizontal link-chains are those induced by the ``next``
members and vertical links down are those of ``block``. :samp:`==|` and
:samp:`--|` mean ``NULL`` pointers to mark the end of a chain:

.. code-block:: c++

  ... ==> IF ==> ...
          |
          +--> IF foo < 20 ==> PRINT *, "Too small" ==> foo = 20 ==|
               |
               +--> IF foo > 50 ==> PRINT *, "Too large" ==> foo = 50 ==|
                    |
                    +--> ELSE ==> PRINT *, "Good" ==|
                         |
                         +--|

IF Blocks
^^^^^^^^^

Conditionals are represented by ``gfc_code`` structures with their
``op`` member set to ``EXEC_IF``.  This structure's ``block``
member must point to another ``gfc_code`` node that is the header of the
if-block.  This header's ``op`` member must be set to ``EXEC_IF``, too,
its ``expr`` member holds the condition to check for, and its ``next``
should point to the code-chain of the statements to execute if the condition is
true.

If in addition an ``ELSEIF`` or ``ELSE`` block is present, the
``block`` member of the if-block-header node points to yet another
``gfc_code`` structure that is the header of the elseif- or else-block.  Its
structure is identical to that of the if-block-header, except that in case of an
``ELSE`` block without a new condition the ``expr`` member should be
``NULL``.  This block can itself have its ``block`` member point to the
next ``ELSEIF`` or ``ELSE`` block if there's a chain of them.

Loops
^^^^^

``DO`` loops are stored in the tree as ``gfc_code`` nodes with their
``op`` set to ``EXEC_DO`` for a ``DO`` loop with iterator variable and
to ``EXEC_DO_WHILE`` for infinite ``DO`` s and ``DO WHILE`` blocks.
Their ``block`` member should point to a ``gfc_code`` structure heading
the code-chain of the loop body; its ``op`` member should be set to
``EXEC_DO`` or ``EXEC_DO_WHILE``, too, respectively.

For ``DO WHILE`` loops, the loop condition is stored on the top
``gfc_code`` structure's ``expr`` member; ``DO`` forever loops are
simply ``DO WHILE`` loops with a constant ``.TRUE.`` loop condition in
the internal representation.

Similarly, ``DO`` loops with an iterator have instead of the condition their
``ext.iterator`` member set to the correct values for the loop iterator
variable and its range.

SELECT Statements
^^^^^^^^^^^^^^^^^

A ``SELECT`` block is introduced by a ``gfc_code`` structure with an
``op`` member of ``EXEC_SELECT`` and ``expr`` containing the expression
to evaluate and test.  Its ``block`` member starts a list of ``gfc_code``
structures linked together by their ``block`` members that stores the various
``CASE`` parts.

Each ``CASE`` node has its ``op`` member set to ``EXEC_SELECT``, too,
its ``next`` member points to the code-chain to be executed in the current
case-block, and ``extx.case_list`` contains the case-values this block
corresponds to.  The ``block`` member links to the next case in the list.

BLOCK and ASSOCIATE
^^^^^^^^^^^^^^^^^^^

The code related to a ``BLOCK`` statement is stored inside an
``gfc_code`` structure (say :samp:`{c}`)
with ``c.op`` set to ``EXEC_BLOCK``.  The
``gfc_namespace`` holding the locally defined variables of the
``BLOCK`` is stored in ``c.ext.block.ns``.  The code inside the
construct is in ``c.code``.

``ASSOCIATE`` constructs are based on ``BLOCK`` and thus also have
the internal storage structure described above (including ``EXEC_BLOCK``).
However, for them ``c.ext.block.assoc`` is set additionally and points
to a linked list of ``gfc_association_list`` structures.  Those
structures basically store a link of associate-names to target expressions.
The associate-names themselves are still also added to the ``BLOCK`` 's
namespace as ordinary symbols, but they have their ``gfc_symbol`` 's
member ``assoc`` set also pointing to the association-list structure.
This way associate-names can be distinguished from ordinary variables
and their target expressions identified.

For association to expressions (as opposed to variables), at the very beginning
of the ``BLOCK`` construct assignments are automatically generated to
set the corresponding variables to their target expressions' values, and
later on the compiler simply disallows using such associate-names in contexts
that may change the value.