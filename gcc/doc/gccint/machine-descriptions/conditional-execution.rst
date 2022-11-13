..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: conditional execution, predication

.. _conditional-execution:

Conditional Execution
*********************

A number of architectures provide for some form of conditional
execution, or predication.  The hallmark of this feature is the
ability to nullify most of the instructions in the instruction set.
When the instruction set is large and not entirely symmetric, it
can be quite tedious to describe these forms directly in the
:samp:`.md` file.  An alternative is the ``define_cond_exec`` template.

.. index:: define_cond_exec

.. code-block::

  (define_cond_exec
    [predicate-pattern]
    "condition"
    "output-template"
    "optional-insn-attribues")

:samp:`{predicate-pattern}` is the condition that must be true for the
insn to be executed at runtime and should match a relational operator.
One can use ``match_operator`` to match several relational operators
at once.  Any ``match_operand`` operands must have no more than one
alternative.

:samp:`{condition}` is a C expression that must be true for the generated
pattern to match.

.. index:: current_insn_predicate

:samp:`{output-template}` is a string similar to the ``define_insn``
output template (see :ref:`output-template`), except that the :samp:`*`
and :samp:`@` special cases do not apply.  This is only useful if the
assembly text for the predicate is a simple prefix to the main insn.
In order to handle the general case, there is a global variable
``current_insn_predicate`` that will contain the entire predicate
if the current insn is predicated, and will otherwise be ``NULL``.

:samp:`{optional-insn-attributes}` is an optional vector of attributes that gets
appended to the insn attributes of the produced cond_exec rtx. It can
be used to add some distinguishing attribute to cond_exec rtxs produced
that way. An example usage would be to use this attribute in conjunction
with attributes on the main pattern to disable particular alternatives under
certain conditions.

When ``define_cond_exec`` is used, an implicit reference to
the ``predicable`` instruction attribute is made.
See :ref:`insn-attributes`.  This attribute must be a boolean (i.e. have
exactly two elements in its :samp:`{list-of-values}`), with the possible
values being ``no`` and ``yes``.  The default and all uses in
the insns must be a simple constant, not a complex expressions.  It
may, however, depend on the alternative, by using a comma-separated
list of values.  If that is the case, the port should also define an
``enabled`` attribute (see :ref:`disable-insn-alternatives`), which
should also allow only ``no`` and ``yes`` as its values.

For each ``define_insn`` for which the ``predicable``
attribute is true, a new ``define_insn`` pattern will be
generated that matches a predicated version of the instruction.
For example,

.. code-block::

  (define_insn "addsi"
    [(set (match_operand:SI 0 "register_operand" "r")
          (plus:SI (match_operand:SI 1 "register_operand" "r")
                   (match_operand:SI 2 "register_operand" "r")))]
    "test1"
    "add %2,%1,%0")

  (define_cond_exec
    [(ne (match_operand:CC 0 "register_operand" "c")
         (const_int 0))]
    "test2"
    "(%0)")

generates a new pattern

.. code-block::

  (define_insn ""
    [(cond_exec
       (ne (match_operand:CC 3 "register_operand" "c") (const_int 0))
       (set (match_operand:SI 0 "register_operand" "r")
            (plus:SI (match_operand:SI 1 "register_operand" "r")
                     (match_operand:SI 2 "register_operand" "r"))))]
    "(test2) && (test1)"
    "(%3) add %2,%1,%0")