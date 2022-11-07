..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: define_subst

.. _define-subst:

RTL Templates Transformations
*****************************

For some hardware architectures there are common cases when the RTL
templates for the instructions can be derived from the other RTL
templates using simple transformations.  E.g., :samp:`i386.md` contains
an RTL template for the ordinary ``sub`` instruction---
``*subsi_1``, and for the ``sub`` instruction with subsequent
zero-extension--- ``*subsi_1_zext``.  Such cases can be easily
implemented by a single meta-template capable of generating a modified
case based on the initial one:

.. index:: define_subst

.. code-block::

  (define_subst "name"
    [input-template]
    "condition"
    [output-template])

:samp:`{input-template}` is a pattern describing the source RTL template,
which will be transformed.

:samp:`{condition}` is a C expression that is conjunct with the condition
from the input-template to generate a condition to be used in the
output-template.

:samp:`{output-template}` is a pattern that will be used in the resulting
template.

``define_subst`` mechanism is tightly coupled with the notion of the
subst attribute (see :ref:`subst-iterators`).  The use of
``define_subst`` is triggered by a reference to a subst attribute in
the transforming RTL template.  This reference initiates duplication of
the source RTL template and substitution of the attributes with their
values.  The source RTL template is left unchanged, while the copy is
transformed by ``define_subst``.  This transformation can fail in the
case when the source RTL template is not matched against the
input-template of the ``define_subst``.  In such case the copy is
deleted.

``define_subst`` can be used only in ``define_insn`` and
``define_expand``, it cannot be used in other expressions (e.g. in
``define_insn_and_split``).

.. toctree::
  :maxdepth: 2


.. index:: define_subst

.. _define-subst-example:

define_subst Example
^^^^^^^^^^^^^^^^^^^^

To illustrate how ``define_subst`` works, let us examine a simple
template transformation.

Suppose there are two kinds of instructions: one that touches flags and
the other that does not.  The instructions of the second type could be
generated with the following ``define_subst`` :

.. code-block::

  (define_subst "add_clobber_subst"
    [(set (match_operand:SI 0 "" "")
          (match_operand:SI 1 "" ""))]
    ""
    [(set (match_dup 0)
          (match_dup 1))
     (clobber (reg:CC FLAGS_REG))])

This ``define_subst`` can be applied to any RTL pattern containing
``set`` of mode SI and generates a copy with clobber when it is
applied.

Assume there is an RTL template for a ``max`` instruction to be used
in ``define_subst`` mentioned above:

.. code-block::

  (define_insn "maxsi"
    [(set (match_operand:SI 0 "register_operand" "=r")
          (max:SI
            (match_operand:SI 1 "register_operand" "r")
            (match_operand:SI 2 "register_operand" "r")))]
    ""
    "max\t{%2, %1, %0|%0, %1, %2}"
   [...])

To mark the RTL template for ``define_subst`` application,
subst-attributes are used.  They should be declared in advance:

.. code-block::

  (define_subst_attr "add_clobber_name" "add_clobber_subst" "_noclobber" "_clobber")

Here :samp:`add_clobber_name` is the attribute name,
:samp:`add_clobber_subst` is the name of the corresponding
``define_subst``, the third argument (:samp:`_noclobber`) is the
attribute value that would be substituted into the unchanged version of
the source RTL template, and the last argument (:samp:`_clobber`) is the
value that would be substituted into the second, transformed,
version of the RTL template.

Once the subst-attribute has been defined, it should be used in RTL
templates which need to be processed by the ``define_subst``.  So,
the original RTL template should be changed:

.. code-block::

  (define_insn "maxsi<add_clobber_name>"
    [(set (match_operand:SI 0 "register_operand" "=r")
          (max:SI
            (match_operand:SI 1 "register_operand" "r")
            (match_operand:SI 2 "register_operand" "r")))]
    ""
    "max\t{%2, %1, %0|%0, %1, %2}"
   [...])

The result of the ``define_subst`` usage would look like the following:

.. code-block::

  (define_insn "maxsi_noclobber"
    [(set (match_operand:SI 0 "register_operand" "=r")
          (max:SI
            (match_operand:SI 1 "register_operand" "r")
            (match_operand:SI 2 "register_operand" "r")))]
    ""
    "max\t{%2, %1, %0|%0, %1, %2}"
   [...])
  (define_insn "maxsi_clobber"
    [(set (match_operand:SI 0 "register_operand" "=r")
          (max:SI
            (match_operand:SI 1 "register_operand" "r")
            (match_operand:SI 2 "register_operand" "r")))
     (clobber (reg:CC FLAGS_REG))]
    ""
    "max\t{%2, %1, %0|%0, %1, %2}"
   [...])

.. index:: define_subst

.. _define-subst-pattern-matching:

Pattern Matching in define_subst
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

All expressions, allowed in ``define_insn`` or ``define_expand``,
are allowed in the input-template of ``define_subst``, except
``match_par_dup``, ``match_scratch``, ``match_parallel``. The
meanings of expressions in the input-template were changed:

``match_operand`` matches any expression (possibly, a subtree in
RTL-template), if modes of the ``match_operand`` and this expression
are the same, or mode of the ``match_operand`` is ``VOIDmode``, or
this expression is ``match_dup``, ``match_op_dup``.  If the
expression is ``match_operand`` too, and predicate of
``match_operand`` from the input pattern is not empty, then the
predicates are compared.  That can be used for more accurate filtering
of accepted RTL-templates.

``match_operator`` matches common operators (like ``plus``,
``minus``), ``unspec``, ``unspec_volatile`` operators and
``match_operator`` s from the original pattern if the modes match and
``match_operator`` from the input pattern has the same number of
operands as the operator from the original pattern.

.. index:: define_subst

.. _define-subst-output-template:

Generation of output template in define_subst
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If all necessary checks for ``define_subst`` application pass, a new
RTL-pattern, based on the output-template, is created to replace the old
template.  Like in input-patterns, meanings of some RTL expressions are
changed when they are used in output-patterns of a ``define_subst``.
Thus, ``match_dup`` is used for copying the whole expression from the
original pattern, which matched corresponding ``match_operand`` from
the input pattern.

``match_dup N`` is used in the output template to be replaced with
the expression from the original pattern, which matched
``match_operand N`` from the input pattern.  As a consequence,
``match_dup`` cannot be used to point to ``match_operand`` s from
the output pattern, it should always refer to a ``match_operand``
from the input pattern.  If a ``match_dup N`` occurs more than once
in the output template, its first occurrence is replaced with the
expression from the original pattern, and the subsequent expressions
are replaced with ``match_dup N``, i.e., a reference to the first
expression.

In the output template one can refer to the expressions from the
original pattern and create new ones.  For instance, some operands could
be added by means of standard ``match_operand``.

After replacing ``match_dup`` with some RTL-subtree from the original
pattern, it could happen that several ``match_operand`` s in the
output pattern have the same indexes.  It is unknown, how many and what
indexes would be used in the expression which would replace
``match_dup``, so such conflicts in indexes are inevitable.  To
overcome this issue, ``match_operands`` and ``match_operators``,
which were introduced into the output pattern, are renumerated when all
``match_dup`` s are replaced.

Number of alternatives in ``match_operand`` s introduced into the
output template ``M`` could differ from the number of alternatives in
the original pattern ``N``, so in the resultant pattern there would
be ``N*M`` alternatives.  Thus, constraints from the original pattern
would be duplicated ``N`` times, constraints from the output pattern
would be duplicated ``M`` times, producing all possible combinations.