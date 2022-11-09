..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: insn attributes, instruction attributes

.. _insn-attributes:

Instruction Attributes
**********************

In addition to describing the instruction supported by the target machine,
the :samp:`md` file also defines a group of :dfn:`attributes` and a set of
values for each.  Every generated insn is assigned a value for each attribute.
One possible attribute would be the effect that the insn has on the machine's
condition code.

.. toctree::
  :maxdepth: 2


.. index:: defining attributes and their values, attributes, defining, define_attr

.. _defining-attributes:

Defining Attributes and their Values
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``define_attr`` expression is used to define each attribute required
by the target machine.  It looks like:

.. code-block::

  (define_attr name list-of-values default)

:samp:`{name}` is a string specifying the name of the attribute being
defined.  Some attributes are used in a special way by the rest of the
compiler. The ``enabled`` attribute can be used to conditionally
enable or disable insn alternatives (see :ref:`disable-insn-alternatives`). The ``predicable`` attribute, together with a
suitable ``define_cond_exec`` (see :ref:`conditional-execution`), can
be used to automatically generate conditional variants of instruction
patterns. The ``mnemonic`` attribute can be used to check for the
instruction mnemonic (see :ref:`mnemonic-attribute`).  The compiler
internally uses the names ``ce_enabled`` and ``nonce_enabled``,
so they should not be used elsewhere as alternative names.

:samp:`{list-of-values}` is either a string that specifies a comma-separated
list of values that can be assigned to the attribute, or a null string to
indicate that the attribute takes numeric values.

:samp:`{default}` is an attribute expression that gives the value of this
attribute for insns that match patterns whose definition does not include
an explicit value for this attribute.  See :ref:`attr-example`, for more
information on the handling of defaults.  See :ref:`constant-attributes`,
for information on attributes that do not depend on any particular insn.

.. index:: insn-attr.h

For each defined attribute, a number of definitions are written to the
:samp:`insn-attr.h` file.  For cases where an explicit set of values is
specified for an attribute, the following are defined:

* A :samp:`#define` is written for the symbol :samp:`HAVE_ATTR_{name}`.

* An enumerated class is defined for :samp:`attr_{name}` with
  elements of the form :samp:`{upper-name}_{upper-value}` where
  the attribute name and value are first converted to uppercase.

* A function :samp:`get_attr_{name}` is defined that is passed an insn and
  returns the attribute value for that insn.

For example, if the following is present in the :samp:`md` file:

.. code-block::

  (define_attr "type" "branch,fp,load,store,arith" ...)

the following lines will be written to the file :samp:`insn-attr.h`.

.. code-block:: c++

  #define HAVE_ATTR_type 1
  enum attr_type {TYPE_BRANCH, TYPE_FP, TYPE_LOAD,
                   TYPE_STORE, TYPE_ARITH};
  extern enum attr_type get_attr_type ();

If the attribute takes numeric values, no ``enum`` type will be
defined and the function to obtain the attribute's value will return
``int``.

There are attributes which are tied to a specific meaning.  These
attributes are not free to use for other purposes:

``length``
  The ``length`` attribute is used to calculate the length of emitted
  code chunks.  This is especially important when verifying branch
  distances. See :ref:`insn-lengths`.

``enabled``
  The ``enabled`` attribute can be defined to prevent certain
  alternatives of an insn definition from being used during code
  generation. See :ref:`disable-insn-alternatives`.

``mnemonic``
  The ``mnemonic`` attribute can be defined to implement instruction
  specific checks in e.g. the pipeline description.
  See :ref:`mnemonic-attribute`.

For each of these special attributes, the corresponding
:samp:`HAVE_ATTR_{name}` :samp:`#define` is also written when the
attribute is not defined; in that case, it is defined as :samp:`0`.

.. index:: define_enum_attr

.. _define_enum_attr:

Another way of defining an attribute is to use:

.. code-block::

  (define_enum_attr "attr" "enum" default)

This works in just the same way as ``define_attr``, except that
the list of values is taken from a separate enumeration called
:samp:`{enum}` (see :ref:`define_enum`).  This form allows you to use
the same list of values for several attributes without having to
repeat the list each time.  For example:

.. code-block::

  (define_enum "processor" [
    model_a
    model_b
    ...
  ])
  (define_enum_attr "arch" "processor"
    (const (symbol_ref "target_arch")))
  (define_enum_attr "tune" "processor"
    (const (symbol_ref "target_tune")))

defines the same attributes as:

.. code-block::

  (define_attr "arch" "model_a,model_b,..."
    (const (symbol_ref "target_arch")))
  (define_attr "tune" "model_a,model_b,..."
    (const (symbol_ref "target_tune")))

but without duplicating the processor list.  The second example defines two
separate C enums (``attr_arch`` and ``attr_tune``) whereas the first
defines a single C enum (``processor``).

.. index:: attribute expressions

.. _expressions:

Attribute Expressions
^^^^^^^^^^^^^^^^^^^^^

RTL expressions used to define attributes use the codes described above
plus a few specific to attribute definitions, to be discussed below.
Attribute value expressions must have one of the following forms:

.. index:: const_int and attributes

:samp:`(const_int {i})`
  The integer :samp:`{i}` specifies the value of a numeric attribute.  :samp:`{i}`
  must be non-negative.

  The value of a numeric attribute can be specified either with a
  ``const_int``, or as an integer represented as a string in
  ``const_string``, ``eq_attr`` (see below), ``attr``,
  ``symbol_ref``, simple arithmetic expressions, and ``set_attr``
  overrides on specific instructions (see :ref:`tagging-insns`).

  .. index:: const_string and attributes

:samp:`(const_string {value})`
  The string :samp:`{value}` specifies a constant attribute value.
  If :samp:`{value}` is specified as :samp:`"*"`, it means that the default value of
  the attribute is to be used for the insn containing this expression.
  :samp:`"*"` obviously cannot be used in the :samp:`{default}` expression
  of a ``define_attr``.

  If the attribute whose value is being specified is numeric, :samp:`{value}`
  must be a string containing a non-negative integer (normally
  ``const_int`` would be used in this case).  Otherwise, it must
  contain one of the valid values for the attribute.

  .. index:: if_then_else and attributes

:samp:`(if_then_else {test} {true-value} {false-value})`
  :samp:`{test}` specifies an attribute test, whose format is defined below.
  The value of this expression is :samp:`{true-value}` if :samp:`{test}` is true,
  otherwise it is :samp:`{false-value}`.

  .. index:: cond and attributes

:samp:`(cond [{test1} {value1} ...] {default})`
  The first operand of this expression is a vector containing an even
  number of expressions and consisting of pairs of :samp:`{test}` and :samp:`{value}`
  expressions.  The value of the ``cond`` expression is that of the
  :samp:`{value}` corresponding to the first true :samp:`{test}` expression.  If
  none of the :samp:`{test}` expressions are true, the value of the ``cond``
  expression is that of the :samp:`{default}` expression.

  :samp:`{test}` expressions can have one of the following forms:

.. index:: const_int and attribute tests

:samp:`(const_int {i})`
  This test is true if :samp:`{i}` is nonzero and false otherwise.

  .. index:: not and attributes, ior and attributes, and and attributes

:samp:`(not {test})` :samp:`(ior {test1} {test2})` :samp:`(and {test1} {test2})`
  These tests are true if the indicated logical function is true.

  .. index:: match_operand and attributes

:samp:`(match_operand:{m} {n} {pred} {constraints})`
  This test is true if operand :samp:`{n}` of the insn whose attribute value
  is being determined has mode :samp:`{m}` (this part of the test is ignored
  if :samp:`{m}` is ``VOIDmode``) and the function specified by the string
  :samp:`{pred}` returns a nonzero value when passed operand :samp:`{n}` and mode
  :samp:`{m}` (this part of the test is ignored if :samp:`{pred}` is the null
  string).

  The :samp:`{constraints}` operand is ignored and should be the null string.

  .. index:: match_test and attributes

:samp:`(match_test {c-expr})`
  The test is true if C expression :samp:`{c-expr}` is true.  In non-constant
  attributes, :samp:`{c-expr}` has access to the following variables:

  :samp:`{insn}`
    The rtl instruction under test.

  :samp:`{which_alternative}`
    The ``define_insn`` alternative that :samp:`{insn}` matches.
    See :ref:`output-statement`.

  :samp:`{operands}`
    An array of :samp:`{insn}` 's rtl operands.

  :samp:`{c-expr}` behaves like the condition in a C ``if`` statement,
  so there is no need to explicitly convert the expression into a boolean
  0 or 1 value.  For example, the following two tests are equivalent:

  .. code-block:: c++

    (match_test "x & 2")
    (match_test "(x & 2) != 0")

  .. index:: le and attributes, leu and attributes, lt and attributes, gt and attributes, gtu and attributes, ge and attributes, geu and attributes, ne and attributes, eq and attributes, plus and attributes, minus and attributes, mult and attributes, div and attributes, mod and attributes, abs and attributes, neg and attributes, ashift and attributes, lshiftrt and attributes, ashiftrt and attributes

:samp:`(le {arith1} {arith2})` :samp:`(leu {arith1} {arith2})` :samp:`(lt {arith1} {arith2})` :samp:`(ltu {arith1} {arith2})` :samp:`(gt {arith1} {arith2})` :samp:`(gtu {arith1} {arith2})` :samp:`(ge {arith1} {arith2})` :samp:`(geu {arith1} {arith2})` :samp:`(ne {arith1} {arith2})` :samp:`(eq {arith1} {arith2})`
  These tests are true if the indicated comparison of the two arithmetic
  expressions is true.  Arithmetic expressions are formed with
  ``plus``, ``minus``, ``mult``, ``div``, ``mod``,
  ``abs``, ``neg``, ``and``, ``ior``, ``xor``, ``not``,
  ``ashift``, ``lshiftrt``, and ``ashiftrt`` expressions.

  .. index:: get_attr

  ``const_int`` and ``symbol_ref`` are always valid terms (see :ref:`insn-lengths`,for additional forms).  ``symbol_ref`` is a string
  denoting a C expression that yields an ``int`` when evaluated by the
  :samp:`get_attr_...` routine.  It should normally be a global
  variable.

  .. index:: eq_attr

:samp:`(eq_attr {name} {value})`
  :samp:`{name}` is a string specifying the name of an attribute.

  :samp:`{value}` is a string that is either a valid value for attribute
  :samp:`{name}`, a comma-separated list of values, or :samp:`!` followed by a
  value or list.  If :samp:`{value}` does not begin with a :samp:`!`, this
  test is true if the value of the :samp:`{name}` attribute of the current
  insn is in the list specified by :samp:`{value}`.  If :samp:`{value}` begins
  with a :samp:`!`, this test is true if the attribute's value is
  *not* in the specified list.

  For example,

  .. code-block::

    (eq_attr "type" "load,store")

  is equivalent to

  .. code-block::

    (ior (eq_attr "type" "load") (eq_attr "type" "store"))

  If :samp:`{name}` specifies an attribute of :samp:`alternative`, it refers to the
  value of the compiler variable ``which_alternative``
  (see :ref:`output-statement`) and the values must be small integers.  For
  example,

  .. code-block::

    (eq_attr "alternative" "2,3")

  is equivalent to

  .. code-block:: c++

    (ior (eq (symbol_ref "which_alternative") (const_int 2))
         (eq (symbol_ref "which_alternative") (const_int 3)))

  Note that, for most attributes, an ``eq_attr`` test is simplified in cases
  where the value of the attribute being tested is known for all insns matching
  a particular pattern.  This is by far the most common case.

  .. index:: attr_flag

:samp:`(attr_flag {name})`
  The value of an ``attr_flag`` expression is true if the flag
  specified by :samp:`{name}` is true for the ``insn`` currently being
  scheduled.

  :samp:`{name}` is a string specifying one of a fixed set of flags to test.
  Test the flags ``forward`` and ``backward`` to determine the
  direction of a conditional branch.

  This example describes a conditional branch delay slot which
  can be nullified for forward branches that are taken (annul-true) or
  for backward branches which are not taken (annul-false).

  .. code-block::

    (define_delay (eq_attr "type" "cbranch")
      [(eq_attr "in_branch_delay" "true")
       (and (eq_attr "in_branch_delay" "true")
            (attr_flag "forward"))
       (and (eq_attr "in_branch_delay" "true")
            (attr_flag "backward"))])

  The ``forward`` and ``backward`` flags are false if the current
  ``insn`` being scheduled is not a conditional branch.

  ``attr_flag`` is only used during delay slot scheduling and has no
  meaning to other passes of the compiler.

  .. index:: attr

:samp:`(attr {name})`
  The value of another attribute is returned.  This is most useful
  for numeric attributes, as ``eq_attr`` and ``attr_flag``
  produce more efficient code for non-numeric attributes.

.. index:: tagging insns, assigning attribute values to insns

.. _tagging-insns:

Assigning Attribute Values to Insns
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The value assigned to an attribute of an insn is primarily determined by
which pattern is matched by that insn (or which ``define_peephole``
generated it).  Every ``define_insn`` and ``define_peephole`` can
have an optional last argument to specify the values of attributes for
matching insns.  The value of any attribute not specified in a particular
insn is set to the default value for that attribute, as specified in its
``define_attr``.  Extensive use of default values for attributes
permits the specification of the values for only one or two attributes
in the definition of most insn patterns, as seen in the example in the
next section.

The optional last argument of ``define_insn`` and
``define_peephole`` is a vector of expressions, each of which defines
the value for a single attribute.  The most general way of assigning an
attribute's value is to use a ``set`` expression whose first operand is an
``attr`` expression giving the name of the attribute being set.  The
second operand of the ``set`` is an attribute expression
(see :ref:`expressions`) giving the value of the attribute.

When the attribute value depends on the :samp:`alternative` attribute
(i.e., which is the applicable alternative in the constraint of the
insn), the ``set_attr_alternative`` expression can be used.  It
allows the specification of a vector of attribute expressions, one for
each alternative.

.. index:: set_attr

When the generality of arbitrary attribute expressions is not required,
the simpler ``set_attr`` expression can be used, which allows
specifying a string giving either a single attribute value or a list
of attribute values, one for each alternative.

The form of each of the above specifications is shown below.  In each case,
:samp:`{name}` is a string specifying the attribute to be set.

:samp:`(set_attr {name} {value-string})`
  :samp:`{value-string}` is either a string giving the desired attribute value,
  or a string containing a comma-separated list giving the values for
  succeeding alternatives.  The number of elements must match the number
  of alternatives in the constraint of the insn pattern.

  Note that it may be useful to specify :samp:`*` for some alternative, in
  which case the attribute will assume its default value for insns matching
  that alternative.

  .. index:: set_attr_alternative

:samp:`(set_attr_alternative {name} [{value1} {value2} ...])`
  Depending on the alternative of the insn, the value will be one of the
  specified values.  This is a shorthand for using a ``cond`` with
  tests on the :samp:`alternative` attribute.

  .. index:: attr

:samp:`(set (attr {name}) {value})`
  The first operand of this ``set`` must be the special RTL expression
  ``attr``, whose sole operand is a string giving the name of the
  attribute being set.  :samp:`{value}` is the value of the attribute.

The following shows three different ways of representing the same
attribute value specification:

.. code-block::

  (set_attr "type" "load,store,arith")

  (set_attr_alternative "type"
                        [(const_string "load") (const_string "store")
                         (const_string "arith")])

  (set (attr "type")
       (cond [(eq_attr "alternative" "1") (const_string "load")
              (eq_attr "alternative" "2") (const_string "store")]
             (const_string "arith")))

.. index:: define_asm_attributes

The ``define_asm_attributes`` expression provides a mechanism to
specify the attributes assigned to insns produced from an ``asm``
statement.  It has the form:

.. code-block::

  (define_asm_attributes [attr-sets])

where :samp:`{attr-sets}` is specified the same as for both the
``define_insn`` and the ``define_peephole`` expressions.

These values will typically be the 'worst case' attribute values.  For
example, they might indicate that the condition code will be clobbered.

A specification for a ``length`` attribute is handled specially.  The
way to compute the length of an ``asm`` insn is to multiply the
length specified in the expression ``define_asm_attributes`` by the
number of machine instructions specified in the ``asm`` statement,
determined by counting the number of semicolons and newlines in the
string.  Therefore, the value of the ``length`` attribute specified
in a ``define_asm_attributes`` should be the maximum possible length
of a single machine instruction.

.. index:: attribute specifications example, attribute specifications

.. _attr-example:

Example of Attribute Specifications
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The judicious use of defaulting is important in the efficient use of
insn attributes.  Typically, insns are divided into :dfn:`types` and an
attribute, customarily called ``type``, is used to represent this
value.  This attribute is normally used only to define the default value
for other attributes.  An example will clarify this usage.

Assume we have a RISC machine with a condition code and in which only
full-word operations are performed in registers.  Let us assume that we
can divide all insns into loads, stores, (integer) arithmetic
operations, floating point operations, and branches.

Here we will concern ourselves with determining the effect of an insn on
the condition code and will limit ourselves to the following possible
effects:  The condition code can be set unpredictably (clobbered), not
be changed, be set to agree with the results of the operation, or only
changed if the item previously set into the condition code has been
modified.

Here is part of a sample :samp:`md` file for such a machine:

.. code-block::

  (define_attr "type" "load,store,arith,fp,branch" (const_string "arith"))

  (define_attr "cc" "clobber,unchanged,set,change0"
               (cond [(eq_attr "type" "load")
                          (const_string "change0")
                      (eq_attr "type" "store,branch")
                          (const_string "unchanged")
                      (eq_attr "type" "arith")
                          (if_then_else (match_operand:SI 0 "" "")
                                        (const_string "set")
                                        (const_string "clobber"))]
                     (const_string "clobber")))

  (define_insn ""
    [(set (match_operand:SI 0 "general_operand" "=r,r,m")
          (match_operand:SI 1 "general_operand" "r,m,r"))]
    ""
    "@
     move %0,%1
     load %0,%1
     store %0,%1"
    [(set_attr "type" "arith,load,store")])

Note that we assume in the above example that arithmetic operations
performed on quantities smaller than a machine word clobber the condition
code since they will set the condition code to a value corresponding to the
full-word result.

.. index:: insn lengths, computing, computing the length of an insn

.. _insn-lengths:

Computing the Length of an Insn
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For many machines, multiple types of branch instructions are provided, each
for different length branch displacements.  In most cases, the assembler
will choose the correct instruction to use.  However, when the assembler
cannot do so, GCC can when a special attribute, the ``length``
attribute, is defined.  This attribute must be defined to have numeric
values by specifying a null string in its ``define_attr``.

In the case of the ``length`` attribute, two additional forms of
arithmetic terms are allowed in test expressions:

.. index:: match_dup and attributes

:samp:`(match_dup {n})`
  This refers to the address of operand :samp:`{n}` of the current insn, which
  must be a ``label_ref``.

  .. index:: pc and attributes

``(pc)``
  For non-branch instructions and backward branch instructions, this refers
  to the address of the current insn.  But for forward branch instructions,
  this refers to the address of the next insn, because the length of the
  current insn is to be computed.

.. index:: addr_vec, length of, addr_diff_vec, length of

For normal insns, the length will be determined by value of the
``length`` attribute.  In the case of ``addr_vec`` and
``addr_diff_vec`` insn patterns, the length is computed as
the number of vectors multiplied by the size of each vector.

Lengths are measured in addressable storage units (bytes).

Note that it is possible to call functions via the ``symbol_ref``
mechanism to compute the length of an insn.  However, if you use this
mechanism you must provide dummy clauses to express the maximum length
without using the function call.  You can see an example of this in the
``pa`` machine description for the ``call_symref`` pattern.

The following macros can be used to refine the length computation:

.. index:: ADJUST_INSN_LENGTH

:samp:`ADJUST_INSN_LENGTH ({insn}, {length})`
  If defined, modifies the length assigned to instruction :samp:`{insn}` as a
  function of the context in which it is used.  :samp:`{length}` is an lvalue
  that contains the initially computed length of the insn and should be
  updated with the correct length of the insn.

  This macro will normally not be required.  A case in which it is
  required is the ROMP.  On this machine, the size of an ``addr_vec``
  insn must be increased by two to compensate for the fact that alignment
  may be required.

.. index:: get_attr_length

The routine that returns ``get_attr_length`` (the value of the
``length`` attribute) can be used by the output routine to
determine the form of the branch instruction to be written, as the
example below illustrates.

As an example of the specification of variable-length branches, consider
the IBM 360.  If we adopt the convention that a register will be set to
the starting address of a function, we can jump to labels within 4k of
the start using a four-byte instruction.  Otherwise, we need a six-byte
sequence to load the address from memory and then branch to it.

On such a machine, a pattern for a branch instruction might be specified
as follows:

.. code-block::

  (define_insn "jump"
    [(set (pc)
          (label_ref (match_operand 0 "" "")))]
    ""
  {
     return (get_attr_length (insn) == 4
             ? "b %l0" : "l r15,=a(%l0); br r15");
  }
    [(set (attr "length")
          (if_then_else (lt (match_dup 0) (const_int 4096))
                        (const_int 4)
                        (const_int 6)))])

.. index:: constant attributes

.. _constant-attributes:

Constant Attributes
^^^^^^^^^^^^^^^^^^^

A special form of ``define_attr``, where the expression for the
default value is a ``const`` expression, indicates an attribute that
is constant for a given run of the compiler.  Constant attributes may be
used to specify which variety of processor is used.  For example,

.. code-block::

  (define_attr "cpu" "m88100,m88110,m88000"
   (const
    (cond [(symbol_ref "TARGET_88100") (const_string "m88100")
           (symbol_ref "TARGET_88110") (const_string "m88110")]
          (const_string "m88000"))))

  (define_attr "memory" "fast,slow"
   (const
    (if_then_else (symbol_ref "TARGET_FAST_MEM")
                  (const_string "fast")
                  (const_string "slow"))))

The routine generated for constant attributes has no parameters as it
does not depend on any particular insn.  RTL expressions used to define
the value of a constant attribute may use the ``symbol_ref`` form,
but may not use either the ``match_operand`` form or ``eq_attr``
forms involving insn attributes.

.. index:: mnemonic attribute

.. _mnemonic-attribute:

Mnemonic Attribute
^^^^^^^^^^^^^^^^^^

The ``mnemonic`` attribute is a string type attribute holding the
instruction mnemonic for an insn alternative.  The attribute values
will automatically be generated by the machine description parser if
there is an attribute definition in the md file:

.. code-block::

  (define_attr "mnemonic" "unknown" (const_string "unknown"))

The default value can be freely chosen as long as it does not collide
with any of the instruction mnemonics.  This value will be used
whenever the machine description parser is not able to determine the
mnemonic string.  This might be the case for output templates
containing more than a single instruction as in
``"mvcle\t%0,%1,0\;jo\t.-4"``.

The ``mnemonic`` attribute set is not generated automatically if the
instruction string is generated via C code.

An existing ``mnemonic`` attribute set in an insn definition will not
be overriden by the md file parser.  That way it is possible to
manually set the instruction mnemonics for the cases where the md file
parser fails to determine it automatically.

The ``mnemonic`` attribute is useful for dealing with instruction
specific properties in the pipeline description without defining
additional insn attributes.

.. code-block::

  (define_attr "ooo_expanded" ""
    (cond [(eq_attr "mnemonic" "dlr,dsgr,d,dsgf,stam,dsgfr,dlgr")
           (const_int 1)]
          (const_int 0)))

.. index:: delay slots, defining

.. _delay-slots:

Delay Slot Scheduling
^^^^^^^^^^^^^^^^^^^^^

The insn attribute mechanism can be used to specify the requirements for
delay slots, if any, on a target machine.  An instruction is said to
require a :dfn:`delay slot` if some instructions that are physically
after the instruction are executed as if they were located before it.
Classic examples are branch and call instructions, which often execute
the following instruction before the branch or call is performed.

On some machines, conditional branch instructions can optionally
:dfn:`annul` instructions in the delay slot.  This means that the
instruction will not be executed for certain branch outcomes.  Both
instructions that annul if the branch is true and instructions that
annul if the branch is false are supported.

Delay slot scheduling differs from instruction scheduling in that
determining whether an instruction needs a delay slot is dependent only
on the type of instruction being generated, not on data flow between the
instructions.  See the next section for a discussion of data-dependent
instruction scheduling.

.. index:: define_delay

The requirement of an insn needing one or more delay slots is indicated
via the ``define_delay`` expression.  It has the following form:

.. code-block::

  (define_delay test
                [delay-1 annul-true-1 annul-false-1
                 delay-2 annul-true-2 annul-false-2
                 ...])

:samp:`{test}` is an attribute test that indicates whether this
``define_delay`` applies to a particular insn.  If so, the number of
required delay slots is determined by the length of the vector specified
as the second argument.  An insn placed in delay slot :samp:`{n}` must
satisfy attribute test :samp:`{delay-n}`.  :samp:`{annul-true-n}` is an
attribute test that specifies which insns may be annulled if the branch
is true.  Similarly, :samp:`{annul-false-n}` specifies which insns in the
delay slot may be annulled if the branch is false.  If annulling is not
supported for that delay slot, ``(nil)`` should be coded.

For example, in the common case where branch and call insns require
a single delay slot, which may contain any insn other than a branch or
call, the following would be placed in the :samp:`md` file:

.. code-block::

  (define_delay (eq_attr "type" "branch,call")
                [(eq_attr "type" "!branch,call") (nil) (nil)])

Multiple ``define_delay`` expressions may be specified.  In this
case, each such expression specifies different delay slot requirements
and there must be no insn for which tests in two ``define_delay``
expressions are both true.

For example, if we have a machine that requires one delay slot for branches
but two for calls,  no delay slot can contain a branch or call insn,
and any valid insn in the delay slot for the branch can be annulled if the
branch is true, we might represent this as follows:

.. code-block::

  (define_delay (eq_attr "type" "branch")
     [(eq_attr "type" "!branch,call")
      (eq_attr "type" "!branch,call")
      (nil)])

  (define_delay (eq_attr "type" "call")
                [(eq_attr "type" "!branch,call") (nil) (nil)
                 (eq_attr "type" "!branch,call") (nil) (nil)])

.. the above is *still* too long.  -mew 4feb93

.. index:: processor pipeline description, processor functional units, instruction latency time, interlock delays, data dependence delays, reservation delays, pipeline hazard recognizer, automaton based pipeline description, regular expressions, deterministic finite state automaton, automaton based scheduler, RISC, VLIW

.. _processor-pipeline-description:

Specifying processor pipeline description
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To achieve better performance, most modern processors
(super-pipelined, superscalar RISC, and VLIW
processors) have many :dfn:`functional units` on which several
instructions can be executed simultaneously.  An instruction starts
execution if its issue conditions are satisfied.  If not, the
instruction is stalled until its conditions are satisfied.  Such
:dfn:`interlock (pipeline) delay` causes interruption of the fetching
of successor instructions (or demands nop instructions, e.g. for some
MIPS processors).

There are two major kinds of interlock delays in modern processors.
The first one is a data dependence delay determining :dfn:`instruction
latency time`.  The instruction execution is not started until all
source data have been evaluated by prior instructions (there are more
complex cases when the instruction execution starts even when the data
are not available but will be ready in given time after the
instruction execution start).  Taking the data dependence delays into
account is simple.  The data dependence (true, output, and
anti-dependence) delay between two instructions is given by a
constant.  In most cases this approach is adequate.  The second kind
of interlock delays is a reservation delay.  The reservation delay
means that two instructions under execution will be in need of shared
processors resources, i.e. buses, internal registers, and/or
functional units, which are reserved for some time.  Taking this kind
of delay into account is complex especially for modern RISC
processors.

The task of exploiting more processor parallelism is solved by an
instruction scheduler.  For a better solution to this problem, the
instruction scheduler has to have an adequate description of the
processor parallelism (or :dfn:`pipeline description`).  GCC
machine descriptions describe processor parallelism and functional
unit reservations for groups of instructions with the aid of
:dfn:`regular expressions`.

The GCC instruction scheduler uses a :dfn:`pipeline hazard recognizer` to
figure out the possibility of the instruction issue by the processor
on a given simulated processor cycle.  The pipeline hazard recognizer is
automatically generated from the processor pipeline description.  The
pipeline hazard recognizer generated from the machine description
is based on a deterministic finite state automaton (DFA):
the instruction issue is possible if there is a transition from one
automaton state to another one.  This algorithm is very fast, and
furthermore, its speed is not dependent on processor
complexity [#f1]_.

.. [#f1] However, the size of the automaton depends on
  processor complexity.  To limit this effect, machine descriptions
  can split orthogonal parts of the machine description among several
  automata: but then, since each of these must be stepped independently,
  this does cause a small decrease in the algorithm's performance.

.. index:: automaton based pipeline description

The rest of this section describes the directives that constitute
an automaton-based processor pipeline description.  The order of
these constructions within the machine description file is not
important.

.. index:: define_automaton, pipeline hazard recognizer

The following optional construction describes names of automata
generated and used for the pipeline hazards recognition.  Sometimes
the generated finite state automaton used by the pipeline hazard
recognizer is large.  If we use more than one automaton and bind functional
units to the automata, the total size of the automata is usually
less than the size of the single automaton.  If there is no one such
construction, only one finite state automaton is generated.

.. code-block::

  (define_automaton automata-names)

:samp:`{automata-names}` is a string giving names of the automata.  The
names are separated by commas.  All the automata should have unique names.
The automaton name is used in the constructions ``define_cpu_unit`` and
``define_query_cpu_unit``.

.. index:: define_cpu_unit, processor functional units

Each processor functional unit used in the description of instruction
reservations should be described by the following construction.

.. code-block::

  (define_cpu_unit unit-names [automaton-name])

:samp:`{unit-names}` is a string giving the names of the functional units
separated by commas.  Don't use name :samp:`nothing`, it is reserved
for other goals.

:samp:`{automaton-name}` is a string giving the name of the automaton with
which the unit is bound.  The automaton should be described in
construction ``define_automaton``.  You should give
:dfn:`automaton-name`, if there is a defined automaton.

The assignment of units to automata are constrained by the uses of the
units in insn reservations.  The most important constraint is: if a
unit reservation is present on a particular cycle of an alternative
for an insn reservation, then some unit from the same automaton must
be present on the same cycle for the other alternatives of the insn
reservation.  The rest of the constraints are mentioned in the
description of the subsequent constructions.

.. index:: define_query_cpu_unit, querying function unit reservations

The following construction describes CPU functional units analogously
to ``define_cpu_unit``.  The reservation of such units can be
queried for an automaton state.  The instruction scheduler never
queries reservation of functional units for given automaton state.  So
as a rule, you don't need this construction.  This construction could
be used for future code generation goals (e.g. to generate
VLIW insn templates).

.. code-block::

  (define_query_cpu_unit unit-names [automaton-name])

:samp:`{unit-names}` is a string giving names of the functional units
separated by commas.

:samp:`{automaton-name}` is a string giving the name of the automaton with
which the unit is bound.

.. index:: define_insn_reservation, instruction latency time, regular expressions, data bypass

The following construction is the major one to describe pipeline
characteristics of an instruction.

.. code-block::

  (define_insn_reservation insn-name default_latency
                           condition regexp)

:samp:`{default_latency}` is a number giving latency time of the
instruction.  There is an important difference between the old
description and the automaton based pipeline description.  The latency
time is used for all dependencies when we use the old description.  In
the automaton based pipeline description, the given latency time is only
used for true dependencies.  The cost of anti-dependencies is always
zero and the cost of output dependencies is the difference between
latency times of the producing and consuming insns (if the difference
is negative, the cost is considered to be zero).  You can always
change the default costs for any description by using the target hook
``TARGET_SCHED_ADJUST_COST`` (see :ref:`scheduling`).

:samp:`{insn-name}` is a string giving the internal name of the insn.  The
internal names are used in constructions ``define_bypass`` and in
the automaton description file generated for debugging.  The internal
name has nothing in common with the names in ``define_insn``.  It is a
good practice to use insn classes described in the processor manual.

:samp:`{condition}` defines what RTL insns are described by this
construction.  You should remember that you will be in trouble if
:samp:`{condition}` for two or more different
``define_insn_reservation`` constructions is TRUE for an insn.  In
this case what reservation will be used for the insn is not defined.
Such cases are not checked during generation of the pipeline hazards
recognizer because in general recognizing that two conditions may have
the same value is quite difficult (especially if the conditions
contain ``symbol_ref``).  It is also not checked during the
pipeline hazard recognizer work because it would slow down the
recognizer considerably.

:samp:`{regexp}` is a string describing the reservation of the cpu's functional
units by the instruction.  The reservations are described by a regular
expression according to the following syntax:

.. code-block:: c++

         regexp = regexp "," oneof
                | oneof

         oneof = oneof "|" allof
               | allof

         allof = allof "+" repeat
               | repeat

         repeat = element "*" number
                | element

         element = cpu_function_unit_name
                 | reservation_name
                 | result_name
                 | "nothing"
                 | "(" regexp ")"

* :samp:`,` is used for describing the start of the next cycle in
  the reservation.

* :samp:`|` is used for describing a reservation described by the first
  regular expression **or** a reservation described by the second
  regular expression **or** etc.

* :samp:`+` is used for describing a reservation described by the first
  regular expression **and** a reservation described by the
  second regular expression **and** etc.

* :samp:`*` is used for convenience and simply means a sequence in which
  the regular expression are repeated :samp:`{number}` times with cycle
  advancing (see :samp:`,`).

* :samp:`cpu_function_unit_name` denotes reservation of the named
  functional unit.

* :samp:`reservation_name` --- see description of construction
  :samp:`define_reservation`.

* :samp:`nothing` denotes no unit reservations.

.. index:: define_reservation

Sometimes unit reservations for different insns contain common parts.
In such case, you can simplify the pipeline description by describing
the common part by the following construction

.. code-block::

  (define_reservation reservation-name regexp)

:samp:`{reservation-name}` is a string giving name of :samp:`{regexp}`.
Functional unit names and reservation names are in the same name
space.  So the reservation names should be different from the
functional unit names and cannot be the reserved name :samp:`nothing`.

.. index:: define_bypass, instruction latency time, data bypass

The following construction is used to describe exceptions in the
latency time for given instruction pair.  This is so called bypasses.

.. code-block::

  (define_bypass number out_insn_names in_insn_names
                 [guard])

:samp:`{number}` defines when the result generated by the instructions
given in string :samp:`{out_insn_names}` will be ready for the
instructions given in string :samp:`{in_insn_names}`.  Each of these
strings is a comma-separated list of filename-style globs and
they refer to the names of ``define_insn_reservation`` s.
For example:

.. code-block::

  (define_bypass 1 "cpu1_load_*, cpu1_store_*" "cpu1_load_*")

defines a bypass between instructions that start with
:samp:`cpu1_load_` or :samp:`cpu1_store_` and those that start with
:samp:`cpu1_load_`.

:samp:`{guard}` is an optional string giving the name of a C function which
defines an additional guard for the bypass.  The function will get the
two insns as parameters.  If the function returns zero the bypass will
be ignored for this case.  The additional guard is necessary to
recognize complicated bypasses, e.g. when the consumer is only an address
of insn :samp:`store` (not a stored value).

If there are more one bypass with the same output and input insns, the
chosen bypass is the first bypass with a guard in description whose
guard function returns nonzero.  If there is no such bypass, then
bypass without the guard function is chosen.

.. index:: exclusion_set, presence_set, final_presence_set, absence_set, final_absence_set, VLIW, RISC

The following five constructions are usually used to describe
VLIW processors, or more precisely, to describe a placement
of small instructions into VLIW instruction slots.  They
can be used for RISC processors, too.

.. code-block:: c++

  (exclusion_set unit-names unit-names)
  (presence_set unit-names patterns)
  (final_presence_set unit-names patterns)
  (absence_set unit-names patterns)
  (final_absence_set unit-names patterns)

:samp:`{unit-names}` is a string giving names of functional units
separated by commas.

:samp:`{patterns}` is a string giving patterns of functional units
separated by comma.  Currently pattern is one unit or units
separated by white-spaces.

The first construction (:samp:`exclusion_set`) means that each
functional unit in the first string cannot be reserved simultaneously
with a unit whose name is in the second string and vice versa.  For
example, the construction is useful for describing processors
(e.g. some SPARC processors) with a fully pipelined floating point
functional unit which can execute simultaneously only single floating
point insns or only double floating point insns.

The second construction (:samp:`presence_set`) means that each
functional unit in the first string cannot be reserved unless at
least one of pattern of units whose names are in the second string is
reserved.  This is an asymmetric relation.  For example, it is useful
for description that VLIW :samp:`slot1` is reserved after
:samp:`slot0` reservation.  We could describe it by the following
construction

.. code-block:: c++

  (presence_set "slot1" "slot0")

Or :samp:`slot1` is reserved only after :samp:`slot0` and unit :samp:`b0`
reservation.  In this case we could write

.. code-block:: c++

  (presence_set "slot1" "slot0 b0")

The third construction (:samp:`final_presence_set`) is analogous to
:samp:`presence_set`.  The difference between them is when checking is
done.  When an instruction is issued in given automaton state
reflecting all current and planned unit reservations, the automaton
state is changed.  The first state is a source state, the second one
is a result state.  Checking for :samp:`presence_set` is done on the
source state reservation, checking for :samp:`final_presence_set` is
done on the result reservation.  This construction is useful to
describe a reservation which is actually two subsequent reservations.
For example, if we use

.. code-block:: c++

  (presence_set "slot1" "slot0")

the following insn will be never issued (because :samp:`slot1` requires
:samp:`slot0` which is absent in the source state).

.. code-block::

  (define_reservation "insn_and_nop" "slot0 + slot1")

but it can be issued if we use analogous :samp:`final_presence_set`.

The forth construction (:samp:`absence_set`) means that each functional
unit in the first string can be reserved only if each pattern of units
whose names are in the second string is not reserved.  This is an
asymmetric relation (actually :samp:`exclusion_set` is analogous to
this one but it is symmetric).  For example it might be useful in a
VLIW description to say that :samp:`slot0` cannot be reserved
after either :samp:`slot1` or :samp:`slot2` have been reserved.  This
can be described as:

.. code-block:: c++

  (absence_set "slot0" "slot1, slot2")

Or :samp:`slot2` cannot be reserved if :samp:`slot0` and unit :samp:`b0`
are reserved or :samp:`slot1` and unit :samp:`b1` are reserved.  In
this case we could write

.. code-block:: c++

  (absence_set "slot2" "slot0 b0, slot1 b1")

All functional units mentioned in a set should belong to the same
automaton.

The last construction (:samp:`final_absence_set`) is analogous to
:samp:`absence_set` but checking is done on the result (state)
reservation.  See comments for :samp:`final_presence_set`.

.. index:: automata_option, deterministic finite state automaton, nondeterministic finite state automaton, finite state automaton minimization

You can control the generator of the pipeline hazard recognizer with
the following construction.

.. code-block:: c++

  (automata_option options)

:samp:`{options}` is a string giving options which affect the generated
code.  Currently there are the following options:

* :dfn:`no-minimization` makes no minimization of the automaton.  This is
  only worth to do when we are debugging the description and need to
  look more accurately at reservations of states.

* :dfn:`time` means printing time statistics about the generation of
  automata.

* :dfn:`stats` means printing statistics about the generated automata
  such as the number of DFA states, NDFA states and arcs.

* :dfn:`v` means a generation of the file describing the result automata.
  The file has suffix :samp:`.dfa` and can be used for the description
  verification and debugging.

* :dfn:`w` means a generation of warning instead of error for
  non-critical errors.

* :dfn:`no-comb-vect` prevents the automaton generator from generating
  two data structures and comparing them for space efficiency.  Using
  a comb vector to represent transitions may be better, but it can be
  very expensive to construct.  This option is useful if the build
  process spends an unacceptably long time in genautomata.

* :dfn:`ndfa` makes nondeterministic finite state automata.  This affects
  the treatment of operator :samp:`|` in the regular expressions.  The
  usual treatment of the operator is to try the first alternative and,
  if the reservation is not possible, the second alternative.  The
  nondeterministic treatment means trying all alternatives, some of them
  may be rejected by reservations in the subsequent insns.

* :dfn:`collapse-ndfa` modifies the behavior of the generator when
  producing an automaton.  An additional state transition to collapse a
  nondeterministic NDFA state to a deterministic DFA
  state is generated.  It can be triggered by passing ``const0_rtx`` to
  state_transition.  In such an automaton, cycle advance transitions are
  available only for these collapsed states.  This option is useful for
  ports that want to use the ``ndfa`` option, but also want to use
  ``define_query_cpu_unit`` to assign units to insns issued in a cycle.

* :dfn:`progress` means output of a progress bar showing how many states
  were generated so far for automaton being processed.  This is useful
  during debugging a DFA description.  If you see too many
  generated states, you could interrupt the generator of the pipeline
  hazard recognizer and try to figure out a reason for generation of the
  huge automaton.

As an example, consider a superscalar RISC machine which can
issue three insns (two integer insns and one floating point insn) on
the cycle but can finish only two insns.  To describe this, we define
the following functional units.

.. code-block::

  (define_cpu_unit "i0_pipeline, i1_pipeline, f_pipeline")
  (define_cpu_unit "port0, port1")

All simple integer insns can be executed in any integer pipeline and
their result is ready in two cycles.  The simple integer insns are
issued into the first pipeline unless it is reserved, otherwise they
are issued into the second pipeline.  Integer division and
multiplication insns can be executed only in the second integer
pipeline and their results are ready correspondingly in 9 and 4
cycles.  The integer division is not pipelined, i.e. the subsequent
integer division insn cannot be issued until the current division
insn finished.  Floating point insns are fully pipelined and their
results are ready in 3 cycles.  Where the result of a floating point
insn is used by an integer insn, an additional delay of one cycle is
incurred.  To describe all of this we could specify

.. code-block::

  (define_cpu_unit "div")

  (define_insn_reservation "simple" 2 (eq_attr "type" "int")
                           "(i0_pipeline | i1_pipeline), (port0 | port1)")

  (define_insn_reservation "mult" 4 (eq_attr "type" "mult")
                           "i1_pipeline, nothing*2, (port0 | port1)")

  (define_insn_reservation "div" 9 (eq_attr "type" "div")
                           "i1_pipeline, div*7, div + (port0 | port1)")

  (define_insn_reservation "float" 3 (eq_attr "type" "float")
                           "f_pipeline, nothing, (port0 | port1))

  (define_bypass 4 "float" "simple,mult,div")

To simplify the description we could describe the following reservation

.. code-block::

  (define_reservation "finish" "port0|port1")

and use it in all ``define_insn_reservation`` as in the following
construction

.. code-block::

  (define_insn_reservation "simple" 2 (eq_attr "type" "int")
                           "(i0_pipeline | i1_pipeline), finish")
