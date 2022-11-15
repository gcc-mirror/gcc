..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: iterators in .md files

.. _iterators:

Iterators
*********

Ports often need to define similar patterns for more than one machine
mode or for more than one rtx code.  GCC provides some simple iterator
facilities to make this process easier.

.. toctree::
  :maxdepth: 2


.. index:: mode iterators in .md files

.. _mode-iterators:

Mode Iterators
^^^^^^^^^^^^^^

Ports often need to define similar patterns for two or more different modes.
For example:

* If a processor has hardware support for both single and double
  floating-point arithmetic, the ``SFmode`` patterns tend to be
  very similar to the ``DFmode`` ones.

* If a port uses ``SImode`` pointers in one configuration and
  ``DImode`` pointers in another, it will usually have very similar
  ``SImode`` and ``DImode`` patterns for manipulating pointers.

Mode iterators allow several patterns to be instantiated from one
:samp:`.md` file template.  They can be used with any type of
rtx-based construct, such as a ``define_insn``,
``define_split``, or ``define_peephole2``.

.. toctree::
  :maxdepth: 2


.. index:: define_mode_iterator

.. _defining-mode-iterators:

Defining Mode Iterators
~~~~~~~~~~~~~~~~~~~~~~~

The syntax for defining a mode iterator is:

.. code-block::

  (define_mode_iterator name [(mode1 "cond1") ... (moden "condn")])

This allows subsequent :samp:`.md` file constructs to use the mode suffix
``:name``.  Every construct that does so will be expanded
:samp:`{n}` times, once with every use of ``:name`` replaced by
``:mode1``, once with every use replaced by ``:mode2``,
and so on.  In the expansion for a particular :samp:`{modei}`, every
C condition will also require that :samp:`{condi}` be true.

For example:

.. code-block::

  (define_mode_iterator P [(SI "Pmode == SImode") (DI "Pmode == DImode")])

defines a new mode suffix ``:P``.  Every construct that uses
``:P`` will be expanded twice, once with every ``:P`` replaced
by ``:SI`` and once with every ``:P`` replaced by ``:DI``.
The ``:SI`` version will only apply if ``Pmode == SImode`` and
the ``:DI`` version will only apply if ``Pmode == DImode``.

As with other :samp:`.md` conditions, an empty string is treated
as 'always true'.  ``(mode "")`` can also be abbreviated
to ``mode``.  For example:

.. code-block::

  (define_mode_iterator GPR [SI (DI "TARGET_64BIT")])

means that the ``:DI`` expansion only applies if ``TARGET_64BIT``
but that the ``:SI`` expansion has no such constraint.

Iterators are applied in the order they are defined.  This can be
significant if two iterators are used in a construct that requires
substitutions.  See :ref:`substitutions`.

.. index:: define_mode_attr

.. _substitutions:

Substitution in Mode Iterators
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If an :samp:`.md` file construct uses mode iterators, each version of the
construct will often need slightly different strings or modes.  For
example:

* When a ``define_expand`` defines several ``addm3`` patterns
  (see :ref:`standard-names`), each expander will need to use the
  appropriate mode name for :samp:`{m}`.

* When a ``define_insn`` defines several instruction patterns,
  each instruction will often use a different assembler mnemonic.

* When a ``define_insn`` requires operands with different modes,
  using an iterator for one of the operand modes usually requires a specific
  mode for the other operand(s).

GCC supports such variations through a system of 'mode attributes'.
There are two standard attributes: ``mode``, which is the name of
the mode in lower case, and ``MODE``, which is the same thing in
upper case.  You can define other attributes using:

.. code-block::

  (define_mode_attr name [(mode1 "value1") ... (moden "valuen")])

where :samp:`{name}` is the name of the attribute and :samp:`{valuei}`
is the value associated with :samp:`{modei}`.

When GCC replaces some :samp:`{:iterator}` with :samp:`{:mode}`, it will scan
each string and mode in the pattern for sequences of the form
``<iterator:attr>``, where :samp:`{attr}` is the name of a
mode attribute.  If the attribute is defined for :samp:`{mode}`, the whole
``<...>`` sequence will be replaced by the appropriate attribute
value.

For example, suppose an :samp:`.md` file has:

.. code-block::

  (define_mode_iterator P [(SI "Pmode == SImode") (DI "Pmode == DImode")])
  (define_mode_attr load [(SI "lw") (DI "ld")])

If one of the patterns that uses ``:P`` contains the string
``"<P:load>\t%0,%1"``, the ``SI`` version of that pattern
will use ``"lw\t%0,%1"`` and the ``DI`` version will use
``"ld\t%0,%1"``.

Here is an example of using an attribute for a mode:

.. code-block::

  (define_mode_iterator LONG [SI DI])
  (define_mode_attr SHORT [(SI "HI") (DI "SI")])
  (define_insn ...
    (sign_extend:LONG (match_operand:<LONG:SHORT> ...)) ...)

The ``iterator:`` prefix may be omitted, in which case the
substitution will be attempted for every iterator expansion.

.. _examples:

Mode Iterator Examples
~~~~~~~~~~~~~~~~~~~~~~

Here is an example from the MIPS port.  It defines the following
modes and attributes (among others):

.. code-block::

  (define_mode_iterator GPR [SI (DI "TARGET_64BIT")])
  (define_mode_attr d [(SI "") (DI "d")])

and uses the following template to define both ``subsi3``
and ``subdi3`` :

.. code-block::

  (define_insn "sub<mode>3"
    [(set (match_operand:GPR 0 "register_operand" "=d")
          (minus:GPR (match_operand:GPR 1 "register_operand" "d")
                     (match_operand:GPR 2 "register_operand" "d")))]
    ""
    "<d>subu\t%0,%1,%2"
    [(set_attr "type" "arith")
     (set_attr "mode" "<MODE>")])

This is exactly equivalent to:

.. code-block::

  (define_insn "subsi3"
    [(set (match_operand:SI 0 "register_operand" "=d")
          (minus:SI (match_operand:SI 1 "register_operand" "d")
                    (match_operand:SI 2 "register_operand" "d")))]
    ""
    "subu\t%0,%1,%2"
    [(set_attr "type" "arith")
     (set_attr "mode" "SI")])

  (define_insn "subdi3"
    [(set (match_operand:DI 0 "register_operand" "=d")
          (minus:DI (match_operand:DI 1 "register_operand" "d")
                    (match_operand:DI 2 "register_operand" "d")))]
    "TARGET_64BIT"
    "dsubu\t%0,%1,%2"
    [(set_attr "type" "arith")
     (set_attr "mode" "DI")])

.. index:: code iterators in .md files, define_code_iterator, define_code_attr

.. _code-iterators:

Code Iterators
^^^^^^^^^^^^^^

Code iterators operate in a similar way to mode iterators.  See :ref:`mode-iterators`.

The construct:

.. code-block::

  (define_code_iterator name [(code1 "cond1") ... (coden "condn")])

defines a pseudo rtx code :samp:`{name}` that can be instantiated as
:samp:`{codei}` if condition :samp:`{condi}` is true.  Each :samp:`{codei}`
must have the same rtx format.  See :ref:`rtl-classes`.

As with mode iterators, each pattern that uses :samp:`{name}` will be
expanded :samp:`{n}` times, once with all uses of :samp:`{name}` replaced by
:samp:`{code1}`, once with all uses replaced by :samp:`{code2}`, and so on.
See :ref:`defining-mode-iterators`.

It is possible to define attributes for codes as well as for modes.
There are two standard code attributes: ``code``, the name of the
code in lower case, and ``CODE``, the name of the code in upper case.
Other attributes are defined using:

.. code-block::

  (define_code_attr name [(code1 "value1") ... (coden "valuen")])

Instruction patterns can use code attributes as rtx codes, which can be
useful if two sets of codes act in tandem.  For example, the following
``define_insn`` defines two patterns, one calculating a signed absolute
difference and another calculating an unsigned absolute difference:

.. code-block::

  (define_code_iterator any_max [smax umax])
  (define_code_attr paired_min [(smax "smin") (umax "umin")])
  (define_insn ...
    [(set (match_operand:SI 0 ...)
          (minus:SI (any_max:SI (match_operand:SI 1 ...)
                                (match_operand:SI 2 ...))
                    (<paired_min>:SI (match_dup 1) (match_dup 2))))]
    ...)

The signed version of the instruction uses ``smax`` and ``smin``
while the unsigned version uses ``umax`` and ``umin``.  There
are no versions that pair ``smax`` with ``umin`` or ``umax``
with ``smin``.

Here's an example of code iterators in action, taken from the MIPS port:

.. code-block::

  (define_code_iterator any_cond [unordered ordered unlt unge uneq ltgt unle ungt
                                  eq ne gt ge lt le gtu geu ltu leu])

  (define_expand "b<code>"
    [(set (pc)
          (if_then_else (any_cond:CC (cc0)
                                     (const_int 0))
                        (label_ref (match_operand 0 ""))
                        (pc)))]
    ""
  {
    gen_conditional_branch (operands, <CODE>);
    DONE;
  })

This is equivalent to:

.. code-block::

  (define_expand "bunordered"
    [(set (pc)
          (if_then_else (unordered:CC (cc0)
                                      (const_int 0))
                        (label_ref (match_operand 0 ""))
                        (pc)))]
    ""
  {
    gen_conditional_branch (operands, UNORDERED);
    DONE;
  })

  (define_expand "bordered"
    [(set (pc)
          (if_then_else (ordered:CC (cc0)
                                    (const_int 0))
                        (label_ref (match_operand 0 ""))
                        (pc)))]
    ""
  {
    gen_conditional_branch (operands, ORDERED);
    DONE;
  })

  ...

.. index:: int iterators in .md files, define_int_iterator, define_int_attr

.. _int-iterators:

Int Iterators
^^^^^^^^^^^^^

Int iterators operate in a similar way to code iterators.  See :ref:`code-iterators`.

The construct:

.. code-block::

  (define_int_iterator name [(int1 "cond1") ... (intn "condn")])

defines a pseudo integer constant :samp:`{name}` that can be instantiated as
:samp:`{inti}` if condition :samp:`{condi}` is true.  Each :samp:`{int}` must have the
same rtx format.  See :ref:`rtl-classes`.  Int iterators can appear in only
those rtx fields that have 'i', 'n', 'w', or 'p' as the specifier.  This
means that each :samp:`{int}` has to be a constant defined using define_constant
or define_c_enum.

As with mode and code iterators, each pattern that uses :samp:`{name}` will be
expanded :samp:`{n}` times, once with all uses of :samp:`{name}` replaced by
:samp:`{int1}`, once with all uses replaced by :samp:`{int2}`, and so on.
See :ref:`defining-mode-iterators`.

It is possible to define attributes for ints as well as for codes and modes.
Attributes are defined using:

.. code-block::

  (define_int_attr name [(int1 "value1") ... (intn "valuen")])

Here's an example of int iterators in action, taken from the ARM port:

.. code-block::

  (define_int_iterator QABSNEG [UNSPEC_VQABS UNSPEC_VQNEG])

  (define_int_attr absneg [(UNSPEC_VQABS "abs") (UNSPEC_VQNEG "neg")])

  (define_insn "neon_vq<absneg><mode>"
    [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
  	(unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
  		       (match_operand:SI 2 "immediate_operand" "i")]
  		      QABSNEG))]
    "TARGET_NEON"
    "vq<absneg>.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
    [(set_attr "type" "neon_vqneg_vqabs")]
  )

This is equivalent to:

.. code-block::

  (define_insn "neon_vqabs<mode>"
    [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
  	(unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
  		       (match_operand:SI 2 "immediate_operand" "i")]
  		      UNSPEC_VQABS))]
    "TARGET_NEON"
    "vqabs.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
    [(set_attr "type" "neon_vqneg_vqabs")]
  )

  (define_insn "neon_vqneg<mode>"
    [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
  	(unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
  		       (match_operand:SI 2 "immediate_operand" "i")]
  		      UNSPEC_VQNEG))]
    "TARGET_NEON"
    "vqneg.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
    [(set_attr "type" "neon_vqneg_vqabs")]
  )

.. index:: subst iterators in .md files, define_subst, define_subst_attr

.. _subst-iterators:

Subst Iterators
^^^^^^^^^^^^^^^

Subst iterators are special type of iterators with the following
restrictions: they could not be declared explicitly, they always have
only two values, and they do not have explicit dedicated name.
Subst-iterators are triggered only when corresponding subst-attribute is
used in RTL-pattern.

Subst iterators transform templates in the following way: the templates
are duplicated, the subst-attributes in these templates are replaced
with the corresponding values, and a new attribute is implicitly added
to the given ``define_insn`` / ``define_expand``.  The name of the
added attribute matches the name of ``define_subst``.  Such
attributes are declared implicitly, and it is not allowed to have a
``define_attr`` named as a ``define_subst``.

Each subst iterator is linked to a ``define_subst``.  It is declared
implicitly by the first appearance of the corresponding
``define_subst_attr``, and it is not allowed to define it explicitly.

Declarations of subst-attributes have the following syntax:

.. index:: define_subst_attr

.. code-block::

  (define_subst_attr "name"
    "subst-name"
    "no-subst-value"
    "subst-applied-value")

:samp:`{name}` is a string with which the given subst-attribute could be
referred to.

:samp:`{subst-name}` shows which ``define_subst`` should be applied to an
RTL-template if the given subst-attribute is present in the
RTL-template.

:samp:`{no-subst-value}` is a value with which subst-attribute would be
replaced in the first copy of the original RTL-template.

:samp:`{subst-applied-value}` is a value with which subst-attribute would be
replaced in the second copy of the original RTL-template.

.. index:: @ in instruction pattern names

.. _parameterized-names:

Parameterized Names
^^^^^^^^^^^^^^^^^^^

Ports sometimes need to apply iterators using C++ code, in order to
get the code or RTL pattern for a specific instruction.  For example,
suppose we have the :samp:`neon_vq<absneg><mode>` pattern given above:

.. code-block::

  (define_int_iterator QABSNEG [UNSPEC_VQABS UNSPEC_VQNEG])

  (define_int_attr absneg [(UNSPEC_VQABS "abs") (UNSPEC_VQNEG "neg")])

  (define_insn "neon_vq<absneg><mode>"
    [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
  	(unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
  		       (match_operand:SI 2 "immediate_operand" "i")]
  		      QABSNEG))]
    ...
  )

A port might need to generate this pattern for a variable
:samp:`QABSNEG` value and a variable :samp:`VDQIW` mode.  There are two
ways of doing this.  The first is to build the rtx for the pattern
directly from C++ code; this is a valid technique and avoids any risk
of combinatorial explosion.  The second is to prefix the instruction
name with the special character :samp:`@`, which tells GCC to generate
the four additional functions below.  In each case, :samp:`{name}` is the
name of the instruction without the leading :samp:`@` character,
without the :samp:`<...>` placeholders, and with any underscore
before a :samp:`<...>` placeholder removed if keeping it would
lead to a double or trailing underscore.

:samp:`insn_code maybe_code_for_{name} ({i1}, {i2}, ...)`
  See whether replacing the first :samp:`<...>` placeholder with
  iterator value :samp:`{i1}`, the second with iterator value :samp:`{i2}`, and
  so on, gives a valid instruction.  Return its code if so, otherwise
  return ``CODE_FOR_nothing``.

:samp:`insn_code code_for_{name} ({i1}, {i2}, ...)`
  Same, but abort the compiler if the requested instruction does not exist.

:samp:`rtx maybe_gen_{name} ({i1}, {i2}, ..., {op0}, {op1}, ...)`
  Check for a valid instruction in the same way as
  ``maybe_code_for_name``.  If the instruction exists,
  generate an instance of it using the operand values given by :samp:`{op0}`,
  :samp:`{op1}`, and so on, otherwise return null.

:samp:`rtx gen_{name} ({i1}, {i2}, ..., {op0}, {op1}, ...)`
  Same, but abort the compiler if the requested instruction does not exist,
  or if the instruction generator invoked the ``FAIL`` macro.

  For example, changing the pattern above to:

.. code-block::

  (define_insn "@neon_vq<absneg><mode>"
    [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
  	(unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
  		       (match_operand:SI 2 "immediate_operand" "i")]
  		      QABSNEG))]
    ...
  )

would define the same patterns as before, but in addition would generate
the four functions below:

.. code-block:: c++

  insn_code maybe_code_for_neon_vq (int, machine_mode);
  insn_code code_for_neon_vq (int, machine_mode);
  rtx maybe_gen_neon_vq (int, machine_mode, rtx, rtx, rtx);
  rtx gen_neon_vq (int, machine_mode, rtx, rtx, rtx);

Calling :samp:`code_for_neon_vq (UNSPEC_VQABS, V8QImode)`
would then give ``CODE_FOR_neon_vqabsv8qi``.

It is possible to have multiple :samp:`@` patterns with the same
name and same types of iterator.  For example:

.. code-block::

  (define_insn "@some_arithmetic_op<mode>"
    [(set (match_operand:INTEGER_MODES 0 "register_operand") ...)]
    ...
  )

  (define_insn "@some_arithmetic_op<mode>"
    [(set (match_operand:FLOAT_MODES 0 "register_operand") ...)]
    ...
  )

would produce a single set of functions that handles both
``INTEGER_MODES`` and ``FLOAT_MODES``.

It is also possible for these :samp:`@` patterns to have different
numbers of operands from each other.  For example, patterns with
a binary rtl code might take three operands (one output and two inputs)
while patterns with a ternary rtl code might take four operands (one
output and three inputs).  This combination would produce separate
:samp:`maybe_gen_{name}` and :samp:`gen_{name}` functions for
each operand count, but it would still produce a single
:samp:`maybe_code_for_{name}` and a single :samp:`code_for_{name}`.
