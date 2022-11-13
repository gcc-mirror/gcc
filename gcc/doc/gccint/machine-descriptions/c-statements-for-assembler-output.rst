..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: output statements, C statements for assembler output, generating assembler output

.. _output-statement:

C Statements for Assembler Output
*********************************

Often a single fixed template string cannot produce correct and efficient
assembler code for all the cases that are recognized by a single
instruction pattern.  For example, the opcodes may depend on the kinds of
operands; or some unfortunate combinations of operands may require extra
machine instructions.

If the output control string starts with a :samp:`@`, then it is actually
a series of templates, each on a separate line.  (Blank lines and
leading spaces and tabs are ignored.)  The templates correspond to the
pattern's constraint alternatives (see :ref:`multi-alternative`).  For example,
if a target machine has a two-address add instruction :samp:`addr` to add
into a register and another :samp:`addm` to add a register to memory, you
might write this pattern:

.. code-block::

  (define_insn "addsi3"
    [(set (match_operand:SI 0 "general_operand" "=r,m")
          (plus:SI (match_operand:SI 1 "general_operand" "0,0")
                   (match_operand:SI 2 "general_operand" "g,r")))]
    ""
    "@
     addr %2,%0
     addm %2,%0")

.. index:: * in template, asterisk in template

If the output control string starts with a :samp:`*`, then it is not an
output template but rather a piece of C program that should compute a
template.  It should execute a ``return`` statement to return the
template-string you want.  Most such templates use C string literals, which
require doublequote characters to delimit them.  To include these
doublequote characters in the string, prefix each one with ``\``.

If the output control string is written as a brace block instead of a
double-quoted string, it is automatically assumed to be C code.  In that
case, it is not necessary to put in a leading asterisk, or to escape the
doublequotes surrounding C string literals.

The operands may be found in the array ``operands``, whose C data type
is ``rtx []``.

It is very common to select different ways of generating assembler code
based on whether an immediate operand is within a certain range.  Be
careful when doing this, because the result of ``INTVAL`` is an
integer on the host machine.  If the host machine has more bits in an
``int`` than the target machine has in the mode in which the constant
will be used, then some of the bits you get from ``INTVAL`` will be
superfluous.  For proper results, you must carefully disregard the
values of those bits.

.. index:: output_asm_insn

It is possible to output an assembler instruction and then go on to output
or compute more of them, using the subroutine ``output_asm_insn``.  This
receives two arguments: a template-string and a vector of operands.  The
vector may be ``operands``, or it may be another array of ``rtx``
that you declare locally and initialize yourself.

.. index:: which_alternative

When an insn pattern has multiple alternatives in its constraints, often
the appearance of the assembler code is determined mostly by which alternative
was matched.  When this is so, the C code can test the variable
``which_alternative``, which is the ordinal number of the alternative
that was actually satisfied (0 for the first, 1 for the second alternative,
etc.).

For example, suppose there are two opcodes for storing zero, :samp:`clrreg`
for registers and :samp:`clrmem` for memory locations.  Here is how
a pattern could use ``which_alternative`` to choose between them:

.. code-block::

  (define_insn ""
    [(set (match_operand:SI 0 "general_operand" "=r,m")
          (const_int 0))]
    ""
    {
    return (which_alternative == 0
            ? "clrreg %0" : "clrmem %0");
    })

The example above, where the assembler code to generate was
*solely* determined by the alternative, could also have been specified
as follows, having the output control string start with a :samp:`@`:

.. code-block::

  (define_insn ""
    [(set (match_operand:SI 0 "general_operand" "=r,m")
          (const_int 0))]
    ""
    "@
     clrreg %0
     clrmem %0")

If you just need a little bit of C code in one (or a few) alternatives,
you can use :samp:`*` inside of a :samp:`@` multi-alternative template:

.. code-block::

  (define_insn ""
    [(set (match_operand:SI 0 "general_operand" "=r,<,m")
          (const_int 0))]
    ""
    "@
     clrreg %0
     * return stack_mem_p (operands[0]) ? \"push 0\" : \"clrmem %0\";
     clrmem %0")