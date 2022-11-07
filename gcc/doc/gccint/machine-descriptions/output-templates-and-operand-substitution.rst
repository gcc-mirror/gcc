..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: output templates, operand substitution, % in template, percent sign

.. _output-template:

Output Templates and Operand Substitution
*****************************************

The :dfn:`output template` is a string which specifies how to output the
assembler code for an instruction pattern.  Most of the template is a
fixed string which is output literally.  The character :samp:`%` is used
to specify where to substitute an operand; it can also be used to
identify places where different variants of the assembler require
different syntax.

In the simplest case, a :samp:`%` followed by a digit :samp:`{n}` says to output
operand :samp:`{n}` at that point in the string.

:samp:`%` followed by a letter and a digit says to output an operand in an
alternate fashion.  Four letters have standard, built-in meanings described
below.  The machine description macro ``PRINT_OPERAND`` can define
additional letters with nonstandard meanings.

:samp:`%c{digit}` can be used to substitute an operand that is a
constant value without the syntax that normally indicates an immediate
operand.

:samp:`%n{digit}` is like :samp:`%c{digit}` except that the value of
the constant is negated before printing.

:samp:`%a{digit}` can be used to substitute an operand as if it were a
memory reference, with the actual operand treated as the address.  This may
be useful when outputting a 'load address' instruction, because often the
assembler syntax for such an instruction requires you to write the operand
as if it were a memory reference.

:samp:`%l{digit}` is used to substitute a ``label_ref`` into a jump
instruction.

:samp:`%=` outputs a number which is unique to each instruction in the
entire compilation.  This is useful for making local labels to be
referred to more than once in a single template that generates multiple
assembler instructions.

:samp:`%` followed by a punctuation character specifies a substitution that
does not use an operand.  Only one case is standard: :samp:`%%` outputs a
:samp:`%` into the assembler code.  Other nonstandard cases can be
defined in the ``PRINT_OPERAND`` macro.  You must also define
which punctuation characters are valid with the
``PRINT_OPERAND_PUNCT_VALID_P`` macro.

.. index:: \, backslash

The template may generate multiple assembler instructions.  Write the text
for the instructions, with :samp:`\\;` between them.

.. index:: matching operands

When the RTL contains two operands which are required by constraint to match
each other, the output template must refer only to the lower-numbered operand.
Matching operands are not always identical, and the rest of the compiler
arranges to put the proper RTL expression for printing into the lower-numbered
operand.

One use of nonstandard letters or punctuation following :samp:`%` is to
distinguish between different assembler languages for the same machine; for
example, Motorola syntax versus MIT syntax for the 68000.  Motorola syntax
requires periods in most opcode names, while MIT syntax does not.  For
example, the opcode :samp:`movel` in MIT syntax is :samp:`move.l` in Motorola
syntax.  The same file of patterns is used for both kinds of output syntax,
but the character sequence :samp:`%.` is used in each place where Motorola
syntax wants a period.  The ``PRINT_OPERAND`` macro for Motorola syntax
defines the sequence to output a period; the macro for MIT syntax defines
it to do nothing.

.. index:: # in template

As a special case, a template consisting of the single character ``#``
instructs the compiler to first split the insn, and then output the
resulting instructions separately.  This helps eliminate redundancy in the
output templates.   If you have a ``define_insn`` that needs to emit
multiple assembler instructions, and there is a matching ``define_split``
already defined, then you can simply use ``#`` as the output template
instead of writing an output template that emits the multiple assembler
instructions.

Note that ``#`` only has an effect while generating assembly code;
it does not affect whether a split occurs earlier.  An associated
``define_split`` must exist and it must be suitable for use after
register allocation.

If the macro ``ASSEMBLER_DIALECT`` is defined, you can use construct
of the form :samp:`{option0|option1|option2}` in the templates.  These
describe multiple variants of assembler language syntax.
See :ref:`instruction-output`.