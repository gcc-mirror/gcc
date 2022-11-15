..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _overview:

Overview of How the Machine Description is Used
***********************************************

There are three main conversions that happen in the compiler:

* The front end reads the source code and builds a parse tree.

* The parse tree is used to generate an RTL insn list based on named
  instruction patterns.

* The insn list is matched against the RTL templates to produce assembler
  code.

For the generate pass, only the names of the insns matter, from either a
named ``define_insn`` or a ``define_expand``.  The compiler will
choose the pattern with the right name and apply the operands according
to the documentation later in this chapter, without regard for the RTL
template or operand constraints.  Note that the names the compiler looks
for are hard-coded in the compiler---it will ignore unnamed patterns and
patterns with names it doesn't know about, but if you don't provide a
named pattern it needs, it will abort.

If a ``define_insn`` is used, the template given is inserted into the
insn list.  If a ``define_expand`` is used, one of three things
happens, based on the condition logic.  The condition logic may manually
create new insns for the insn list, say via ``emit_insn()``, and
invoke ``DONE``.  For certain named patterns, it may invoke ``FAIL`` to tell the
compiler to use an alternate way of performing that task.  If it invokes
neither ``DONE`` nor ``FAIL``, the template given in the pattern
is inserted, as if the ``define_expand`` were a ``define_insn``.

Once the insn list is generated, various optimization passes convert,
replace, and rearrange the insns in the insn list.  This is where the
``define_split`` and ``define_peephole`` patterns get used, for
example.

Finally, the insn list's RTL is matched up with the RTL templates in the
``define_insn`` patterns, and those patterns are used to emit the
final assembly code.  For this purpose, each named ``define_insn``
acts like it's unnamed, since the names are ignored.
