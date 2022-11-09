..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: asm keyword, assembly language in C, inline assembly language, mixing assembly language and C

.. _using-assembly-language-with-c:

How to Use Inline Assembly Language in C Code
*********************************************

The ``asm`` keyword allows you to embed assembler instructions
within C code.  GCC provides two forms of inline ``asm``
statements.  A basic ``asm`` statement is one with no
operands (see :ref:`basic-asm`), while an extended ``asm``
statement (see :ref:`extended-asm`) includes one or more operands.
The extended form is preferred for mixing C and assembly language
within a function, but to include assembly language at
top level you must use basic ``asm``.

You can also use the ``asm`` keyword to override the assembler name
for a C symbol, or to place a C variable in a specific register.

.. toctree::
  :maxdepth: 2


.. index:: basic asm, assembly language in C, basic

.. _basic-asm:

Basic Asm --- Assembler Instructions Without Operands
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A basic ``asm`` statement has the following syntax:

.. code-block::

  asm asm-qualifiers ( AssemblerInstructions )

For the C language, the ``asm`` keyword is a GNU extension.
When writing C code that can be compiled with :option:`-ansi` and the
:option:`-std` options that select C dialects without GNU extensions, use
``__asm__`` instead of ``asm`` (see :ref:`alternate-keywords`).  For
the C++ language, ``asm`` is a standard keyword, but ``__asm__``
can be used for code compiled with :option:`-fno-asm`.

Qualifiers
^^^^^^^^^^

``volatile``
  The optional ``volatile`` qualifier has no effect.
  All basic ``asm`` blocks are implicitly volatile.

``inline``
  If you use the ``inline`` qualifier, then for inlining purposes the size
  of the ``asm`` statement is taken as the smallest size possible (see :ref:`size-of-an-asm`).

Parameters
^^^^^^^^^^

:samp:`{AssemblerInstructions}`
  This is a literal string that specifies the assembler code. The string can
  contain any instructions recognized by the assembler, including directives.
  GCC does not parse the assembler instructions themselves and
  does not know what they mean or even whether they are valid assembler input.

  You may place multiple assembler instructions together in a single ``asm``
  string, separated by the characters normally used in assembly code for the
  system. A combination that works in most places is a newline to break the
  line, plus a tab character (written as :samp:`\\n\\t`).
  Some assemblers allow semicolons as a line separator. However,
  note that some assembler dialects use semicolons to start a comment.

Remarks
^^^^^^^

Using extended ``asm`` (see :ref:`extended-asm`) typically produces
smaller, safer, and more efficient code, and in most cases it is a
better solution than basic ``asm``.  However, there are two
situations where only basic ``asm`` can be used:

* Extended ``asm`` statements have to be inside a C
  function, so to write inline assembly language at file scope ('top-level'),
  outside of C functions, you must use basic ``asm``.
  You can use this technique to emit assembler directives,
  define assembly language macros that can be invoked elsewhere in the file,
  or write entire functions in assembly language.
  Basic ``asm`` statements outside of functions may not use any
  qualifiers.

* Functions declared
  with the :fn-attr:`naked` attribute also require basic ``asm``
  (see :ref:`function-attributes`).

Safely accessing C data and calling functions from basic ``asm`` is more
complex than it may appear. To access C data, it is better to use extended
``asm``.

Do not expect a sequence of ``asm`` statements to remain perfectly
consecutive after compilation. If certain instructions need to remain
consecutive in the output, put them in a single multi-instruction ``asm``
statement. Note that GCC's optimizers can move ``asm`` statements
relative to other code, including across jumps.

``asm`` statements may not perform jumps into other ``asm`` statements.
GCC does not know about these jumps, and therefore cannot take
account of them when deciding how to optimize. Jumps from ``asm`` to C
labels are only supported in extended ``asm``.

Under certain circumstances, GCC may duplicate (or remove duplicates of) your
assembly code when optimizing. This can lead to unexpected duplicate
symbol errors during compilation if your assembly code defines symbols or
labels.

.. warning::

  The C standards do not specify semantics for ``asm``,
  making it a potential source of incompatibilities between compilers.  These
  incompatibilities may not produce compiler warnings/errors.

GCC does not parse basic ``asm`` 's :samp:`{AssemblerInstructions}`, which
means there is no way to communicate to the compiler what is happening
inside them.  GCC has no visibility of symbols in the ``asm`` and may
discard them as unreferenced.  It also does not know about side effects of
the assembler code, such as modifications to memory or registers.  Unlike
some compilers, GCC assumes that no changes to general purpose registers
occur.  This assumption may change in a future release.

To avoid complications from future changes to the semantics and the
compatibility issues between compilers, consider replacing basic ``asm``
with extended ``asm``.  See
`How to convert
from basic asm to extended asm <https://gcc.gnu.org/wiki/ConvertBasicAsmToExtended>`_ for information about how to perform this
conversion.

The compiler copies the assembler instructions in a basic ``asm``
verbatim to the assembly language output file, without
processing dialects or any of the :samp:`%` operators that are available with
extended ``asm``. This results in minor differences between basic
``asm`` strings and extended ``asm`` templates. For example, to refer to
registers you might use :samp:`%eax` in basic ``asm`` and
:samp:`%%eax` in extended ``asm``.

On targets such as x86 that support multiple assembler dialects,
all basic ``asm`` blocks use the assembler dialect specified by the
:option:`-masm` command-line option (see :ref:`x86-options`).
Basic ``asm`` provides no
mechanism to provide different assembler strings for different dialects.

For basic ``asm`` with non-empty assembler string GCC assumes
the assembler block does not change any general purpose registers,
but it may read or write any globally accessible variable.

Here is an example of basic ``asm`` for i386:

.. code-block:: c++

  /* Note that this code will not compile with -masm=intel */
  #define DebugBreak() asm("int $3")

.. index:: extended asm, assembly language in C, extended

.. _extended-asm:

Extended Asm - Assembler Instructions with C Expression Operands
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

With extended ``asm`` you can read and write C variables from
assembler and perform jumps from assembler code to C labels.
Extended ``asm`` syntax uses colons (:samp:`:`) to delimit
the operand parameters after the assembler template:

.. code-block::

  asm asm-qualifiers ( AssemblerTemplate
                   : OutputOperands
                   [ : InputOperands
                   [ : Clobbers ] ])

  asm asm-qualifiers ( AssemblerTemplate
                        : OutputOperands
                        : InputOperands
                        : Clobbers
                        : GotoLabels)

where in the last form, :samp:`{asm-qualifiers}` contains ``goto`` (and in the
first form, not).

The ``asm`` keyword is a GNU extension.
When writing code that can be compiled with :option:`-ansi` and the
various :option:`-std` options, use ``__asm__`` instead of
``asm`` (see :ref:`alternate-keywords`).

Qualifiers
^^^^^^^^^^

``volatile``
  The typical use of extended ``asm`` statements is to manipulate input
  values to produce output values. However, your ``asm`` statements may
  also produce side effects. If so, you may need to use the ``volatile``
  qualifier to disable certain optimizations. See :ref:`volatile`.

``inline``
  If you use the ``inline`` qualifier, then for inlining purposes the size
  of the ``asm`` statement is taken as the smallest size possible
  (see :ref:`size-of-an-asm`).

``goto``
  This qualifier informs the compiler that the ``asm`` statement may
  perform a jump to one of the labels listed in the :samp:`{GotoLabels}`.
  See :ref:`gotolabels`.

Parameters
^^^^^^^^^^

:samp:`{AssemblerTemplate}`
  This is a literal string that is the template for the assembler code. It is a
  combination of fixed text and tokens that refer to the input, output,
  and goto parameters. See :ref:`assemblertemplate`.

:samp:`{OutputOperands}`
  A comma-separated list of the C variables modified by the instructions in the
  :samp:`{AssemblerTemplate}`.  An empty list is permitted.  See :ref:`outputoperands`.

:samp:`{InputOperands}`
  A comma-separated list of C expressions read by the instructions in the
  :samp:`{AssemblerTemplate}`.  An empty list is permitted.  See :ref:`inputoperands`.

:samp:`{Clobbers}`
  A comma-separated list of registers or other values changed by the
  :samp:`{AssemblerTemplate}`, beyond those listed as outputs.
  An empty list is permitted.  See :ref:`clobbers-and-scratch-registers`.

:samp:`{GotoLabels}`
  When you are using the ``goto`` form of ``asm``, this section contains
  the list of all C labels to which the code in the
  :samp:`{AssemblerTemplate}` may jump.
  See :ref:`gotolabels`.

  ``asm`` statements may not perform jumps into other ``asm`` statements,
  only to the listed :samp:`{GotoLabels}`.
  GCC's optimizers do not know about other jumps; therefore they cannot take
  account of them when deciding how to optimize.

  The total number of input + output + goto operands is limited to 30.

Remarks
^^^^^^^

The ``asm`` statement allows you to include assembly instructions directly
within C code. This may help you to maximize performance in time-sensitive
code or to access assembly instructions that are not readily available to C
programs.

Note that extended ``asm`` statements must be inside a function. Only
basic ``asm`` may be outside functions (see :ref:`basic-asm`).
Functions declared with the :fn-attr:`naked` attribute also require basic
``asm`` (see :ref:`function-attributes`).

While the uses of ``asm`` are many and varied, it may help to think of an
``asm`` statement as a series of low-level instructions that convert input
parameters to output parameters. So a simple (if not particularly useful)
example for i386 using ``asm`` might look like this:

.. code-block:: c++

  int src = 1;
  int dst;

  asm ("mov %1, %0\n\t"
      "add $1, %0"
      : "=r" (dst)
      : "r" (src));

  printf("%d\n", dst);

This code copies ``src`` to ``dst`` and add 1 to ``dst``.

.. index:: volatile asm, asm volatile

.. _volatile:

Volatile
~~~~~~~~

GCC's optimizers sometimes discard ``asm`` statements if they determine
there is no need for the output variables. Also, the optimizers may move
code out of loops if they believe that the code will always return the same
result (i.e. none of its input values change between calls). Using the
``volatile`` qualifier disables these optimizations. ``asm`` statements
that have no output operands and ``asm goto`` statements,
are implicitly volatile.

This i386 code demonstrates a case that does not use (or require) the
``volatile`` qualifier. If it is performing assertion checking, this code
uses ``asm`` to perform the validation. Otherwise, ``dwRes`` is
unreferenced by any code. As a result, the optimizers can discard the
``asm`` statement, which in turn removes the need for the entire
``DoCheck`` routine. By omitting the ``volatile`` qualifier when it
isn't needed you allow the optimizers to produce the most efficient code
possible.

.. code-block:: c++

  void DoCheck(uint32_t dwSomeValue)
  {
     uint32_t dwRes;

     // Assumes dwSomeValue is not zero.
     asm ("bsfl %1,%0"
       : "=r" (dwRes)
       : "r" (dwSomeValue)
       : "cc");

     assert(dwRes > 3);
  }

The next example shows a case where the optimizers can recognize that the input
(``dwSomeValue``) never changes during the execution of the function and can
therefore move the ``asm`` outside the loop to produce more efficient code.
Again, using the ``volatile`` qualifier disables this type of optimization.

.. code-block:: c++

  void do_print(uint32_t dwSomeValue)
  {
     uint32_t dwRes;

     for (uint32_t x=0; x < 5; x++)
     {
        // Assumes dwSomeValue is not zero.
        asm ("bsfl %1,%0"
          : "=r" (dwRes)
          : "r" (dwSomeValue)
          : "cc");

        printf("%u: %u %u\n", x, dwSomeValue, dwRes);
     }
  }

The following example demonstrates a case where you need to use the
``volatile`` qualifier.
It uses the x86 ``rdtsc`` instruction, which reads
the computer's time-stamp counter. Without the ``volatile`` qualifier,
the optimizers might assume that the ``asm`` block will always return the
same value and therefore optimize away the second call.

.. code-block:: c++

  uint64_t msr;

  asm volatile ( "rdtsc\n\t"    // Returns the time in EDX:EAX.
          "shl $32, %%rdx\n\t"  // Shift the upper bits left.
          "or %%rdx, %0"        // 'Or' in the lower bits.
          : "=a" (msr)
          :
          : "rdx");

  printf("msr: %llx\n", msr);

  // Do other work...

  // Reprint the timestamp
  asm volatile ( "rdtsc\n\t"    // Returns the time in EDX:EAX.
          "shl $32, %%rdx\n\t"  // Shift the upper bits left.
          "or %%rdx, %0"        // 'Or' in the lower bits.
          : "=a" (msr)
          :
          : "rdx");

  printf("msr: %llx\n", msr);

GCC's optimizers do not treat this code like the non-volatile code in the
earlier examples. They do not move it out of loops or omit it on the
assumption that the result from a previous call is still valid.

Note that the compiler can move even ``volatile asm`` instructions relative
to other code, including across jump instructions. For example, on many
targets there is a system register that controls the rounding mode of
floating-point operations. Setting it with a ``volatile asm`` statement,
as in the following PowerPC example, does not work reliably.

.. code-block:: c++

  asm volatile("mtfsf 255, %0" : : "f" (fpenv));
  sum = x + y;

The compiler may move the addition back before the ``volatile asm``
statement. To make it work as expected, add an artificial dependency to
the ``asm`` by referencing a variable in the subsequent code, for
example:

.. code-block:: c++

  asm volatile ("mtfsf 255,%1" : "=X" (sum) : "f" (fpenv));
  sum = x + y;

Under certain circumstances, GCC may duplicate (or remove duplicates of) your
assembly code when optimizing. This can lead to unexpected duplicate symbol
errors during compilation if your ``asm`` code defines symbols or labels.
Using :samp:`%=`
(see :ref:`assemblertemplate`) may help resolve this problem.

.. index:: asm assembler template

.. _assemblertemplate:

Assembler Template
~~~~~~~~~~~~~~~~~~

An assembler template is a literal string containing assembler instructions.
The compiler replaces tokens in the template that refer
to inputs, outputs, and goto labels,
and then outputs the resulting string to the assembler. The
string can contain any instructions recognized by the assembler, including
directives. GCC does not parse the assembler instructions
themselves and does not know what they mean or even whether they are valid
assembler input. However, it does count the statements
(see :ref:`size-of-an-asm`).

You may place multiple assembler instructions together in a single ``asm``
string, separated by the characters normally used in assembly code for the
system. A combination that works in most places is a newline to break the
line, plus a tab character to move to the instruction field (written as
:samp:`\\n\\t`).
Some assemblers allow semicolons as a line separator. However, note
that some assembler dialects use semicolons to start a comment.

Do not expect a sequence of ``asm`` statements to remain perfectly
consecutive after compilation, even when you are using the ``volatile``
qualifier. If certain instructions need to remain consecutive in the output,
put them in a single multi-instruction ``asm`` statement.

Accessing data from C programs without using input/output operands (such as
by using global symbols directly from the assembler template) may not work as
expected. Similarly, calling functions directly from an assembler template
requires a detailed understanding of the target assembler and ABI.

Since GCC does not parse the assembler template,
it has no visibility of any
symbols it references. This may result in GCC discarding those symbols as
unreferenced unless they are also listed as input, output, or goto operands.

Special format strings
^^^^^^^^^^^^^^^^^^^^^^

In addition to the tokens described by the input, output, and goto operands,
these tokens have special meanings in the assembler template:

:samp:`%%`
  Outputs a single :samp:`%` into the assembler code.

:samp:`%=`
  Outputs a number that is unique to each instance of the ``asm``
  statement in the entire compilation. This option is useful when creating local
  labels and referring to them multiple times in a single template that
  generates multiple assembler instructions.

:samp:`%{` :samp:`%|` :samp:`%}`
  Outputs :samp:`{`, :samp:`|`, and :samp:`}` characters (respectively)
  into the assembler code.  When unescaped, these characters have special
  meaning to indicate multiple assembler dialects, as described below.

Multiple assembler dialects in asm templates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

On targets such as x86, GCC supports multiple assembler dialects.
The :option:`-masm` option controls which dialect GCC uses as its
default for inline assembler. The target-specific documentation for the
:option:`-masm` option contains the list of supported dialects, as well as the
default dialect if the option is not specified. This information may be
important to understand, since assembler code that works correctly when
compiled using one dialect will likely fail if compiled using another.
See :ref:`x86-options`.

If your code needs to support multiple assembler dialects (for example, if
you are writing public headers that need to support a variety of compilation
options), use constructs of this form:

.. code-block:: c++

  { dialect0 | dialect1 | dialect2... }

This construct outputs ``dialect0``
when using dialect #0 to compile the code,
``dialect1`` for dialect #1, etc. If there are fewer alternatives within the
braces than the number of dialects the compiler supports, the construct
outputs nothing.

For example, if an x86 compiler supports two dialects
(:samp:`att`, :samp:`intel`), an
assembler template such as this:

.. code-block:: c++

  "bt{l %[Offset],%[Base] | %[Base],%[Offset]}; jc %l2"

is equivalent to one of

.. code-block:: c++

  "btl %[Offset],%[Base] ; jc %l2"   /* att dialect */
  "bt %[Base],%[Offset]; jc %l2"     /* intel dialect */

Using that same compiler, this code:

.. code-block:: c++

  "xchg{l}\t{%%}ebx, %1"

corresponds to either

.. code-block:: c++

  "xchgl\t%%ebx, %1"                 /* att dialect */
  "xchg\tebx, %1"                    /* intel dialect */

There is no support for nesting dialect alternatives.

.. index:: asm output operands

.. _outputoperands:

Output Operands
~~~~~~~~~~~~~~~

An ``asm`` statement has zero or more output operands indicating the names
of C variables modified by the assembler code.

In this i386 example, ``old`` (referred to in the template string as
``%0``) and ``*Base`` (as ``%1``) are outputs and ``Offset``
(``%2``) is an input:

.. code-block:: c++

  bool old;

  __asm__ ("btsl %2,%1\n\t" // Turn on zero-based bit #Offset in Base.
           "sbb %0,%0"      // Use the CF to calculate old.
     : "=r" (old), "+rm" (*Base)
     : "Ir" (Offset)
     : "cc");

  return old;

Operands are separated by commas.  Each operand has this format:

.. code-block:: c++

  [ [asmSymbolicName] ] constraint (cvariablename)

:samp:`{asmSymbolicName}`
  Specifies a symbolic name for the operand.
  Reference the name in the assembler template
  by enclosing it in square brackets
  (i.e. :samp:`%[Value]`). The scope of the name is the ``asm`` statement
  that contains the definition. Any valid C variable name is acceptable,
  including names already defined in the surrounding code. No two operands
  within the same ``asm`` statement can use the same symbolic name.

  When not using an :samp:`{asmSymbolicName}`, use the (zero-based) position
  of the operand
  in the list of operands in the assembler template. For example if there are
  three output operands, use :samp:`%0` in the template to refer to the first,
  :samp:`%1` for the second, and :samp:`%2` for the third.

:samp:`{constraint}`
  A string constant specifying constraints on the placement of the operand;
  See :ref:`constraints`, for details.

  Output constraints must begin with either :samp:`=` (a variable overwriting an
  existing value) or :samp:`+` (when reading and writing). When using
  :samp:`=`, do not assume the location contains the existing value
  on entry to the ``asm``, except
  when the operand is tied to an input; see :ref:`inputoperands`.

  After the prefix, there must be one or more additional constraints
  (see :ref:`constraints`) that describe where the value resides. Common
  constraints include :samp:`r` for register and :samp:`m` for memory.
  When you list more than one possible location (for example, ``"=rm"``),
  the compiler chooses the most efficient one based on the current context.
  If you list as many alternates as the ``asm`` statement allows, you permit
  the optimizers to produce the best possible code.
  If you must use a specific register, but your Machine Constraints do not
  provide sufficient control to select the specific register you want,
  local register variables may provide a solution (see :ref:`local-register-variables`).

:samp:`{cvariablename}`
  Specifies a C lvalue expression to hold the output, typically a variable name.
  The enclosing parentheses are a required part of the syntax.

When the compiler selects the registers to use to
represent the output operands, it does not use any of the clobbered registers
(see :ref:`clobbers-and-scratch-registers`).

Output operand expressions must be lvalues. The compiler cannot check whether
the operands have data types that are reasonable for the instruction being
executed. For output expressions that are not directly addressable (for
example a bit-field), the constraint must allow a register. In that case, GCC
uses the register as the output of the ``asm``, and then stores that
register into the output.

Operands using the :samp:`+` constraint modifier count as two operands
(that is, both as input and output) towards the total maximum of 30 operands
per ``asm`` statement.

Use the :samp:`&` constraint modifier (see :ref:`modifiers`) on all output
operands that must not overlap an input.  Otherwise,
GCC may allocate the output operand in the same register as an unrelated
input operand, on the assumption that the assembler code consumes its
inputs before producing outputs. This assumption may be false if the assembler
code actually consists of more than one instruction.

The same problem can occur if one output parameter (:samp:`{a}`) allows a register
constraint and another output parameter (:samp:`{b}`) allows a memory constraint.
The code generated by GCC to access the memory address in :samp:`{b}` can contain
registers which *might* be shared by :samp:`{a}`, and GCC considers those
registers to be inputs to the asm. As above, GCC assumes that such input
registers are consumed before any outputs are written. This assumption may
result in incorrect behavior if the ``asm`` statement writes to :samp:`{a}`
before using
:samp:`{b}`. Combining the :samp:`&` modifier with the register constraint on :samp:`{a}`
ensures that modifying :samp:`{a}` does not affect the address referenced by
:samp:`{b}`. Otherwise, the location of :samp:`{b}`
is undefined if :samp:`{a}` is modified before using :samp:`{b}`.

``asm`` supports operand modifiers on operands (for example :samp:`%k2`
instead of simply :samp:`%2`). Typically these qualifiers are hardware
dependent. The list of supported modifiers for x86 is found at :ref:`x86operandmodifiers`.

If the C code that follows the ``asm`` makes no use of any of the output
operands, use ``volatile`` for the ``asm`` statement to prevent the
optimizers from discarding the ``asm`` statement as unneeded
(see :ref:`volatile`).

This code makes no use of the optional :samp:`{asmSymbolicName}`. Therefore it
references the first output operand as ``%0`` (were there a second, it
would be ``%1``, etc). The number of the first input operand is one greater
than that of the last output operand. In this i386 example, that makes
``Mask`` referenced as ``%1`` :

.. code-block:: c++

  uint32_t Mask = 1234;
  uint32_t Index;

    asm ("bsfl %1, %0"
       : "=r" (Index)
       : "r" (Mask)
       : "cc");

That code overwrites the variable ``Index`` (:samp:`=`),
placing the value in a register (:samp:`r`).
Using the generic :samp:`r` constraint instead of a constraint for a specific
register allows the compiler to pick the register to use, which can result
in more efficient code. This may not be possible if an assembler instruction
requires a specific register.

The following i386 example uses the :samp:`{asmSymbolicName}` syntax.
It produces the
same result as the code above, but some may consider it more readable or more
maintainable since reordering index numbers is not necessary when adding or
removing operands. The names ``aIndex`` and ``aMask``
are only used in this example to emphasize which
names get used where.
It is acceptable to reuse the names ``Index`` and ``Mask``.

.. code-block:: c++

  uint32_t Mask = 1234;
  uint32_t Index;

    asm ("bsfl %[aMask], %[aIndex]"
       : [aIndex] "=r" (Index)
       : [aMask] "r" (Mask)
       : "cc");

Here are some more examples of output operands.

.. code-block:: c++

  uint32_t c = 1;
  uint32_t d;
  uint32_t *e = &c;

  asm ("mov %[e], %[d]"
     : [d] "=rm" (d)
     : [e] "rm" (*e));

Here, ``d`` may either be in a register or in memory. Since the compiler
might already have the current value of the ``uint32_t`` location
pointed to by ``e``
in a register, you can enable it to choose the best location
for ``d`` by specifying both constraints.

.. index:: asm flag output operands

.. _flagoutputoperands:

Flag Output Operands
~~~~~~~~~~~~~~~~~~~~

Some targets have a special register that holds the 'flags' for the
result of an operation or comparison.  Normally, the contents of that
register are either unmodifed by the asm, or the ``asm`` statement is
considered to clobber the contents.

On some targets, a special form of output operand exists by which
conditions in the flags register may be outputs of the asm.  The set of
conditions supported are target specific, but the general rule is that
the output variable must be a scalar integer, and the value is boolean.
When supported, the target defines the preprocessor symbol
``__GCC_ASM_FLAG_OUTPUTS__``.

Because of the special nature of the flag output operands, the constraint
may not include alternatives.

Most often, the target has only one flags register, and thus is an implied
operand of many instructions.  In this case, the operand should not be
referenced within the assembler template via ``%0`` etc, as there's
no corresponding text in the assembly language.

ARM AArch64
  The flag output constraints for the ARM family are of the form
  :samp:`=@cc{cond}` where :samp:`{cond}` is one of the standard
  conditions defined in the ARM ARM for ``ConditionHolds``.

  ``eq``
    Z flag set, or equal

  ``ne``
    Z flag clear or not equal

  ``cs`` ``hs``
    C flag set or unsigned greater than equal

  ``cc`` ``lo``
    C flag clear or unsigned less than

  ``mi``
    N flag set or 'minus'

  ``pl``
    N flag clear or 'plus'

  ``vs``
    V flag set or signed overflow

  ``vc``
    V flag clear

  ``hi``
    unsigned greater than

  ``ls``
    unsigned less than equal

  ``ge``
    signed greater than equal

  ``lt``
    signed less than

  ``gt``
    signed greater than

  ``le``
    signed less than equal

    The flag output constraints are not supported in thumb1 mode.

x86 family
  The flag output constraints for the x86 family are of the form
  :samp:`=@cc{cond}` where :samp:`{cond}` is one of the standard
  conditions defined in the ISA manual for ``jcc`` or
  ``setcc``.

  ``a``
    'above' or unsigned greater than

  ``ae``
    'above or equal' or unsigned greater than or equal

  ``b``
    'below' or unsigned less than

  ``be``
    'below or equal' or unsigned less than or equal

  ``c``
    carry flag set

  ``e`` ``z``
    'equal' or zero flag set

  ``g``
    signed greater than

  ``ge``
    signed greater than or equal

  ``l``
    signed less than

  ``le``
    signed less than or equal

  ``o``
    overflow flag set

  ``p``
    parity flag set

  ``s``
    sign flag set

  ``na`` ``nae`` ``nb`` ``nbe`` ``nc`` ``ne`` ``ng`` ``nge`` ``nl`` ``nle`` ``no`` ``np`` ``ns`` ``nz``
    'not' :samp:`{flag}`, or inverted versions of those above

.. index:: asm input operands, asm expressions

.. _inputoperands:

Input Operands
~~~~~~~~~~~~~~

Input operands make values from C variables and expressions available to the
assembly code.

Operands are separated by commas.  Each operand has this format:

.. code-block:: c++

  [ [asmSymbolicName] ] constraint (cexpression)

:samp:`{asmSymbolicName}`
  Specifies a symbolic name for the operand.
  Reference the name in the assembler template
  by enclosing it in square brackets
  (i.e. :samp:`%[Value]`). The scope of the name is the ``asm`` statement
  that contains the definition. Any valid C variable name is acceptable,
  including names already defined in the surrounding code. No two operands
  within the same ``asm`` statement can use the same symbolic name.

  When not using an :samp:`{asmSymbolicName}`, use the (zero-based) position
  of the operand
  in the list of operands in the assembler template. For example if there are
  two output operands and three inputs,
  use :samp:`%2` in the template to refer to the first input operand,
  :samp:`%3` for the second, and :samp:`%4` for the third.

:samp:`{constraint}`
  A string constant specifying constraints on the placement of the operand;
  See :ref:`constraints`, for details.

  Input constraint strings may not begin with either :samp:`=` or :samp:`+`.
  When you list more than one possible location (for example, :samp:`"irm"`),
  the compiler chooses the most efficient one based on the current context.
  If you must use a specific register, but your Machine Constraints do not
  provide sufficient control to select the specific register you want,
  local register variables may provide a solution (see :ref:`local-register-variables`).

  Input constraints can also be digits (for example, ``"0"``). This indicates
  that the specified input must be in the same place as the output constraint
  at the (zero-based) index in the output constraint list.
  When using :samp:`{asmSymbolicName}` syntax for the output operands,
  you may use these names (enclosed in brackets :samp:`[]`) instead of digits.

:samp:`{cexpression}`
  This is the C variable or expression being passed to the ``asm`` statement
  as input.  The enclosing parentheses are a required part of the syntax.

When the compiler selects the registers to use to represent the input
operands, it does not use any of the clobbered registers
(see :ref:`clobbers-and-scratch-registers`).

If there are no output operands but there are input operands, place two
consecutive colons where the output operands would go:

.. code-block:: c++

  __asm__ ("some instructions"
     : /* No outputs. */
     : "r" (Offset / 8));

.. warning::

  Do *not* modify the contents of input-only operands
  (except for inputs tied to outputs). The compiler assumes that on exit from
  the ``asm`` statement these operands contain the same values as they
  had before executing the statement.

It is *not* possible to use clobbers
to inform the compiler that the values in these inputs are changing. One
common work-around is to tie the changing input variable to an output variable
that never gets used. Note, however, that if the code that follows the
``asm`` statement makes no use of any of the output operands, the GCC
optimizers may discard the ``asm`` statement as unneeded
(see :ref:`volatile`).

``asm`` supports operand modifiers on operands (for example :samp:`%k2`
instead of simply :samp:`%2`). Typically these qualifiers are hardware
dependent. The list of supported modifiers for x86 is found at
:ref:`x86operandmodifiers`.

In this example using the fictitious ``combine`` instruction, the
constraint ``"0"`` for input operand 1 says that it must occupy the same
location as output operand 0. Only input operands may use numbers in
constraints, and they must each refer to an output operand. Only a number (or
the symbolic assembler name) in the constraint can guarantee that one operand
is in the same place as another. The mere fact that ``foo`` is the value of
both operands is not enough to guarantee that they are in the same place in
the generated assembler code.

.. code-block:: c++

  asm ("combine %2, %0"
     : "=r" (foo)
     : "0" (foo), "g" (bar));

Here is an example using symbolic names.

.. code-block:: c++

  asm ("cmoveq %1, %2, %[result]"
     : [result] "=r"(result)
     : "r" (test), "r" (new), "[result]" (old));

.. index:: asm clobbers, asm scratch registers

.. _clobbers-and-scratch-registers:

Clobbers and Scratch Registers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

While the compiler is aware of changes to entries listed in the output
operands, the inline ``asm`` code may modify more than just the outputs. For
example, calculations may require additional registers, or the processor may
overwrite a register as a side effect of a particular assembler instruction.
In order to inform the compiler of these changes, list them in the clobber
list. Clobber list items are either register names or the special clobbers
(listed below). Each clobber list item is a string constant
enclosed in double quotes and separated by commas.

Clobber descriptions may not in any way overlap with an input or output
operand. For example, you may not have an operand describing a register class
with one member when listing that register in the clobber list. Variables
declared to live in specific registers (see :ref:`explicit-register-variables`) and used
as ``asm`` input or output operands must have no part mentioned in the
clobber description. In particular, there is no way to specify that input
operands get modified without also specifying them as output operands.

When the compiler selects which registers to use to represent input and output
operands, it does not use any of the clobbered registers. As a result,
clobbered registers are available for any use in the assembler code.

Another restriction is that the clobber list should not contain the
stack pointer register.  This is because the compiler requires the
value of the stack pointer to be the same after an ``asm``
statement as it was on entry to the statement.  However, previous
versions of GCC did not enforce this rule and allowed the stack
pointer to appear in the list, with unclear semantics.  This behavior
is deprecated and listing the stack pointer may become an error in
future versions of GCC.

Here is a realistic example for the VAX showing the use of clobbered
registers:

.. code-block:: c++

  asm volatile ("movc3 %0, %1, %2"
                     : /* No outputs. */
                     : "g" (from), "g" (to), "g" (count)
                     : "r0", "r1", "r2", "r3", "r4", "r5", "memory");

Also, there are two special clobber arguments:

``"cc"``
  The ``"cc"`` clobber indicates that the assembler code modifies the flags
  register. On some machines, GCC represents the condition codes as a specific
  hardware register; ``"cc"`` serves to name this register.
  On other machines, condition code handling is different,
  and specifying ``"cc"`` has no effect. But
  it is valid no matter what the target.

``"memory"``
  The ``"memory"`` clobber tells the compiler that the assembly code
  performs memory
  reads or writes to items other than those listed in the input and output
  operands (for example, accessing the memory pointed to by one of the input
  parameters). To ensure memory contains correct values, GCC may need to flush
  specific register values to memory before executing the ``asm``. Further,
  the compiler does not assume that any values read from memory before an
  ``asm`` remain unchanged after that ``asm`` ; it reloads them as
  needed.
  Using the ``"memory"`` clobber effectively forms a read/write
  memory barrier for the compiler.

  Note that this clobber does not prevent the *processor* from doing
  speculative reads past the ``asm`` statement. To prevent that, you need
  processor-specific fence instructions.

Flushing registers to memory has performance implications and may be
an issue for time-sensitive code.  You can provide better information
to GCC to avoid this, as shown in the following examples.  At a
minimum, aliasing rules allow GCC to know what memory *doesn't*
need to be flushed.

Here is a fictitious sum of squares instruction, that takes two
pointers to floating point values in memory and produces a floating
point register output.
Notice that ``x``, and ``y`` both appear twice in the ``asm``
parameters, once to specify memory accessed, and once to specify a
base register used by the ``asm``.  You won't normally be wasting a
register by doing this as GCC can use the same register for both
purposes.  However, it would be foolish to use both ``%1`` and
``%3`` for ``x`` in this ``asm`` and expect them to be the
same.  In fact, ``%3`` may well not be a register.  It might be a
symbolic memory reference to the object pointed to by ``x``.

.. code-block:: c++

  asm ("sumsq %0, %1, %2"
       : "+f" (result)
       : "r" (x), "r" (y), "m" (*x), "m" (*y));

Here is a fictitious ``*z++ = *x++ * *y++`` instruction.
Notice that the ``x``, ``y`` and ``z`` pointer registers
must be specified as input/output because the ``asm`` modifies
them.

.. code-block:: c++

  asm ("vecmul %0, %1, %2"
       : "+r" (z), "+r" (x), "+r" (y), "=m" (*z)
       : "m" (*x), "m" (*y));

An x86 example where the string memory argument is of unknown length.

.. code-block:: c++

  asm("repne scasb"
      : "=c" (count), "+D" (p)
      : "m" (*(const char (*)[]) p), "0" (-1), "a" (0));

If you know the above will only be reading a ten byte array then you
could instead use a memory input like:
``"m" (*(const char (*)[10]) p)``.

Here is an example of a PowerPC vector scale implemented in assembly,
complete with vector and condition code clobbers, and some initialized
offset registers that are unchanged by the ``asm``.

.. code-block:: c++

  void
  dscal (size_t n, double *x, double alpha)
  {
    asm ("/* lots of asm here */"
         : "+m" (*(double (*)[n]) x), "+&r" (n), "+b" (x)
         : "d" (alpha), "b" (32), "b" (48), "b" (64),
           "b" (80), "b" (96), "b" (112)
         : "cr0",
           "vs32","vs33","vs34","vs35","vs36","vs37","vs38","vs39",
           "vs40","vs41","vs42","vs43","vs44","vs45","vs46","vs47");
  }

Rather than allocating fixed registers via clobbers to provide scratch
registers for an ``asm`` statement, an alternative is to define a
variable and make it an early-clobber output as with ``a2`` and
``a3`` in the example below.  This gives the compiler register
allocator more freedom.  You can also define a variable and make it an
output tied to an input as with ``a0`` and ``a1``, tied
respectively to ``ap`` and ``lda``.  Of course, with tied
outputs your ``asm`` can't use the input value after modifying the
output register since they are one and the same register.  What's
more, if you omit the early-clobber on the output, it is possible that
GCC might allocate the same register to another of the inputs if GCC
could prove they had the same value on entry to the ``asm``.  This
is why ``a1`` has an early-clobber.  Its tied input, ``lda``
might conceivably be known to have the value 16 and without an
early-clobber share the same register as ``%11``.  On the other
hand, ``ap`` can't be the same as any of the other inputs, so an
early-clobber on ``a0`` is not needed.  It is also not desirable in
this case.  An early-clobber on ``a0`` would cause GCC to allocate
a separate register for the ``"m" (*(const double (*)[]) ap)``
input.  Note that tying an input to an output is the way to set up an
initialized temporary register modified by an ``asm`` statement.
An input not tied to an output is assumed by GCC to be unchanged, for
example ``"b" (16)`` below sets up ``%11`` to 16, and GCC might
use that register in following code if the value 16 happened to be
needed.  You can even use a normal ``asm`` output for a scratch if
all inputs that might share the same register are consumed before the
scratch is used.  The VSX registers clobbered by the ``asm``
statement could have used this technique except for GCC's limit on the
number of ``asm`` parameters.

.. code-block:: c++

  static void
  dgemv_kernel_4x4 (long n, const double *ap, long lda,
                    const double *x, double *y, double alpha)
  {
    double *a0;
    double *a1;
    double *a2;
    double *a3;

    __asm__
      (
       /* lots of asm here */
       "#n=%1 ap=%8=%12 lda=%13 x=%7=%10 y=%0=%2 alpha=%9 o16=%11\n"
       "#a0=%3 a1=%4 a2=%5 a3=%6"
       :
         "+m" (*(double (*)[n]) y),
         "+&r" (n),	// 1
         "+b" (y),	// 2
         "=b" (a0),	// 3
         "=&b" (a1),	// 4
         "=&b" (a2),	// 5
         "=&b" (a3)	// 6
       :
         "m" (*(const double (*)[n]) x),
         "m" (*(const double (*)[]) ap),
         "d" (alpha),	// 9
         "r" (x),		// 10
         "b" (16),	// 11
         "3" (ap),	// 12
         "4" (lda)	// 13
       :
         "cr0",
         "vs32","vs33","vs34","vs35","vs36","vs37",
         "vs40","vs41","vs42","vs43","vs44","vs45","vs46","vs47"
       );
  }

.. index:: asm goto labels

.. _gotolabels:

Goto Labels
~~~~~~~~~~~

``asm goto`` allows assembly code to jump to one or more C labels.  The
:samp:`{GotoLabels}` section in an ``asm goto`` statement contains
a comma-separated
list of all C labels to which the assembler code may jump. GCC assumes that
``asm`` execution falls through to the next statement (if this is not the
case, consider using the ``__builtin_unreachable`` intrinsic after the
``asm`` statement). Optimization of ``asm goto`` may be improved by
using the :fn-attr:`hot` and :fn-attr:`cold` label attributes (see :ref:`label-attributes`).

If the assembler code does modify anything, use the ``"memory"`` clobber
to force the
optimizers to flush all register values to memory and reload them if
necessary after the ``asm`` statement.

Also note that an ``asm goto`` statement is always implicitly
considered volatile.

Be careful when you set output operands inside ``asm goto`` only on
some possible control flow paths.  If you don't set up the output on
given path and never use it on this path, it is okay.  Otherwise, you
should use :samp:`+` constraint modifier meaning that the operand is
input and output one.  With this modifier you will have the correct
values on all possible paths from the ``asm goto``.

To reference a label in the assembler template, prefix it with
:samp:`%l` (lowercase :samp:`L`) followed by its (zero-based) position
in :samp:`{GotoLabels}` plus the number of input and output operands.
Output operand with constraint modifier :samp:`+` is counted as two
operands because it is considered as one output and one input operand.
For example, if the ``asm`` has three inputs, one output operand
with constraint modifier :samp:`+` and one output operand with
constraint modifier :samp:`=` and references two labels, refer to the
first label as :samp:`%l6` and the second as :samp:`%l7`).

Alternately, you can reference labels using the actual C label name
enclosed in brackets.  For example, to reference a label named
``carry``, you can use :samp:`%l[carry]`.  The label must still be
listed in the :samp:`{GotoLabels}` section when using this approach.  It
is better to use the named references for labels as in this case you
can avoid counting input and output operands and special treatment of
output operands with constraint modifier :samp:`+`.

Here is an example of ``asm goto`` for i386:

.. code-block:: c++

  asm goto (
      "btl %1, %0\n\t"
      "jc %l2"
      : /* No outputs. */
      : "r" (p1), "r" (p2)
      : "cc"
      : carry);

  return 0;

  carry:
  return 1;

The following example shows an ``asm goto`` that uses a memory clobber.

.. code-block:: c++

  int frob(int x)
  {
    int y;
    asm goto ("frob %%r5, %1; jc %l[error]; mov (%2), %%r5"
              : /* No outputs. */
              : "r"(x), "r"(&y)
              : "r5", "memory"
              : error);
    return y;
  error:
    return -1;
  }

The following example shows an ``asm goto`` that uses an output.

.. code-block:: c++

  int foo(int count)
  {
    asm goto ("dec %0; jb %l[stop]"
              : "+r" (count)
              :
              :
              : stop);
    return count;
  stop:
    return 0;
  }

The following artificial example shows an ``asm goto`` that sets
up an output only on one path inside the ``asm goto``.  Usage of
constraint modifier ``=`` instead of ``+`` would be wrong as
``factor`` is used on all paths from the ``asm goto``.

.. code-block:: c++

  int foo(int inp)
  {
    int factor = 0;
    asm goto ("cmp %1, 10; jb %l[lab]; mov 2, %0"
              : "+r" (factor)
              : "r" (inp)
              :
              : lab);
  lab:
    return inp * factor; /* return 2 * inp or 0 if inp < 10 */
  }

.. _x86operandmodifiers:

x86 Operand Modifiers
~~~~~~~~~~~~~~~~~~~~~

References to input, output, and goto operands in the assembler template
of extended ``asm`` statements can use
modifiers to affect the way the operands are formatted in
the code output to the assembler. For example, the
following code uses the :samp:`h` and :samp:`b` modifiers for x86:

.. code-block:: c++

  uint16_t  num;
  asm volatile ("xchg %h0, %b0" : "+a" (num) );

These modifiers generate this assembler code:

.. code-block:: c++

  xchg %ah, %al

The rest of this discussion uses the following code for illustrative purposes.

.. code-block:: c++

  int main()
  {
     int iInt = 1;

  top:

     asm volatile goto ("some assembler instructions here"
     : /* No outputs. */
     : "q" (iInt), "X" (sizeof(unsigned char) + 1), "i" (42)
     : /* No clobbers. */
     : top);
  }

With no modifiers, this is what the output from the operands would be
for the :samp:`att` and :samp:`intel` dialects of assembler:

.. list-table::
   :header-rows: 1

   * - Operand
     - :samp:`att`
     - :samp:`intel`

   * - ``%0``
     - ``%eax``
     - ``eax``
   * - ``%1``
     - ``$2``
     - ``2``
   * - ``%3``
     - ``$.L3``
     - ``OFFSET FLAT:.L3``
   * - ``%4``
     - ``$8``
     - ``8``
   * - ``%5``
     - ``%xmm0``
     - ``xmm0``
   * - ``%7``
     - ``$0``
     - ``0``

The table below shows the list of supported modifiers and their effects.

.. list-table::
   :header-rows: 1
   :widths: 10 50 10 10 10

   * - Modifier
     - Description
     - Operand
     - :samp:`att`
     - :samp:`intel`

   * - ``A``
     - Print an absolute memory reference.
     - ``%A0``
     - ``*%rax``
     - ``rax``
   * - ``b``
     - Print the QImode name of the register.
     - ``%b0``
     - ``%al``
     - ``al``
   * - ``B``
     - print the opcode suffix of b.
     - ``%B0``
     - ``b``
     -
   * - ``c``
     - Require a constant operand and print the constant expression with no punctuation.
     - ``%c1``
     - ``2``
     - ``2``
   * - ``d``
     - print duplicated register operand for AVX instruction.
     - ``%d5``
     - ``%xmm0, %xmm0``
     - ``xmm0, xmm0``
   * - ``E``
     - Print the address in Double Integer (DImode) mode (8 bytes) when the target is 64-bit. Otherwise mode is unspecified (VOIDmode).
     - ``%E1``
     - ``%(rax)``
     - ``[rax]``
   * - ``g``
     - Print the V16SFmode name of the register.
     - ``%g0``
     - ``%zmm0``
     - ``zmm0``
   * - ``h``
     - Print the QImode name for a 'high' register.
     - ``%h0``
     - ``%ah``
     - ``ah``
   * - ``H``
     - Add 8 bytes to an offsettable memory reference. Useful when accessing the high 8 bytes of SSE values. For a memref in (%rax), it generates
     - ``%H0``
     - ``8(%rax)``
     - ``8[rax]``
   * - ``k``
     - Print the SImode name of the register.
     - ``%k0``
     - ``%eax``
     - ``eax``
   * - ``l``
     - Print the label name with no punctuation.
     - ``%l3``
     - ``.L3``
     - ``.L3``
   * - ``L``
     - print the opcode suffix of l.
     - ``%L0``
     - ``l``
     -
   * - ``N``
     - print maskz.
     - ``%N7``
     - ``{z}``
     - ``{z}``
   * - ``p``
     - Print raw symbol name (without syntax-specific prefixes).
     - ``%p2``
     - ``42``
     - ``42``
   * - ``P``
     - If used for a function, print the PLT suffix and generate PIC code. For example, emit ``foo@PLT`` instead of 'foo' for the function foo(). If used for a constant, drop all syntax-specific prefixes and issue the bare constant. See ``p`` above.
     -
     -
     -
   * - ``q``
     - Print the DImode name of the register.
     - ``%q0``
     - ``%rax``
     - ``rax``
   * - ``Q``
     - print the opcode suffix of q.
     - ``%Q0``
     - ``q``
     -
   * - ``R``
     - print embedded rounding and sae.
     - ``%R4``
     - ``{rn-sae},``
     - ``, {rn-sae}``
   * - ``r``
     - print only sae.
     - ``%r4``
     - ``{sae},``
     - ``, {sae}``
   * - ``s``
     - print a shift double count, followed by the assemblers argument delimiterprint the opcode suffix of s.
     - ``%s1``
     - ``$2,``
     - ``2,``
   * - ``S``
     - print the opcode suffix of s.
     - ``%S0``
     - ``s``
     -
   * - ``t``
     - print the V8SFmode name of the register.
     - ``%t5``
     - ``%ymm0``
     - ``ymm0``
   * - ``T``
     - print the opcode suffix of t.
     - ``%T0``
     - ``t``
     -
   * - ``V``
     - print naked full integer register name without %.
     - ``%V0``
     - ``eax``
     - ``eax``
   * - ``w``
     - Print the HImode name of the register.
     - ``%w0``
     - ``%ax``
     - ``ax``
   * - ``W``
     - print the opcode suffix of w.
     - ``%W0``
     - ``w``
     -
   * - ``x``
     - print the V4SFmode name of the register.
     - ``%x5``
     - ``%xmm0``
     - ``xmm0``
   * - ``y``
     - print "st(0)" instead of "st" as a register.
     - ``%y6``
     - ``%st(0)``
     - ``st(0)``
   * - ``z``
     - Print the opcode suffix for the size of the current integer operand (one of ``b`` / ``w`` / ``l`` / ``q``).
     - ``%z0``
     - ``l``
     -
   * - ``Z``
     - Like ``z``, with special suffixes for x87 instructions.
     -
     -
     -

.. _x86floatingpointasmoperands:

x86 Floating-Point asm Operands
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

On x86 targets, there are several rules on the usage of stack-like registers
in the operands of an ``asm``.  These rules apply only to the operands
that are stack-like registers:

* Given a set of input registers that die in an ``asm``, it is
  necessary to know which are implicitly popped by the ``asm``, and
  which must be explicitly popped by GCC.

  An input register that is implicitly popped by the ``asm`` must be
  explicitly clobbered, unless it is constrained to match an
  output operand.

* For any input register that is implicitly popped by an ``asm``, it is
  necessary to know how to adjust the stack to compensate for the pop.
  If any non-popped input is closer to the top of the reg-stack than
  the implicitly popped register, it would not be possible to know what the
  stack looked like---it's not clear how the rest of the stack 'slides
  up'.

  All implicitly popped input registers must be closer to the top of
  the reg-stack than any input that is not implicitly popped.

  It is possible that if an input dies in an ``asm``, the compiler might
  use the input register for an output reload.  Consider this example:

  .. code-block:: c++

    asm ("foo" : "=t" (a) : "f" (b));

  This code says that input ``b`` is not popped by the ``asm``, and that
  the ``asm`` pushes a result onto the reg-stack, i.e., the stack is one
  deeper after the ``asm`` than it was before.  But, it is possible that
  reload may think that it can use the same register for both the input and
  the output.

  To prevent this from happening,
  if any input operand uses the :samp:`f` constraint, all output register
  constraints must use the :samp:`&` early-clobber modifier.

  The example above is correctly written as:

  .. code-block:: c++

    asm ("foo" : "=&t" (a) : "f" (b));

* Some operands need to be in particular places on the stack.  All
  output operands fall in this category---GCC has no other way to
  know which registers the outputs appear in unless you indicate
  this in the constraints.

  Output operands must specifically indicate which register an output
  appears in after an ``asm``.  :samp:`=f` is not allowed: the operand
  constraints must select a class with a single register.

* Output operands may not be 'inserted' between existing stack registers.
  Since no 387 opcode uses a read/write operand, all output operands
  are dead before the ``asm``, and are pushed by the ``asm``.
  It makes no sense to push anywhere but the top of the reg-stack.

  Output operands must start at the top of the reg-stack: output
  operands may not 'skip' a register.

* Some ``asm`` statements may need extra stack space for internal
  calculations.  This can be guaranteed by clobbering stack registers
  unrelated to the inputs and outputs.

This ``asm``
takes one input, which is internally popped, and produces two outputs.

.. code-block:: c++

  asm ("fsincos" : "=t" (cos), "=u" (sin) : "0" (inp));

This ``asm`` takes two inputs, which are popped by the ``fyl2xp1`` opcode,
and replaces them with one output.  The ``st(1)`` clobber is necessary
for the compiler to know that ``fyl2xp1`` pops both inputs.

.. code-block:: c++

  asm ("fyl2xp1" : "=t" (result) : "0" (x), "u" (y) : "st(1)");

.. _msp430operandmodifiers:

MSP430 Operand Modifiers
~~~~~~~~~~~~~~~~~~~~~~~~

The list below describes the supported modifiers and their effects for MSP430.

.. list-table::
   :header-rows: 1
   :widths: 10 90

   * - Modifier
     - Description

   * - ``A``
     - Select low 16-bits of the constant/register/memory operand.
   * - ``B``
     - Select high 16-bits of the constant/register/memory operand.
   * - ``C``
     - Select bits 32-47 of the constant/register/memory operand.
   * - ``D``
     - Select bits 48-63 of the constant/register/memory operand.
   * - ``H``
     - Equivalent to ``B`` (for backwards compatibility).
   * - ``I``
     - Print the inverse (logical ``NOT``) of the constant value.
   * - ``J``
     - Print an integer without a ``#`` prefix.
   * - ``L``
     - Equivalent to ``A`` (for backwards compatibility).
   * - ``O``
     - Offset of the current frame from the top of the stack.
   * - ``Q``
     - Use the ``A`` instruction postfix.
   * - ``R``
     - Inverse of condition code, for unsigned comparisons.
   * - ``W``
     - Subtract 16 from the constant value.
   * - ``X``
     - Use the ``X`` instruction postfix.
   * - ``Y``
     - Subtract 4 from the constant value.
   * - ``Z``
     - Subtract 1 from the constant value.
   * - ``b``
     - Append ``.B``, ``.W`` or ``.A`` to the instruction, depending on the mode.
   * - ``d``
     - Offset 1 byte of a memory reference or constant value.
   * - ``e``
     - Offset 3 bytes of a memory reference or constant value.
   * - ``f``
     - Offset 5 bytes of a memory reference or constant value.
   * - ``g``
     - Offset 7 bytes of a memory reference or constant value.
   * - ``p``
     - Print the value of 2, raised to the power of the given constant.  Used to select the specified bit position.
   * - ``r``
     - Inverse of condition code, for signed comparisons.
   * - ``x``
     - Equivialent to ``X``, but only for pointers.

.. Most of this node appears by itself (in a different place) even
   when the INTERNALS flag is clear.  Passages that require the internals
   manual's context are conditionalized to appear only in the internals manual.

.. index:: operand constraints, asm, constraints, asm, asm constraints

.. _constraints:

Constraints for asm Operands
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here are specific details on what constraint letters you can use with
``asm`` operands.
Constraints can say whether
an operand may be in a register, and which kinds of register; whether the
operand can be a memory reference, and which kinds of address; whether the
operand may be an immediate constant, and which possible values it may
have.  Constraints can also require two operands to match.
Side-effects aren't allowed in operands of inline ``asm``, unless
:samp:`<` or :samp:`>` constraints are used, because there is no guarantee
that the side effects will happen exactly once in an instruction that can update
the addressing register.

.. toctree::
  :maxdepth: 2


.. include:: ../../../../doc/md.rst


.. Each of the following nodes are wrapped in separate
   "@ifset INTERNALS" to work around memory limits for the default
   configuration in older tetex distributions.  Known to not work:
   tetex-1.0.7, known to work: tetex-2.0.2.

.. index:: assembler names for identifiers, names used in assembler code, identifiers, names in assembler code

.. _asm-labels:

Controlling Names Used in Assembler Code
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can specify the name to be used in the assembler code for a C
function or variable by writing the ``asm`` (or ``__asm__``)
keyword after the declarator.
It is up to you to make sure that the assembler names you choose do not
conflict with any other assembler symbols, or reference registers.

Assembler names for data
^^^^^^^^^^^^^^^^^^^^^^^^

This sample shows how to specify the assembler name for data:

.. code-block:: c++

  int foo asm ("myfoo") = 2;

This specifies that the name to be used for the variable ``foo`` in
the assembler code should be :samp:`myfoo` rather than the usual
:samp:`_foo`.

On systems where an underscore is normally prepended to the name of a C
variable, this feature allows you to define names for the
linker that do not start with an underscore.

GCC does not support using this feature with a non-static local variable
since such variables do not have assembler names.  If you are
trying to put the variable in a particular register, see
:ref:`explicit-register-variables`.

Assembler names for functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To specify the assembler name for functions, write a declaration for the
function before its definition and put ``asm`` there, like this:

.. code-block:: c++

  int func (int x, int y) asm ("MYFUNC");

  int func (int x, int y)
  {
     /* ... */

This specifies that the name to be used for the function ``func`` in
the assembler code should be ``MYFUNC``.

.. _explicit-register-variables:

Variables in Specified Registers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: explicit register variables, variables in specified registers, specified registers

.. _explicit-reg-vars:

GNU C allows you to associate specific hardware registers with C
variables.  In almost all cases, allowing the compiler to assign
registers produces the best code.  However under certain unusual
circumstances, more precise control over the variable storage is
required.

Both global and local variables can be associated with a register.  The
consequences of performing this association are very different between
the two, as explained in the sections below.

.. toctree::
  :maxdepth: 2


.. _global-register-variables:

Defining Global Register Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index:: global register variables, registers, global variables in, registers, global allocation

.. _global-reg-vars:

You can define a global register variable and associate it with a specified
register like this:

.. code-block:: c++

  register int *foo asm ("r12");

Here ``r12`` is the name of the register that should be used. Note that
this is the same syntax used for defining local register variables, but for
a global variable the declaration appears outside a function. The
``register`` keyword is required, and cannot be combined with
``static``. The register name must be a valid register name for the
target platform.

Do not use type qualifiers such as ``const`` and ``volatile``, as
the outcome may be contrary to expectations.  In  particular, using the
``volatile`` qualifier does not fully prevent the compiler from
optimizing accesses to the register.

Registers are a scarce resource on most systems and allowing the
compiler to manage their usage usually results in the best code. However,
under special circumstances it can make sense to reserve some globally.
For example this may be useful in programs such as programming language
interpreters that have a couple of global variables that are accessed
very often.

After defining a global register variable, for the current compilation
unit:

* If the register is a call-saved register, call ABI is affected:
  the register will not be restored in function epilogue sequences after
  the variable has been assigned.  Therefore, functions cannot safely
  return to callers that assume standard ABI.

* Conversely, if the register is a call-clobbered register, making
  calls to functions that use standard ABI may lose contents of the variable.
  Such calls may be created by the compiler even if none are evident in
  the original program, for example when libgcc functions are used to
  make up for unavailable instructions.

* Accesses to the variable may be optimized as usual and the register
  remains available for allocation and use in any computations, provided that
  observable values of the variable are not affected.

* If the variable is referenced in inline assembly, the type of access
  must be provided to the compiler via constraints (see :ref:`constraints`).
  Accesses from basic asms are not supported.

Note that these points *only* apply to code that is compiled with the
definition. The behavior of code that is merely linked in (for example
code from libraries) is not affected.

If you want to recompile source files that do not actually use your global
register variable so they do not use the specified register for any other
purpose, you need not actually add the global register declaration to
their source code. It suffices to specify the compiler option
:option:`-ffixed-reg` (see :ref:`code-gen-options`) to reserve the
register.

Declaring the variable
^^^^^^^^^^^^^^^^^^^^^^

Global register variables cannot have initial values, because an
executable file has no means to supply initial contents for a register.

When selecting a register, choose one that is normally saved and
restored by function calls on your machine. This ensures that code
which is unaware of this reservation (such as library routines) will
restore it before returning.

On machines with register windows, be sure to choose a global
register that is not affected magically by the function call mechanism.

.. index:: qsort, and global register variables

Using the variable
^^^^^^^^^^^^^^^^^^

When calling routines that are not aware of the reservation, be
cautious if those routines call back into code which uses them. As an
example, if you call the system library version of ``qsort``, it may
clobber your registers during execution, but (if you have selected
appropriate registers) it will restore them before returning. However
it will *not* restore them before calling ``qsort`` 's comparison
function. As a result, global values will not reliably be available to
the comparison function unless the ``qsort`` function itself is rebuilt.

Similarly, it is not safe to access the global register variables from signal
handlers or from more than one thread of control. Unless you recompile
them specially for the task at hand, the system library routines may
temporarily use the register for other things.  Furthermore, since the register
is not reserved exclusively for the variable, accessing it from handlers of
asynchronous signals may observe unrelated temporary values residing in the
register.

.. index:: register variable after longjmp, global register after longjmp, value after longjmp, longjmp, setjmp

On most machines, ``longjmp`` restores to each global register
variable the value it had at the time of the ``setjmp``. On some
machines, however, ``longjmp`` does not change the value of global
register variables. To be portable, the function that called ``setjmp``
should make other arrangements to save the values of the global register
variables, and to restore them in a ``longjmp``. This way, the same
thing happens regardless of what ``longjmp`` does.

.. _local-register-variables:

Specifying Registers for Local Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index:: local variables, specifying registers, specifying registers for local variables, registers for local variables

.. _local-reg-vars:

You can define a local register variable and associate it with a specified
register like this:

.. code-block:: c++

  register int *foo asm ("r12");

Here ``r12`` is the name of the register that should be used.  Note
that this is the same syntax used for defining global register variables,
but for a local variable the declaration appears within a function.  The
``register`` keyword is required, and cannot be combined with
``static``.  The register name must be a valid register name for the
target platform.

Do not use type qualifiers such as ``const`` and ``volatile``, as
the outcome may be contrary to expectations. In particular, when the
``const`` qualifier is used, the compiler may substitute the
variable with its initializer in ``asm`` statements, which may cause
the corresponding operand to appear in a different register.

As with global register variables, it is recommended that you choose
a register that is normally saved and restored by function calls on your
machine, so that calls to library routines will not clobber it.

The only supported use for this feature is to specify registers
for input and output operands when calling Extended ``asm``
(see :ref:`extended-asm`).  This may be necessary if the constraints for a
particular machine don't provide sufficient control to select the desired
register.  To force an operand into a register, create a local variable
and specify the register name after the variable's declaration.  Then use
the local variable for the ``asm`` operand and specify any constraint
letter that matches the register:

.. code-block:: c++

  register int *p1 asm ("r0") = ...;
  register int *p2 asm ("r1") = ...;
  register int *result asm ("r0");
  asm ("sysint" : "=r" (result) : "0" (p1), "r" (p2));

.. warning::

  In the above example, be aware that a register (for example
  ``r0``) can be call-clobbered by subsequent code, including function
  calls and library calls for arithmetic operators on other variables (for
  example the initialization of ``p2``).  In this case, use temporary
  variables for expressions between the register assignments:

.. code-block:: c++

  int t1 = ...;
  register int *p1 asm ("r0") = ...;
  register int *p2 asm ("r1") = t1;
  register int *result asm ("r0");
  asm ("sysint" : "=r" (result) : "0" (p1), "r" (p2));

Defining a register variable does not reserve the register.  Other than
when invoking the Extended ``asm``, the contents of the specified
register are not guaranteed.  For this reason, the following uses
are explicitly *not* supported.  If they appear to work, it is only
happenstance, and may stop working as intended due to (seemingly)
unrelated changes in surrounding code, or even minor changes in the
optimization of a future version of gcc:

* Passing parameters to or from Basic ``asm``

* Passing parameters to or from Extended ``asm`` without using input
  or output operands.

* Passing parameters to or from routines written in assembler (or
  other languages) using non-standard calling conventions.

Some developers use Local Register Variables in an attempt to improve
gcc's allocation of registers, especially in large functions.  In this
case the register name is essentially a hint to the register allocator.
While in some instances this can generate better code, improvements are
subject to the whims of the allocator/optimizers.  Since there are no
guarantees that your improvements won't be lost, this usage of Local
Register Variables is discouraged.

On the MIPS platform, there is related use for local register variables
with slightly different characteristics (see :ref:`gccint:mips-coprocessors`).

.. _size-of-an-asm:

Size of an asm
^^^^^^^^^^^^^^

Some targets require that GCC track the size of each instruction used
in order to generate correct code.  Because the final length of the
code produced by an ``asm`` statement is only known by the
assembler, GCC must make an estimate as to how big it will be.  It
does this by counting the number of instructions in the pattern of the
``asm`` and multiplying that by the length of the longest
instruction supported by that processor.  (When working out the number
of instructions, it assumes that any occurrence of a newline or of
whatever statement separator character is supported by the assembler ---
typically :samp:`;` --- indicates the end of an instruction.)

Normally, GCC's estimate is adequate to ensure that correct
code is generated, but it is possible to confuse the compiler if you use
pseudo instructions or assembler macros that expand into multiple real
instructions, or if you use assembler directives that expand to more
space in the object file than is needed for a single instruction.
If this happens then the assembler may produce a diagnostic saying that
a label is unreachable.

.. index:: asm inline

This size is also used for inlining decisions.  If you use ``asm inline``
instead of just ``asm``, then for inlining purposes the size of the asm
is taken as the minimum size, ignoring how many instructions GCC thinks it is.
