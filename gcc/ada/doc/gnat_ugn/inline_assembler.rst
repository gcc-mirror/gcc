.. role:: switch(samp)

.. _Inline_Assembler:

****************
Inline Assembler
****************

.. index:: Inline Assembler

If you need to write low-level software that interacts directly
with the hardware, Ada provides two ways to incorporate assembly
language code into your program.  First, you can import and invoke
external routines written in assembly language, an Ada feature fully
supported by GNAT.  However, for small sections of code it may be simpler
or more efficient to include assembly language statements directly
in your Ada source program, using the facilities of the implementation-defined
package ``System.Machine_Code``, which incorporates the gcc
Inline Assembler.  The Inline Assembler approach offers a number of advantages,
including the following:

* No need to use non-Ada tools
* Consistent interface over different targets
* Automatic usage of the proper calling conventions
* Access to Ada constants and variables
* Definition of intrinsic routines
* Possibility of inlining a subprogram comprising assembler code
* Code optimizer can take Inline Assembler code into account

This appendix presents a series of examples to show you how to use
the Inline Assembler.  Although it focuses on the Intel x86,
the general approach applies also to other processors.
It is assumed that you are familiar with Ada
and with assembly language programming.

.. _Basic_Assembler_Syntax:

Basic Assembler Syntax
======================

The assembler used by GNAT and gcc is based not on the Intel assembly
language, but rather on a language that descends from the AT&T Unix
assembler ``as`` (and which is often referred to as 'AT&T syntax').
The following table summarizes the main features of ``as`` syntax
and points out the differences from the Intel conventions.
See the gcc ``as`` and ``gas`` (an ``as`` macro
pre-processor) documentation for further information.


| *Register names*
|   gcc / ``as``: Prefix with '%'; for example ``%eax``
|   Intel: No extra punctuation; for example ``eax``


| *Immediate operand*
|   gcc / ``as``: Prefix with '$'; for example ``$4``
|   Intel: No extra punctuation; for example ``4``


| *Address*
|   gcc / ``as``: Prefix with '$'; for example ``$loc``
|   Intel: No extra punctuation; for example ``loc``


| *Memory contents*
|   gcc / ``as``: No extra punctuation; for example ``loc``
|   Intel: Square brackets; for example ``[loc]``


| *Register contents*
|   gcc / ``as``: Parentheses; for example ``(%eax)``
|   Intel: Square brackets; for example ``[eax]``


| *Hexadecimal numbers*
|   gcc / ``as``: Leading '0x' (C language syntax); for example ``0xA0``
|   Intel: Trailing 'h'; for example ``A0h``


| *Operand size*
|   gcc / ``as``: Explicit in op code; for example ``movw`` to move a 16-bit word
|   Intel: Implicit, deduced by assembler; for example ``mov``


| *Instruction repetition*
|   gcc / ``as``: Split into two lines; for example
|     ``rep``
|     ``stosl``
|   Intel: Keep on one line; for example ``rep stosl``


| *Order of operands*
|   gcc / ``as``: Source first; for example ``movw $4, %eax``
|   Intel: Destination first; for example ``mov eax, 4``


.. _A_Simple_Example_of_Inline_Assembler:

A Simple Example of Inline Assembler
====================================

The following example will generate a single assembly language statement,
``nop``, which does nothing.  Despite its lack of run-time effect,
the example will be useful in illustrating the basics of
the Inline Assembler facility.

  .. code-block:: ada

     with System.Machine_Code; use System.Machine_Code;
     procedure Nothing is
     begin
        Asm ("nop");
     end Nothing;

``Asm`` is a procedure declared in package ``System.Machine_Code``;
here it takes one parameter, a *template string* that must be a static
expression and that will form the generated instruction.
``Asm`` may be regarded as a compile-time procedure that parses
the template string and additional parameters (none here),
from which it generates a sequence of assembly language instructions.

The examples in this chapter will illustrate several of the forms
for invoking ``Asm``; a complete specification of the syntax
is found in the ``Machine_Code_Insertions`` section of the
:title:`GNAT Reference Manual`.

Under the standard GNAT conventions, the ``Nothing`` procedure
should be in a file named :file:`nothing.adb`.
You can build the executable in the usual way:

  ::

     $ gnatmake nothing

However, the interesting aspect of this example is not its run-time behavior
but rather the generated assembly code.
To see this output, invoke the compiler as follows:

  ::

     $  gcc -c -S -fomit-frame-pointer -gnatp nothing.adb

where the options are:

* :switch:`-c`
    compile only (no bind or link)

* :switch:`-S`
    generate assembler listing

* :switch:`-fomit-frame-pointer`
    do not set up separate stack frames

* :switch:`-gnatp`
    do not add runtime checks

This gives a human-readable assembler version of the code. The resulting
file will have the same name as the Ada source file, but with a ``.s``
extension. In our example, the file :file:`nothing.s` has the following
contents:

  ::

     .file "nothing.adb"
     gcc2_compiled.:
     ___gnu_compiled_ada:
     .text
        .align 4
     .globl __ada_nothing
     __ada_nothing:
     #APP
        nop
     #NO_APP
        jmp L1
        .align 2,0x90
     L1:
        ret

The assembly code you included is clearly indicated by
the compiler, between the ``#APP`` and ``#NO_APP``
delimiters. The character before the 'APP' and 'NOAPP'
can differ on different targets. For example, GNU/Linux uses '#APP' while
on NT you will see '/APP'.

If you make a mistake in your assembler code (such as using the
wrong size modifier, or using a wrong operand for the instruction) GNAT
will report this error in a temporary file, which will be deleted when
the compilation is finished.  Generating an assembler file will help
in such cases, since you can assemble this file separately using the
``as`` assembler that comes with gcc.

Assembling the file using the command

  ::

     $ as nothing.s

will give you error messages whose lines correspond to the assembler
input file, so you can easily find and correct any mistakes you made.
If there are no errors, ``as`` will generate an object file
:file:`nothing.out`.


.. _Output_Variables_in_Inline_Assembler:

Output Variables in Inline Assembler
====================================

The examples in this section, showing how to access the processor flags,
illustrate how to specify the destination operands for assembly language
statements.


  .. code-block:: ada

     with Interfaces; use Interfaces;
     with Ada.Text_IO; use Ada.Text_IO;
     with System.Machine_Code; use System.Machine_Code;
     procedure Get_Flags is
        Flags : Unsigned_32;
        use ASCII;
     begin
        Asm ("pushfl"          & LF & HT & -- push flags on stack
             "popl %%eax"      & LF & HT & -- load eax with flags
             "movl %%eax, %0",             -- store flags in variable
             Outputs => Unsigned_32'Asm_Output ("=g", Flags));
        Put_Line ("Flags register:" & Flags'Img);
     end Get_Flags;

In order to have a nicely aligned assembly listing, we have separated
multiple assembler statements in the Asm template string with linefeed
(ASCII.LF) and horizontal tab (ASCII.HT) characters.
The resulting section of the assembly output file is:

  ::

     #APP
        pushfl
        popl %eax
        movl %eax, -40(%ebp)
     #NO_APP

It would have been legal to write the Asm invocation as:

  .. code-block:: ada

     Asm ("pushfl popl %%eax movl %%eax, %0")

but in the generated assembler file, this would come out as:

  ::

     #APP
        pushfl popl %eax movl %eax, -40(%ebp)
     #NO_APP

which is not so convenient for the human reader.

We use Ada comments
at the end of each line to explain what the assembler instructions
actually do.  This is a useful convention.

When writing Inline Assembler instructions, you need to precede each register
and variable name with a percent sign.  Since the assembler already requires
a percent sign at the beginning of a register name, you need two consecutive
percent signs for such names in the Asm template string, thus ``%%eax``.
In the generated assembly code, one of the percent signs will be stripped off.

Names such as ``%0``, ``%1``, ``%2``, etc., denote input or output
variables: operands you later define using ``Input`` or ``Output``
parameters to ``Asm``.
An output variable is illustrated in
the third statement in the Asm template string:

  ::

     movl %%eax, %0

The intent is to store the contents of the eax register in a variable that can
be accessed in Ada.  Simply writing ``movl %%eax, Flags`` would not
necessarily work, since the compiler might optimize by using a register
to hold Flags, and the expansion of the ``movl`` instruction would not be
aware of this optimization.  The solution is not to store the result directly
but rather to advise the compiler to choose the correct operand form;
that is the purpose of the ``%0`` output variable.

Information about the output variable is supplied in the ``Outputs``
parameter to ``Asm``:

  .. code-block:: ada

     Outputs => Unsigned_32'Asm_Output ("=g", Flags));

The output is defined by the ``Asm_Output`` attribute of the target type;
the general format is

  .. code-block:: ada

     Type'Asm_Output (constraint_string, variable_name)

The constraint string directs the compiler how
to store/access the associated variable.  In the example

  .. code-block:: ada

     Unsigned_32'Asm_Output ("=m", Flags);

the ``"m"`` (memory) constraint tells the compiler that the variable
``Flags`` should be stored in a memory variable, thus preventing
the optimizer from keeping it in a register.  In contrast,

  .. code-block:: ada

     Unsigned_32'Asm_Output ("=r", Flags);

uses the ``"r"`` (register) constraint, telling the compiler to
store the variable in a register.

If the constraint is preceded by the equal character '=', it tells
the compiler that the variable will be used to store data into it.

In the ``Get_Flags`` example, we used the ``"g"`` (global) constraint,
allowing the optimizer to choose whatever it deems best.

There are a fairly large number of constraints, but the ones that are
most useful (for the Intel x86 processor) are the following:

 ====== ==========================================
 *=*    output constraint
 *g*    global (i.e., can be stored anywhere)
 *m*    in memory
 *I*    a constant
 *a*    use eax
 *b*    use ebx
 *c*    use ecx
 *d*    use edx
 *S*    use esi
 *D*    use edi
 *r*    use one of eax, ebx, ecx or edx
 *q*    use one of eax, ebx, ecx, edx, esi or edi
 ====== ==========================================

The full set of constraints is described in the gcc and ``as``
documentation; note that it is possible to combine certain constraints
in one constraint string.

You specify the association of an output variable with an assembler operand
through the :samp:`%{n}` notation, where *n* is a non-negative
integer.  Thus in

  .. code-block:: ada

     Asm ("pushfl"          & LF & HT & -- push flags on stack
          "popl %%eax"      & LF & HT & -- load eax with flags
          "movl %%eax, %0",             -- store flags in variable
          Outputs => Unsigned_32'Asm_Output ("=g", Flags));


``%0`` will be replaced in the expanded code by the appropriate operand,
whatever
the compiler decided for the ``Flags`` variable.

In general, you may have any number of output variables:

* Count the operands starting at 0; thus ``%0``, ``%1``, etc.

* Specify the ``Outputs`` parameter as a parenthesized comma-separated list
  of ``Asm_Output`` attributes

For example:

  .. code-block:: ada

     Asm ("movl %%eax, %0" & LF & HT &
          "movl %%ebx, %1" & LF & HT &
          "movl %%ecx, %2",
          Outputs => (Unsigned_32'Asm_Output ("=g", Var_A),   --  %0 = Var_A
                      Unsigned_32'Asm_Output ("=g", Var_B),   --  %1 = Var_B
                      Unsigned_32'Asm_Output ("=g", Var_C))); --  %2 = Var_C

where ``Var_A``, ``Var_B``, and ``Var_C`` are variables
in the Ada program.

As a variation on the ``Get_Flags`` example, we can use the constraints
string to direct the compiler to store the eax register into the ``Flags``
variable, instead of including the store instruction explicitly in the
``Asm`` template string:

  .. code-block:: ada

     with Interfaces; use Interfaces;
     with Ada.Text_IO; use Ada.Text_IO;
     with System.Machine_Code; use System.Machine_Code;
     procedure Get_Flags_2 is
        Flags : Unsigned_32;
        use ASCII;
     begin
        Asm ("pushfl"      & LF & HT & -- push flags on stack
             "popl %%eax",             -- save flags in eax
             Outputs => Unsigned_32'Asm_Output ("=a", Flags));
        Put_Line ("Flags register:" & Flags'Img);
     end Get_Flags_2;

The ``"a"`` constraint tells the compiler that the ``Flags``
variable will come from the eax register. Here is the resulting code:

  ::

     #APP
        pushfl
        popl %eax
     #NO_APP
        movl %eax,-40(%ebp)

The compiler generated the store of eax into Flags after
expanding the assembler code.

Actually, there was no need to pop the flags into the eax register;
more simply, we could just pop the flags directly into the program variable:

  .. code-block:: ada

     with Interfaces; use Interfaces;
     with Ada.Text_IO; use Ada.Text_IO;
     with System.Machine_Code; use System.Machine_Code;
     procedure Get_Flags_3 is
        Flags : Unsigned_32;
        use ASCII;
     begin
        Asm ("pushfl"  & LF & HT & -- push flags on stack
             "pop %0",             -- save flags in Flags
             Outputs => Unsigned_32'Asm_Output ("=g", Flags));
        Put_Line ("Flags register:" & Flags'Img);
     end Get_Flags_3;


.. _Input_Variables_in_Inline_Assembler:

Input Variables in Inline Assembler
===================================

The example in this section illustrates how to specify the source operands
for assembly language statements.
The program simply increments its input value by 1:

  .. code-block:: ada

     with Interfaces; use Interfaces;
     with Ada.Text_IO; use Ada.Text_IO;
     with System.Machine_Code; use System.Machine_Code;
     procedure Increment is

        function Incr (Value : Unsigned_32) return Unsigned_32 is
           Result : Unsigned_32;
        begin
           Asm ("incl %0",
                Outputs => Unsigned_32'Asm_Output ("=a", Result),
                Inputs  => Unsigned_32'Asm_Input ("a", Value));
           return Result;
        end Incr;

        Value : Unsigned_32;

     begin
        Value := 5;
        Put_Line ("Value before is" & Value'Img);
        Value := Incr (Value);
       Put_Line ("Value after is" & Value'Img);
     end Increment;

The ``Outputs`` parameter to ``Asm`` specifies
that the result will be in the eax register and that it is to be stored
in the ``Result`` variable.

The ``Inputs`` parameter looks much like the ``Outputs`` parameter,
but with an ``Asm_Input`` attribute.
The ``"="`` constraint, indicating an output value, is not present.

You can have multiple input variables, in the same way that you can have more
than one output variable.

The parameter count (%0, %1) etc, still starts at the first output statement,
and continues with the input statements.

Just as the ``Outputs`` parameter causes the register to be stored into the
target variable after execution of the assembler statements, so does the
``Inputs`` parameter cause its variable to be loaded into the register
before execution of the assembler statements.

Thus the effect of the ``Asm`` invocation is:

* load the 32-bit value of ``Value`` into eax
* execute the ``incl %eax`` instruction
* store the contents of eax into the ``Result`` variable

The resulting assembler file (with :switch:`-O2` optimization) contains:

  ::

     _increment__incr.1:
        subl $4,%esp
        movl 8(%esp),%eax
     #APP
        incl %eax
     #NO_APP
        movl %eax,%edx
        movl %ecx,(%esp)
        addl $4,%esp
        ret


.. _Inlining_Inline_Assembler_Code:

Inlining Inline Assembler Code
==============================

For a short subprogram such as the ``Incr`` function in the previous
section, the overhead of the call and return (creating / deleting the stack
frame) can be significant, compared to the amount of code in the subprogram
body.  A solution is to apply Ada's ``Inline`` pragma to the subprogram,
which directs the compiler to expand invocations of the subprogram at the
point(s) of call, instead of setting up a stack frame for out-of-line calls.
Here is the resulting program:

  .. code-block:: ada

     with Interfaces; use Interfaces;
     with Ada.Text_IO; use Ada.Text_IO;
     with System.Machine_Code; use System.Machine_Code;
     procedure Increment_2 is

        function Incr (Value : Unsigned_32) return Unsigned_32 is
           Result : Unsigned_32;
        begin
           Asm ("incl %0",
                Outputs => Unsigned_32'Asm_Output ("=a", Result),
                Inputs  => Unsigned_32'Asm_Input ("a", Value));
           return Result;
        end Incr;
        pragma Inline (Increment);

        Value : Unsigned_32;

     begin
        Value := 5;
        Put_Line ("Value before is" & Value'Img);
        Value := Increment (Value);
        Put_Line ("Value after is" & Value'Img);
     end Increment_2;

Compile the program with both optimization (:switch:`-O2`) and inlining
(:switch:`-gnatn`) enabled.

The ``Incr`` function is still compiled as usual, but at the
point in ``Increment`` where our function used to be called:


  ::

     pushl %edi
     call _increment__incr.1

the code for the function body directly appears:


  ::

     movl %esi,%eax
     #APP
        incl %eax
     #NO_APP
        movl %eax,%edx

thus saving the overhead of stack frame setup and an out-of-line call.


.. _Other_Asm_Functionality:

Other ``Asm`` Functionality
===========================

This section describes two important parameters to the ``Asm``
procedure: ``Clobber``, which identifies register usage;
and ``Volatile``, which inhibits unwanted optimizations.

.. _The_Clobber_Parameter:

The ``Clobber`` Parameter
-------------------------

One of the dangers of intermixing assembly language and a compiled language
such as Ada is that the compiler needs to be aware of which registers are
being used by the assembly code.  In some cases, such as the earlier examples,
the constraint string is sufficient to indicate register usage (e.g.,
``"a"`` for
the eax register).  But more generally, the compiler needs an explicit
identification of the registers that are used by the Inline Assembly
statements.

Using a register that the compiler doesn't know about
could be a side effect of an instruction (like ``mull``
storing its result in both eax and edx).
It can also arise from explicit register usage in your
assembly code; for example:

  .. code-block:: ada

     Asm ("movl %0, %%ebx" & LF & HT &
          "movl %%ebx, %1",
          Outputs => Unsigned_32'Asm_Output ("=g", Var_Out),
          Inputs  => Unsigned_32'Asm_Input  ("g", Var_In));

where the compiler (since it does not analyze the ``Asm`` template string)
does not know you are using the ebx register.

In such cases you need to supply the ``Clobber`` parameter to ``Asm``,
to identify the registers that will be used by your assembly code:


  .. code-block:: ada

     Asm ("movl %0, %%ebx" & LF & HT &
          "movl %%ebx, %1",
          Outputs => Unsigned_32'Asm_Output ("=g", Var_Out),
          Inputs  => Unsigned_32'Asm_Input  ("g", Var_In),
          Clobber => "ebx");

The Clobber parameter is a static string expression specifying the
register(s) you are using.  Note that register names are *not* prefixed
by a percent sign. Also, if more than one register is used then their names
are separated by commas; e.g., ``"eax, ebx"``

The ``Clobber`` parameter has several additional uses:

* Use 'register' name ``cc`` to indicate that flags might have changed
* Use 'register' name ``memory`` if you changed a memory location


.. _The_Volatile_Parameter:

The ``Volatile`` Parameter
--------------------------

.. index:: Volatile parameter

Compiler optimizations in the presence of Inline Assembler may sometimes have
unwanted effects.  For example, when an ``Asm`` invocation with an input
variable is inside a loop, the compiler might move the loading of the input
variable outside the loop, regarding it as a one-time initialization.

If this effect is not desired, you can disable such optimizations by setting
the ``Volatile`` parameter to ``True``; for example:

  .. code-block:: ada

     Asm ("movl %0, %%ebx" & LF & HT &
          "movl %%ebx, %1",
          Outputs  => Unsigned_32'Asm_Output ("=g", Var_Out),
          Inputs   => Unsigned_32'Asm_Input  ("g", Var_In),
          Clobber  => "ebx",
          Volatile => True);

By default, ``Volatile`` is set to ``False`` unless there is no
``Outputs`` parameter.

Although setting ``Volatile`` to ``True`` prevents unwanted
optimizations, it will also disable other optimizations that might be
important for efficiency. In general, you should set ``Volatile``
to ``True`` only if the compiler's optimizations have created
problems.
