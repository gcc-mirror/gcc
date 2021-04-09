.. Copyright (C) 2014-2021 Free Software Foundation, Inc.
   Originally contributed by David Malcolm <dmalcolm@redhat.com>

   This is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see
   <http://www.gnu.org/licenses/>.

.. default-domain:: cpp

Tutorial part 4: Adding JIT-compilation to a toy interpreter
------------------------------------------------------------
In this example we construct a "toy" interpreter, and add JIT-compilation
to it.

Our toy interpreter
*******************

It's a stack-based interpreter, and is intended as a (very simple) example
of the kind of bytecode interpreter seen in dynamic languages such as
Python, Ruby etc.

For the sake of simplicity, our toy virtual machine is very limited:

  * The only data type is `int`

  * It can only work on one function at a time (so that the only
    function call that can be made is to recurse).

  * Functions can only take one parameter.

  * Functions have a stack of `int` values.

  * We'll implement function call within the interpreter by calling a
    function in our implementation, rather than implementing our own
    frame stack.

  * The parser is only good enough to get the examples to work.

Naturally, a real interpreter would be much more complicated that this.

The following operations are supported:

====================== ======================== =============== ==============
Operation              Meaning                  Old Stack       New Stack
====================== ======================== =============== ==============
DUP                    Duplicate top of stack.  ``[..., x]``    ``[..., x, x]``
ROT                    Swap top two elements    ``[..., x, y]`` ``[..., y, x]``
                       of stack.
BINARY_ADD             Add the top two elements ``[..., x, y]`` ``[..., (x+y)]``
                       on the stack.
BINARY_SUBTRACT        Likewise, but subtract.  ``[..., x, y]`` ``[..., (x-y)]``
BINARY_MULT            Likewise, but multiply.  ``[..., x, y]`` ``[..., (x*y)]``
BINARY_COMPARE_LT      Compare the top two      ``[..., x, y]`` ``[..., (x<y)]``
                       elements on the stack
                       and push a nonzero/zero
                       if (x<y).
RECURSE                Recurse, passing the top ``[..., x]``    ``[..., fn(x)]``
                       of the stack, and
                       popping the result.
RETURN                 Return the top of the    ``[x]``         ``[]``
                       stack.
PUSH_CONST `arg`       Push an int const.       ``[...]``       ``[..., arg]``
JUMP_ABS_IF_TRUE `arg` Pop; if top of stack was ``[..., x]``    ``[...]``
                       nonzero, jump to
                       ``arg``.
====================== ======================== =============== ==============

Programs can be interpreted, disassembled, and compiled to machine code.

The interpreter reads ``.toy`` scripts.  Here's what a simple recursive
factorial program looks like, the script ``factorial.toy``.
The parser ignores lines beginning with a `#`.

   .. literalinclude:: ../../examples/tut04-toyvm/factorial.toy
    :lines: 1-
    :language: c

The interpreter is a simple infinite loop with a big ``switch`` statement
based on what the next opcode is:

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: /* Execute the given function.  */
    :end-before: /* JIT compilation.  */
    :language: c++

Compiling to machine code
*************************
We want to generate machine code that can be cast to this type and
then directly executed in-process:

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: /* Functions are compiled to this function ptr type.  */
    :end-before: enum opcode
    :language: c++

Our compiler isn't very sophisticated; it takes the implementation of
each opcode above, and maps it directly to the operations supported by
the libgccjit API.

How should we handle the stack?  In theory we could calculate what the
stack depth will be at each opcode, and optimize away the stack
manipulation "by hand".  We'll see below that libgccjit is able to do
this for us, so we'll implement stack manipulation
in a direct way, by creating a ``stack`` array and ``stack_depth``
variables, local within the generated function, equivalent to this C code:

.. code-block:: c

  int stack_depth;
  int stack[MAX_STACK_DEPTH];

We'll also have local variables ``x`` and ``y`` for use when implementing
the opcodes, equivalent to this:

.. code-block:: c

  int x;
  int y;

This means our compiler has the following state:

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after:   /* State.  */
    :end-before: };
    :language: c++

Setting things up
*****************

First we create our types:

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: /* Create types.  */
    :end-before: /* The constant value 1.  */
    :language: c++

along with extracting a useful `int` constant:

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: /* The constant value 1.  */
    :end-before: /* Create locations.  */
    :language: c++

We'll implement push and pop in terms of the ``stack`` array and
``stack_depth``.  Here are helper functions for adding statements to
a block, implementing pushing and popping values:

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: /* Stack manipulation.  */
    :end-before: /* Create the context. */
    :language: c++

We will support single-stepping through the generated code in the
debugger, so we need to create :type:`gccjit::location` instances, one
per operation in the source code.  These will reference the lines of
e.g. ``factorial.toy``.

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: /* Create locations.  */
    :end-before: /* Creating the function.  */
    :language: c++

Let's create the function itself.  As usual, we create its parameter
first, then use the parameter to create the function:

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: /* Creating the function.  */
    :end-before: /* Create stack lvalues.  */
    :language: c++

We create the locals within the function.

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: /* Create stack lvalues.  */
    :end-before: /* 1st pass: create blocks, one per opcode.
    :language: c++

Populating the function
***********************

There's some one-time initialization, and the API treats the first block
you create as the entrypoint of the function, so we need to create that
block first:

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: first.  */
    :end-before: /* Create a block per operation.  */
    :language: c++

We can now create blocks for each of the operations.  Most of these will
be consolidated into larger blocks when the optimizer runs.

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: /* Create a block per operation.  */
    :end-before: /* Populate the initial block.  */
    :language: c++

Now that we have a block it can jump to when it's done, we can populate
the initial block:

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: /* Populate the initial block.  */
    :end-before: /* 2nd pass: fill in instructions.  */
    :language: c++

We can now populate the blocks for the individual operations.  We loop
through them, adding instructions to their blocks:

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: /* 2nd pass: fill in instructions.  */
    :end-before: /* Helper macros.  */
    :language: c++

We're going to have another big ``switch`` statement for implementing
the opcodes, this time for compiling them, rather than interpreting
them.  It's helpful to have macros for implementing push and pop, so that
we can make the ``switch`` statement that's coming up look as much as
possible like the one above within the interpreter:

.. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: /* Helper macros.  */
    :end-before: block.add_comment
    :language: c++

.. note::

   A particularly clever implementation would have an *identical*
   ``switch`` statement shared by the interpreter and the compiler, with
   some preprocessor "magic".  We're not doing that here, for the sake
   of simplicity.

When I first implemented this compiler, I accidentally missed an edit
when copying and pasting the ``Y_EQUALS_POP`` macro, so that popping the
stack into ``y`` instead erroneously assigned it to ``x``, leaving ``y``
uninitialized.

To track this kind of thing down, we can use
:func:`gccjit::block::add_comment` to add descriptive comments
to the internal representation.  This is invaluable when looking through
the generated IR for, say ``factorial``:

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: PUSH_RVALUE (y)
    :end-before: /* Handle the individual opcodes.  */
    :language: c++

We can now write the big ``switch`` statement that implements the
individual opcodes, populating the relevant block with statements:

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: /* Handle the individual opcodes.  */
    :end-before: /* Go to the next block.  */
    :language: c++

Every block must be terminated, via a call to one of the
``gccjit::block::end_with_`` entrypoints.  This has been done for two
of the opcodes, but we need to do it for the other ones, by jumping
to the next block.

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: /* Go to the next block.  */
    :end-before: /* end of loop on PC locations.  */
    :language: c++

This is analogous to simply incrementing the program counter.

Verifying the control flow graph
********************************
Having finished looping over the blocks, the context is complete.

As before, we can verify that the control flow and statements are sane by
using :func:`gccjit::function::dump_to_dot`:

.. code-block:: c++

  fn.dump_to_dot ("/tmp/factorial.dot");

and viewing the result.  Note how the label names, comments, and
variable names show up in the dump, to make it easier to spot
errors in our compiler.

  .. figure:: ../../intro/factorial.png
    :alt: image of a control flow graph

Compiling the context
*********************
Having finished looping over the blocks and populating them with
statements, the context is complete.

We can now compile it, extract machine code from the result, and
run it:

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: /* Wrapper around a gcc_jit_result *.  */
    :end-before: /* Functions are compiled to this function ptr type.  */
    :language: c++

   .. literalinclude:: ../../examples/tut04-toyvm/toyvm.cc
    :start-after: /* JIT-compilation.  */
    :end-before: return 0;
    :language: c++

Single-stepping through the generated code
******************************************

It's possible to debug the generated code.  To do this we need to both:

  * Set up source code locations for our statements, so that we can
    meaningfully step through the code.  We did this above by
    calling :func:`gccjit::context::new_location` and using the
    results.

  * Enable the generation of debugging information, by setting
    :c:macro:`GCC_JIT_BOOL_OPTION_DEBUGINFO` on the
    :type:`gccjit::context` via
    :func:`gccjit::context::set_bool_option`:

    .. code-block:: c++

      ctxt.set_bool_option (GCC_JIT_BOOL_OPTION_DEBUGINFO, 1);

Having done this, we can put a breakpoint on the generated function:

.. code-block:: console

  $ gdb --args ./toyvm factorial.toy 10
  (gdb) break factorial
  Function "factorial" not defined.
  Make breakpoint pending on future shared library load? (y or [n]) y
  Breakpoint 1 (factorial) pending.
  (gdb) run
  Breakpoint 1, factorial (arg=10) at factorial.toy:14
  14	DUP

We've set up location information, which references ``factorial.toy``.
This allows us to use e.g. ``list`` to see where we are in the script:

.. code-block:: console

  (gdb) list
  9
  10    # Initial state:
  11    # stack: [arg]
  12
  13    # 0:
  14    DUP
  15    # stack: [arg, arg]
  16
  17    # 1:
  18    PUSH_CONST 2

and to step through the function, examining the data:

.. code-block:: console

  (gdb) n
  18    PUSH_CONST 2
  (gdb) n
  22    BINARY_COMPARE_LT
  (gdb) print stack
  $5 = {10, 10, 2, 0, -7152, 32767, 0, 0}
  (gdb) print stack_depth
  $6 = 3

You'll see that the parts of the ``stack`` array that haven't been
touched yet are uninitialized.

.. note::

   Turning on optimizations may lead to unpredictable results when
   stepping through the generated code: the execution may appear to
   "jump around" the source code.  This is analogous to turning up the
   optimization level in a regular compiler.

Examining the generated code
****************************

How good is the optimized code?

We can turn up optimizations, by calling
:cpp:func:`gccjit::context::set_int_option` with
:c:macro:`GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL`:

.. code-block:: c++

  ctxt.set_int_option (GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL, 3);

One of GCC's internal representations is called "gimple".  A dump of the
initial gimple representation of the code can be seen by setting:

.. code-block:: c++

  ctxt.set_bool_option (GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE, 1);

With optimization on and source locations displayed, this gives:

.. We'll use "c" for gimple dumps

.. code-block:: c

  factorial (signed int arg)
  {
    <unnamed type> D.80;
    signed int D.81;
    signed int D.82;
    signed int D.83;
    signed int D.84;
    signed int D.85;
    signed int y;
    signed int x;
    signed int stack_depth;
    signed int stack[8];

    try
      {
        initial:
        stack_depth = 0;
        stack[stack_depth] = arg;
        stack_depth = stack_depth + 1;
        goto instr0;
        instr0:
        /* DUP */:
        stack_depth = stack_depth + -1;
        x = stack[stack_depth];
        stack[stack_depth] = x;
        stack_depth = stack_depth + 1;
        stack[stack_depth] = x;
        stack_depth = stack_depth + 1;
        goto instr1;
        instr1:
        /* PUSH_CONST */:
        stack[stack_depth] = 2;
        stack_depth = stack_depth + 1;
        goto instr2;

        /* etc */

You can see the generated machine code in assembly form via:

.. code-block:: c++

  ctxt.set_bool_option (GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE, 1);
  result = ctxt.compile ();

which shows that (on this x86_64 box) the compiler has unrolled the loop
and is using MMX instructions to perform several multiplications
simultaneously:

.. code-block:: gas

          .file   "fake.c"
          .text
  .Ltext0:
          .p2align 4,,15
          .globl  factorial
          .type   factorial, @function
  factorial:
  .LFB0:
          .file 1 "factorial.toy"
          .loc 1 14 0
          .cfi_startproc
  .LVL0:
  .L2:
          .loc 1 26 0
          cmpl    $1, %edi
          jle     .L13
          leal    -1(%rdi), %edx
          movl    %edx, %ecx
          shrl    $2, %ecx
          leal    0(,%rcx,4), %esi
          testl   %esi, %esi
          je      .L14
          cmpl    $9, %edx
          jbe     .L14
          leal    -2(%rdi), %eax
          movl    %eax, -16(%rsp)
          leal    -3(%rdi), %eax
          movd    -16(%rsp), %xmm0
          movl    %edi, -16(%rsp)
          movl    %eax, -12(%rsp)
          movd    -16(%rsp), %xmm1
          xorl    %eax, %eax
          movl    %edx, -16(%rsp)
          movd    -12(%rsp), %xmm4
          movd    -16(%rsp), %xmm6
          punpckldq       %xmm4, %xmm0
          movdqa  .LC1(%rip), %xmm4
          punpckldq       %xmm6, %xmm1
          punpcklqdq      %xmm0, %xmm1
          movdqa  .LC0(%rip), %xmm0
          jmp     .L5
          # etc - edited for brevity

This is clearly overkill for a function that will likely overflow the
``int`` type before the vectorization is worthwhile - but then again, this
is a toy example.

Turning down the optimization level to 2:

.. code-block:: c++

  ctxt.set_int_option (GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL, 2);

yields this code, which is simple enough to quote in its entirety:

.. code-block:: gas

          .file   "fake.c"
          .text
          .p2align 4,,15
          .globl  factorial
          .type   factorial, @function
  factorial:
  .LFB0:
          .cfi_startproc
  .L2:
          cmpl    $1, %edi
          jle     .L8
          movl    $1, %edx
          jmp     .L4
          .p2align 4,,10
          .p2align 3
  .L6:
          movl    %eax, %edi
  .L4:
  .L5:
          leal    -1(%rdi), %eax
          imull   %edi, %edx
          cmpl    $1, %eax
          jne     .L6
  .L3:
  .L7:
          imull   %edx, %eax
          ret
  .L8:
          movl    %edi, %eax
          movl    $1, %edx
          jmp     .L7
          .cfi_endproc
  .LFE0:
          .size   factorial, .-factorial
          .ident  "GCC: (GNU) 4.9.0 20131023 (Red Hat 0.2-%{gcc_release})"
          .section        .note.GNU-stack,"",@progbits

Note that the stack pushing and popping have been eliminated, as has the
recursive call (in favor of an iteration).

Putting it all together
***********************

The complete example can be seen in the source tree at
``gcc/jit/docs/examples/tut04-toyvm/toyvm.cc``

along with a Makefile and a couple of sample .toy scripts:

.. code-block:: console

  $ ls -al
  drwxrwxr-x. 2 david david   4096 Sep 19 17:46 .
  drwxrwxr-x. 3 david david   4096 Sep 19 15:26 ..
  -rw-rw-r--. 1 david david    615 Sep 19 12:43 factorial.toy
  -rw-rw-r--. 1 david david    834 Sep 19 13:08 fibonacci.toy
  -rw-rw-r--. 1 david david    238 Sep 19 14:22 Makefile
  -rw-rw-r--. 1 david david  16457 Sep 19 17:07 toyvm.cc

  $ make toyvm
  g++ -Wall -g -o toyvm toyvm.cc -lgccjit

  $ ./toyvm factorial.toy 10
  interpreter result: 3628800
  compiler result: 3628800

  $ ./toyvm fibonacci.toy 10
  interpreter result: 55
  compiler result: 55

Behind the curtain: How does our code get optimized?
****************************************************

Our example is done, but you may be wondering about exactly how the
compiler turned what we gave it into the machine code seen above.

We can examine what the compiler is doing in detail by setting:

.. code-block:: c++

  state.ctxt.set_bool_option (GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING, 1);
  state.ctxt.set_bool_option (GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES, 1);

This will dump detailed information about the compiler's state to a
directory under ``/tmp``, and keep it from being cleaned up.

The precise names and their formats of these files is subject to change.
Higher optimization levels lead to more files.
Here's what I saw (edited for brevity; there were almost 200 files):

.. code-block:: console

  intermediate files written to /tmp/libgccjit-KPQbGw
  $ ls /tmp/libgccjit-KPQbGw/
  fake.c.000i.cgraph
  fake.c.000i.type-inheritance
  fake.c.004t.gimple
  fake.c.007t.omplower
  fake.c.008t.lower
  fake.c.011t.eh
  fake.c.012t.cfg
  fake.c.014i.visibility
  fake.c.015i.early_local_cleanups
  fake.c.016t.ssa
  # etc

The gimple code is converted into Static Single Assignment form,
with annotations for use when generating the debuginfo:

.. code-block:: console

  $ less /tmp/libgccjit-KPQbGw/fake.c.016t.ssa

.. code-block:: c

  ;; Function factorial (factorial, funcdef_no=0, decl_uid=53, symbol_order=0)

  factorial (signed int arg)
  {
    signed int stack[8];
    signed int stack_depth;
    signed int x;
    signed int y;
    <unnamed type> _20;
    signed int _21;
    signed int _38;
    signed int _44;
    signed int _51;
    signed int _56;

  initial:
    stack_depth_3 = 0;
    # DEBUG stack_depth => stack_depth_3
    stack[stack_depth_3] = arg_5(D);
    stack_depth_7 = stack_depth_3 + 1;
    # DEBUG stack_depth => stack_depth_7
    # DEBUG instr0 => NULL
    # DEBUG /* DUP */ => NULL
    stack_depth_8 = stack_depth_7 + -1;
    # DEBUG stack_depth => stack_depth_8
    x_9 = stack[stack_depth_8];
    # DEBUG x => x_9
    stack[stack_depth_8] = x_9;
    stack_depth_11 = stack_depth_8 + 1;
    # DEBUG stack_depth => stack_depth_11
    stack[stack_depth_11] = x_9;
    stack_depth_13 = stack_depth_11 + 1;
    # DEBUG stack_depth => stack_depth_13
    # DEBUG instr1 => NULL
    # DEBUG /* PUSH_CONST */ => NULL
    stack[stack_depth_13] = 2;

    /* etc; edited for brevity */

We can perhaps better see the code by turning off
:c:macro:`GCC_JIT_BOOL_OPTION_DEBUGINFO` to suppress all those ``DEBUG``
statements, giving:

.. code-block:: console

  $ less /tmp/libgccjit-1Hywc0/fake.c.016t.ssa

.. code-block:: c

  ;; Function factorial (factorial, funcdef_no=0, decl_uid=53, symbol_order=0)

  factorial (signed int arg)
  {
    signed int stack[8];
    signed int stack_depth;
    signed int x;
    signed int y;
    <unnamed type> _20;
    signed int _21;
    signed int _38;
    signed int _44;
    signed int _51;
    signed int _56;

  initial:
    stack_depth_3 = 0;
    stack[stack_depth_3] = arg_5(D);
    stack_depth_7 = stack_depth_3 + 1;
    stack_depth_8 = stack_depth_7 + -1;
    x_9 = stack[stack_depth_8];
    stack[stack_depth_8] = x_9;
    stack_depth_11 = stack_depth_8 + 1;
    stack[stack_depth_11] = x_9;
    stack_depth_13 = stack_depth_11 + 1;
    stack[stack_depth_13] = 2;
    stack_depth_15 = stack_depth_13 + 1;
    stack_depth_16 = stack_depth_15 + -1;
    y_17 = stack[stack_depth_16];
    stack_depth_18 = stack_depth_16 + -1;
    x_19 = stack[stack_depth_18];
    _20 = x_19 < y_17;
    _21 = (signed int) _20;
    stack[stack_depth_18] = _21;
    stack_depth_23 = stack_depth_18 + 1;
    stack_depth_24 = stack_depth_23 + -1;
    x_25 = stack[stack_depth_24];
    if (x_25 != 0)
      goto <bb 4> (instr9);
    else
      goto <bb 3> (instr4);

  instr4:
  /* DUP */:
    stack_depth_26 = stack_depth_24 + -1;
    x_27 = stack[stack_depth_26];
    stack[stack_depth_26] = x_27;
    stack_depth_29 = stack_depth_26 + 1;
    stack[stack_depth_29] = x_27;
    stack_depth_31 = stack_depth_29 + 1;
    stack[stack_depth_31] = 1;
    stack_depth_33 = stack_depth_31 + 1;
    stack_depth_34 = stack_depth_33 + -1;
    y_35 = stack[stack_depth_34];
    stack_depth_36 = stack_depth_34 + -1;
    x_37 = stack[stack_depth_36];
    _38 = x_37 - y_35;
    stack[stack_depth_36] = _38;
    stack_depth_40 = stack_depth_36 + 1;
    stack_depth_41 = stack_depth_40 + -1;
    x_42 = stack[stack_depth_41];
    _44 = factorial (x_42);
    stack[stack_depth_41] = _44;
    stack_depth_46 = stack_depth_41 + 1;
    stack_depth_47 = stack_depth_46 + -1;
    y_48 = stack[stack_depth_47];
    stack_depth_49 = stack_depth_47 + -1;
    x_50 = stack[stack_depth_49];
    _51 = x_50 * y_48;
    stack[stack_depth_49] = _51;
    stack_depth_53 = stack_depth_49 + 1;

    # stack_depth_1 = PHI <stack_depth_24(2), stack_depth_53(3)>
  instr9:
  /* RETURN */:
    stack_depth_54 = stack_depth_1 + -1;
    x_55 = stack[stack_depth_54];
    _56 = x_55;
    stack ={v} {CLOBBER};
    return _56;

  }

Note in the above how all the :type:`gccjit::block` instances we
created have been consolidated into just 3 blocks in GCC's internal
representation: ``initial``, ``instr4`` and ``instr9``.

Optimizing away stack manipulation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Recall our simple implementation of stack operations.  Let's examine
how the stack operations are optimized away.

After a pass of constant-propagation, the depth of the stack at each
opcode can be determined at compile-time:

.. code-block:: console

  $ less /tmp/libgccjit-1Hywc0/fake.c.021t.ccp1

.. code-block:: c

  ;; Function factorial (factorial, funcdef_no=0, decl_uid=53, symbol_order=0)

  factorial (signed int arg)
  {
    signed int stack[8];
    signed int stack_depth;
    signed int x;
    signed int y;
    <unnamed type> _20;
    signed int _21;
    signed int _38;
    signed int _44;
    signed int _51;

  initial:
    stack[0] = arg_5(D);
    x_9 = stack[0];
    stack[0] = x_9;
    stack[1] = x_9;
    stack[2] = 2;
    y_17 = stack[2];
    x_19 = stack[1];
    _20 = x_19 < y_17;
    _21 = (signed int) _20;
    stack[1] = _21;
    x_25 = stack[1];
    if (x_25 != 0)
      goto <bb 4> (instr9);
    else
      goto <bb 3> (instr4);

  instr4:
  /* DUP */:
    x_27 = stack[0];
    stack[0] = x_27;
    stack[1] = x_27;
    stack[2] = 1;
    y_35 = stack[2];
    x_37 = stack[1];
    _38 = x_37 - y_35;
    stack[1] = _38;
    x_42 = stack[1];
    _44 = factorial (x_42);
    stack[1] = _44;
    y_48 = stack[1];
    x_50 = stack[0];
    _51 = x_50 * y_48;
    stack[0] = _51;

  instr9:
  /* RETURN */:
    x_55 = stack[0];
    x_56 = x_55;
    stack ={v} {CLOBBER};
    return x_56;

  }

Note how, in the above, all those ``stack_depth`` values are now just
constants: we're accessing specific stack locations at each opcode.

The "esra" pass ("Early Scalar Replacement of Aggregates") breaks
out our "stack" array into individual elements:

.. code-block:: console

  $ less /tmp/libgccjit-1Hywc0/fake.c.024t.esra

.. code-block:: c

  ;; Function factorial (factorial, funcdef_no=0, decl_uid=53, symbol_order=0)

  Created a replacement for stack offset: 0, size: 32: stack$0
  Created a replacement for stack offset: 32, size: 32: stack$1
  Created a replacement for stack offset: 64, size: 32: stack$2

  Symbols to be put in SSA form
  { D.89 D.90 D.91 }
  Incremental SSA update started at block: 0
  Number of blocks in CFG: 5
  Number of blocks to update: 4 ( 80%)


  factorial (signed int arg)
  {
    signed int stack$2;
    signed int stack$1;
    signed int stack$0;
    signed int stack[8];
    signed int stack_depth;
    signed int x;
    signed int y;
    <unnamed type> _20;
    signed int _21;
    signed int _38;
    signed int _44;
    signed int _51;

  initial:
    stack$0_45 = arg_5(D);
    x_9 = stack$0_45;
    stack$0_39 = x_9;
    stack$1_32 = x_9;
    stack$2_30 = 2;
    y_17 = stack$2_30;
    x_19 = stack$1_32;
    _20 = x_19 < y_17;
    _21 = (signed int) _20;
    stack$1_28 = _21;
    x_25 = stack$1_28;
    if (x_25 != 0)
      goto <bb 4> (instr9);
    else
      goto <bb 3> (instr4);

  instr4:
  /* DUP */:
    x_27 = stack$0_39;
    stack$0_22 = x_27;
    stack$1_14 = x_27;
    stack$2_12 = 1;
    y_35 = stack$2_12;
    x_37 = stack$1_14;
    _38 = x_37 - y_35;
    stack$1_10 = _38;
    x_42 = stack$1_10;
    _44 = factorial (x_42);
    stack$1_6 = _44;
    y_48 = stack$1_6;
    x_50 = stack$0_22;
    _51 = x_50 * y_48;
    stack$0_1 = _51;

    # stack$0_52 = PHI <stack$0_39(2), stack$0_1(3)>
  instr9:
  /* RETURN */:
    x_55 = stack$0_52;
    x_56 = x_55;
    stack ={v} {CLOBBER};
    return x_56;

  }

Hence at this point, all those pushes and pops of the stack are now
simply assignments to specific temporary variables.

After some copy propagation, the stack manipulation has been completely
optimized away:

.. code-block:: console

  $ less /tmp/libgccjit-1Hywc0/fake.c.026t.copyprop1

.. code-block:: c

  ;; Function factorial (factorial, funcdef_no=0, decl_uid=53, symbol_order=0)

  factorial (signed int arg)
  {
    signed int stack$2;
    signed int stack$1;
    signed int stack$0;
    signed int stack[8];
    signed int stack_depth;
    signed int x;
    signed int y;
    <unnamed type> _20;
    signed int _21;
    signed int _38;
    signed int _44;
    signed int _51;

  initial:
    stack$0_39 = arg_5(D);
    _20 = arg_5(D) <= 1;
    _21 = (signed int) _20;
    if (_21 != 0)
      goto <bb 4> (instr9);
    else
      goto <bb 3> (instr4);

  instr4:
  /* DUP */:
    _38 = arg_5(D) + -1;
    _44 = factorial (_38);
    _51 = arg_5(D) * _44;
    stack$0_1 = _51;

    # stack$0_52 = PHI <arg_5(D)(2), _51(3)>
  instr9:
  /* RETURN */:
    stack ={v} {CLOBBER};
    return stack$0_52;

  }

Later on, another pass finally eliminated ``stack_depth`` local and the
unused parts of the `stack`` array altogether:

.. code-block:: console

  $ less /tmp/libgccjit-1Hywc0/fake.c.036t.release_ssa

.. code-block:: c

  ;; Function factorial (factorial, funcdef_no=0, decl_uid=53, symbol_order=0)

  Released 44 names, 314.29%, removed 44 holes
  factorial (signed int arg)
  {
    signed int stack$0;
    signed int mult_acc_1;
    <unnamed type> _5;
    signed int _6;
    signed int _7;
    signed int mul_tmp_10;
    signed int mult_acc_11;
    signed int mult_acc_13;

    # arg_9 = PHI <arg_8(D)(0)>
    # mult_acc_13 = PHI <1(0)>
  initial:

    <bb 5>:
    # arg_4 = PHI <arg_9(2), _7(3)>
    # mult_acc_1 = PHI <mult_acc_13(2), mult_acc_11(3)>
    _5 = arg_4 <= 1;
    _6 = (signed int) _5;
    if (_6 != 0)
      goto <bb 4> (instr9);
    else
      goto <bb 3> (instr4);

  instr4:
  /* DUP */:
    _7 = arg_4 + -1;
    mult_acc_11 = mult_acc_1 * arg_4;
    goto <bb 5>;

    # stack$0_12 = PHI <arg_4(5)>
  instr9:
  /* RETURN */:
    mul_tmp_10 = mult_acc_1 * stack$0_12;
    return mul_tmp_10;

  }


Elimination of tail recursion
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Another significant optimization is the detection that the call to
``factorial`` is tail recursion, which can be eliminated in favor of
an iteration:

.. code-block:: console

  $ less /tmp/libgccjit-1Hywc0/fake.c.030t.tailr1

.. code-block:: c

  ;; Function factorial (factorial, funcdef_no=0, decl_uid=53, symbol_order=0)


  Symbols to be put in SSA form
  { D.88 }
  Incremental SSA update started at block: 0
  Number of blocks in CFG: 5
  Number of blocks to update: 4 ( 80%)


  factorial (signed int arg)
  {
    signed int stack$2;
    signed int stack$1;
    signed int stack$0;
    signed int stack[8];
    signed int stack_depth;
    signed int x;
    signed int y;
    signed int mult_acc_1;
    <unnamed type> _20;
    signed int _21;
    signed int _38;
    signed int mul_tmp_44;
    signed int mult_acc_51;

    # arg_5 = PHI <arg_39(D)(0), _38(3)>
    # mult_acc_1 = PHI <1(0), mult_acc_51(3)>
  initial:
    _20 = arg_5 <= 1;
    _21 = (signed int) _20;
    if (_21 != 0)
      goto <bb 4> (instr9);
    else
      goto <bb 3> (instr4);

  instr4:
  /* DUP */:
    _38 = arg_5 + -1;
    mult_acc_51 = mult_acc_1 * arg_5;
    goto <bb 2> (initial);

    # stack$0_52 = PHI <arg_5(2)>
  instr9:
  /* RETURN */:
    stack ={v} {CLOBBER};
    mul_tmp_44 = mult_acc_1 * stack$0_52;
    return mul_tmp_44;

  }
