..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: code generation conventions, options, code generation, run-time options

.. _code-gen-options:

Options for Code Generation Conventions
***************************************

These machine-independent options control the interface conventions
used in code generation.

Most of them have both positive and negative forms; the negative form
of :samp:`-ffoo` is :samp:`-fno-foo`.  In the table below, only
one of the forms is listed---the one that is not the default.  You
can figure out the other form by either removing :samp:`no-` or adding
it.

.. option:: -fstack-reuse={reuse-level}

  This option controls stack space reuse for user declared local/auto variables
  and compiler generated temporaries.  :samp:`{reuse_level}` can be :samp:`all`,
  :samp:`named_vars`, or :samp:`none`. :samp:`all` enables stack reuse for all
  local variables and temporaries, :samp:`named_vars` enables the reuse only for
  user defined local variables with names, and :samp:`none` disables stack reuse
  completely. The default value is :samp:`all`. The option is needed when the
  program extends the lifetime of a scoped local variable or a compiler generated
  temporary beyond the end point defined by the language.  When a lifetime of
  a variable ends, and if the variable lives in memory, the optimizing compiler
  has the freedom to reuse its stack space with other temporaries or scoped
  local variables whose live range does not overlap with it. Legacy code extending
  local lifetime is likely to break with the stack reuse optimization.

  For example,

  .. code-block:: c++

       int *p;
       {
         int local1;

         p = &local1;
         local1 = 10;
         ....
       }
       {
          int local2;
          local2 = 20;
          ...
       }

       if (*p == 10)  // out of scope use of local1
         {

         }

  Another example:

  .. code-block:: c++

       struct A
       {
           A(int k) : i(k), j(k) { }
           int i;
           int j;
       };

       A *ap;

       void foo(const A& ar)
       {
          ap = &ar;
       }

       void bar()
       {
          foo(A(10)); // temp object's lifetime ends when foo returns

          {
            A a(20);
            ....
          }
          ap->i+= 10;  // ap references out of scope temp whose space
                       // is reused with a. What is the value of ap->i?
       }

  The lifetime of a compiler generated temporary is well defined by the C++
  standard. When a lifetime of a temporary ends, and if the temporary lives
  in memory, the optimizing compiler has the freedom to reuse its stack
  space with other temporaries or scoped local variables whose live range
  does not overlap with it. However some of the legacy code relies on
  the behavior of older compilers in which temporaries' stack space is
  not reused, the aggressive stack reuse can lead to runtime errors. This
  option is used to control the temporary stack reuse optimization.

.. option:: -ftrapv

  This option generates traps for signed overflow on addition, subtraction,
  multiplication operations.
  The options :option:`-ftrapv` and :option:`-fwrapv` override each other, so using
  :option:`-ftrapv` :option:`-fwrapv` on the command-line results in
  :option:`-fwrapv` being effective.  Note that only active options override, so
  using :option:`-ftrapv` :option:`-fwrapv` :option:`-fno-wrapv` on the command-line
  results in :option:`-ftrapv` being effective.

.. option:: -fwrapv

  This option instructs the compiler to assume that signed arithmetic
  overflow of addition, subtraction and multiplication wraps around
  using twos-complement representation.  This flag enables some optimizations
  and disables others.
  The options :option:`-ftrapv` and :option:`-fwrapv` override each other, so using
  :option:`-ftrapv` :option:`-fwrapv` on the command-line results in
  :option:`-fwrapv` being effective.  Note that only active options override, so
  using :option:`-ftrapv` :option:`-fwrapv` :option:`-fno-wrapv` on the command-line
  results in :option:`-ftrapv` being effective.

.. option:: -fwrapv-pointer

  This option instructs the compiler to assume that pointer arithmetic
  overflow on addition and subtraction wraps around using twos-complement
  representation.  This flag disables some optimizations which assume
  pointer overflow is invalid.

.. option:: -fstrict-overflow

  This option implies :option:`-fno-wrapv` :option:`-fno-wrapv-pointer` and when
  negated implies :option:`-fwrapv` :option:`-fwrapv-pointer`.

.. option:: -fexceptions

  Enable exception handling.  Generates extra code needed to propagate
  exceptions.  For some targets, this implies GCC generates frame
  unwind information for all functions, which can produce significant data
  size overhead, although it does not affect execution.  If you do not
  specify this option, GCC enables it by default for languages like
  C++ that normally require exception handling, and disables it for
  languages like C that do not normally require it.  However, you may need
  to enable this option when compiling C code that needs to interoperate
  properly with exception handlers written in C++.  You may also wish to
  disable this option if you are compiling older C++ programs that don't
  use exception handling.

.. option:: -fnon-call-exceptions

  Generate code that allows trapping instructions to throw exceptions.
  Note that this requires platform-specific runtime support that does
  not exist everywhere.  Moreover, it only allows *trapping*
  instructions to throw exceptions, i.e. memory references or floating-point
  instructions.  It does not allow exceptions to be thrown from
  arbitrary signal handlers such as ``SIGALRM``.  This enables
  :option:`-fexceptions`.

.. option:: -fdelete-dead-exceptions

  Consider that instructions that may throw exceptions but don't otherwise
  contribute to the execution of the program can be optimized away.
  This does not affect calls to functions except those with the
  :fn-attr:`pure` or :fn-attr:`const` attributes.
  This option is enabled by default for the Ada and C++ compilers, as permitted by
  the language specifications.
  Optimization passes that cause dead exceptions to be removed are enabled independently at different optimization levels.

.. option:: -funwind-tables

  Similar to :option:`-fexceptions`, except that it just generates any needed
  static data, but does not affect the generated code in any other way.
  You normally do not need to enable this option; instead, a language processor
  that needs this handling enables it on your behalf.

.. option:: -fasynchronous-unwind-tables

  Generate unwind table in DWARF format, if supported by target machine.  The
  table is exact at each instruction boundary, so it can be used for stack
  unwinding from asynchronous events (such as debugger or garbage collector).

.. option:: -fno-gnu-unique

  On systems with recent GNU assembler and C library, the C++ compiler
  uses the ``STB_GNU_UNIQUE`` binding to make sure that definitions
  of template static data members and static local variables in inline
  functions are unique even in the presence of ``RTLD_LOCAL`` ; this
  is necessary to avoid problems with a library used by two different
  ``RTLD_LOCAL`` plugins depending on a definition in one of them and
  therefore disagreeing with the other one about the binding of the
  symbol.  But this causes ``dlclose`` to be ignored for affected
  DSOs; if your program relies on reinitialization of a DSO via
  ``dlclose`` and ``dlopen``, you can use
  :option:`-fno-gnu-unique`.

.. option:: -fgnu-unique

  Default setting; overrides :option:`-fno-gnu-unique`.

.. option:: -fpcc-struct-return

  Return 'short' ``struct`` and ``union`` values in memory like
  longer ones, rather than in registers.  This convention is less
  efficient, but it has the advantage of allowing intercallability between
  GCC-compiled files and files compiled with other compilers, particularly
  the Portable C Compiler (pcc).

  The precise convention for returning structures in memory depends
  on the target configuration macros.

  Short structures and unions are those whose size and alignment match
  that of some integer type.

  .. warning::

    Code compiled with the :option:`-fpcc-struct-return`
    switch is not binary compatible with code compiled with the
    :option:`-freg-struct-return` switch.
    Use it to conform to a non-default application binary interface.

.. option:: -freg-struct-return

  Return ``struct`` and ``union`` values in registers when possible.
  This is more efficient for small structures than
  :option:`-fpcc-struct-return`.

  If you specify neither :option:`-fpcc-struct-return` nor
  :option:`-freg-struct-return`, GCC defaults to whichever convention is
  standard for the target.  If there is no standard convention, GCC
  defaults to :option:`-fpcc-struct-return`, except on targets where GCC is
  the principal compiler.  In those cases, we can choose the standard, and
  we chose the more efficient register return alternative.

  .. warning::

    Code compiled with the :option:`-freg-struct-return`
    switch is not binary compatible with code compiled with the
    :option:`-fpcc-struct-return` switch.
    Use it to conform to a non-default application binary interface.

.. option:: -fshort-enums

  Allocate to an ``enum`` type only as many bytes as it needs for the
  declared range of possible values.  Specifically, the ``enum`` type
  is equivalent to the smallest integer type that has enough room.

  .. warning::

    The :option:`-fshort-enums` switch causes GCC to generate
    code that is not binary compatible with code generated without that switch.
    Use it to conform to a non-default application binary interface.

.. option:: -fshort-wchar

  Override the underlying type for ``wchar_t`` to be ``short
  unsigned int`` instead of the default for the target.  This option is
  useful for building programs to run under WINE.

  .. warning::

    The :option:`-fshort-wchar` switch causes GCC to generate
    code that is not binary compatible with code generated without that switch.
    Use it to conform to a non-default application binary interface.

.. index:: tentative definitions

.. option:: -fcommon

  In C code, this option controls the placement of global variables
  defined without an initializer, known as :dfn:`tentative definitions`
  in the C standard.  Tentative definitions are distinct from declarations
  of a variable with the ``extern`` keyword, which do not allocate storage.

  The default is :option:`-fno-common`, which specifies that the compiler places
  uninitialized global variables in the BSS section of the object file.
  This inhibits the merging of tentative definitions by the linker so you get a
  multiple-definition error if the same variable is accidentally defined in more
  than one compilation unit.

  The :option:`-fcommon` places uninitialized global variables in a common block.
  This allows the linker to resolve all tentative definitions of the same variable
  in different compilation units to the same object, or to a non-tentative
  definition.  This behavior is inconsistent with C++, and on many targets implies
  a speed and code size penalty on global variable references.  It is mainly
  useful to enable legacy code to link without errors.

.. option:: -fno-common

  Default setting; overrides :option:`-fcommon`.

.. option:: -fno-ident

  Ignore the ``#ident`` directive.

.. option:: -fident

  Default setting; overrides :option:`-fno-ident`.

.. option:: -finhibit-size-directive

  Don't output a ``.size`` assembler directive, or anything else that
  would cause trouble if the function is split in the middle, and the
  two halves are placed at locations far apart in memory.  This option is
  used when compiling :samp:`crtstuff.c`; you should not need to use it
  for anything else.

.. option:: -fverbose-asm

  Put extra commentary information in the generated assembly code to
  make it more readable.  This option is generally only of use to those
  who actually need to read the generated assembly code (perhaps while
  debugging the compiler itself).

  :option:`-fno-verbose-asm`, the default, causes the
  extra information to be omitted and is useful when comparing two assembler
  files.

  The added comments include:

  * information on the compiler version and command-line options,

  * the source code lines associated with the assembly instructions,
    in the form FILENAME:LINENUMBER:CONTENT OF LINE,

  * hints on which high-level expressions correspond to
    the various assembly instruction operands.

  For example, given this C source file:

  .. code-block:: c++

    int test (int n)
    {
      int i;
      int total = 0;

      for (i = 0; i < n; i++)
        total += i * i;

      return total;
    }

  compiling to (x86_64) assembly via :option:`-S` and emitting the result
  direct to stdout via :option:`-o` :option:`-`

  .. code-block:: shell

    gcc -S test.c -fverbose-asm -Os -o -

  gives output similar to this:

  .. code-block:: gas

    	.file	"test.c"
    # GNU C11 (GCC) version 7.0.0 20160809 (experimental) (x86_64-pc-linux-gnu)
    # [...snip...]
    # options passed:
    # [...snip...]

    	.text
    	.globl	test
    	.type	test, @function
    test:
    .LFB0:
    	.cfi_startproc
    # test.c:4:   int total = 0;
    	xorl	%eax, %eax	# <retval>
    # test.c:6:   for (i = 0; i < n; i++)
    	xorl	%edx, %edx	# i
    .L2:
    # test.c:6:   for (i = 0; i < n; i++)
    	cmpl	%edi, %edx	# n, i
    	jge	.L5	#,
    # test.c:7:     total += i * i;
    	movl	%edx, %ecx	# i, tmp92
    	imull	%edx, %ecx	# i, tmp92
    # test.c:6:   for (i = 0; i < n; i++)
    	incl	%edx	# i
    # test.c:7:     total += i * i;
    	addl	%ecx, %eax	# tmp92, <retval>
    	jmp	.L2	#
    .L5:
    # test.c:10: }
    	ret
    	.cfi_endproc
    .LFE0:
    	.size	test, .-test
    	.ident	"GCC: (GNU) 7.0.0 20160809 (experimental)"
    	.section	.note.GNU-stack,"",@progbits

  The comments are intended for humans rather than machines and hence the
  precise format of the comments is subject to change.

.. option:: -frecord-gcc-switches

  This switch causes the command line used to invoke the
  compiler to be recorded into the object file that is being created.
  This switch is only implemented on some targets and the exact format
  of the recording is target and binary file format dependent, but it
  usually takes the form of a section containing ASCII text.  This
  switch is related to the :option:`-fverbose-asm` switch, but that
  switch only records information in the assembler output file as
  comments, so it never reaches the object file.
  See also :option:`-grecord-gcc-switches` for another
  way of storing compiler options into the object file.

.. index:: global offset table, PIC

.. option:: -fpic

  Generate position-independent code (PIC) suitable for use in a shared
  library, if supported for the target machine.  Such code accesses all
  constant addresses through a global offset table (GOT).  The dynamic
  loader resolves the GOT entries when the program starts (the dynamic
  loader is not part of GCC; it is part of the operating system).  If
  the GOT size for the linked executable exceeds a machine-specific
  maximum size, you get an error message from the linker indicating that
  :option:`-fpic` does not work; in that case, recompile with :option:`-fPIC`
  instead.  (These maximums are 8k on the SPARC, 28k on AArch64 and 32k
  on the m68k and RS/6000.  The x86 has no such limit.)

  Position-independent code requires special support, and therefore works
  only on certain machines.  For the x86, GCC supports PIC for System V
  but not for the Sun 386i.  Code generated for the IBM RS/6000 is always
  position-independent.

  When this flag is set, the macros ``__pic__`` and ``__PIC__``
  are defined to 1.

.. option:: -fPIC

  If supported for the target machine, emit position-independent code,
  suitable for dynamic linking and avoiding any limit on the size of the
  global offset table.  This option makes a difference on AArch64, m68k,
  PowerPC and SPARC.

  Position-independent code requires special support, and therefore works
  only on certain machines.

  When this flag is set, the macros ``__pic__`` and ``__PIC__``
  are defined to 2.

.. option:: -fpie, -fPIE

  These options are similar to :option:`-fpic` and :option:`-fPIC`, but the
  generated position-independent code can be only linked into executables.
  Usually these options are used to compile code that will be linked using
  the :option:`-pie` GCC option.

  :option:`-fpie` and :option:`-fPIE` both define the macros
  ``__pie__`` and ``__PIE__``.  The macros have the value 1
  for :option:`-fpie` and 2 for :option:`-fPIE`.

.. option:: -fno-plt

  Do not use the PLT for external function calls in position-independent code.
  Instead, load the callee address at call sites from the GOT and branch to it.
  This leads to more efficient code by eliminating PLT stubs and exposing
  GOT loads to optimizations.  On architectures such as 32-bit x86 where
  PLT stubs expect the GOT pointer in a specific register, this gives more
  register allocation freedom to the compiler.
  Lazy binding requires use of the PLT;
  with :option:`-fno-plt` all external symbols are resolved at load time.

  Alternatively, the function attribute :fn-attr:`noplt` can be used to avoid calls
  through the PLT for specific external functions.

  In position-dependent code, a few targets also convert calls to
  functions that are marked to not use the PLT to use the GOT instead.

.. option:: -fplt

  Default setting; overrides :option:`-fno-plt`.

.. option:: -fno-jump-tables

  Do not use jump tables for switch statements even where it would be
  more efficient than other code generation strategies.  This option is
  of use in conjunction with :option:`-fpic` or :option:`-fPIC` for
  building code that forms part of a dynamic linker and cannot
  reference the address of a jump table.  On some targets, jump tables
  do not require a GOT and this option is not needed.

.. option:: -fjump-tables

  Default setting; overrides :option:`-fno-jump-tables`.

.. option:: -fno-bit-tests

  Do not use bit tests for switch statements even where it would be
  more efficient than other code generation strategies.

.. option:: -fbit-tests

  Default setting; overrides :option:`-fno-bit-tests`.

.. option:: -ffixed-reg

  Treat the register named :samp:`{reg}` as a fixed register; generated code
  should never refer to it (except perhaps as a stack pointer, frame
  pointer or in some other fixed role).

  :samp:`{reg}` must be the name of a register.  The register names accepted
  are machine-specific and are defined in the ``REGISTER_NAMES``
  macro in the machine description macro file.

  This flag does not have a negative form, because it specifies a
  three-way choice.

.. option:: -fcall-used-reg

  Treat the register named :samp:`{reg}` as an allocable register that is
  clobbered by function calls.  It may be allocated for temporaries or
  variables that do not live across a call.  Functions compiled this way
  do not save and restore the register :samp:`{reg}`.

  It is an error to use this flag with the frame pointer or stack pointer.
  Use of this flag for other registers that have fixed pervasive roles in
  the machine's execution model produces disastrous results.

  This flag does not have a negative form, because it specifies a
  three-way choice.

.. option:: -fcall-saved-reg

  Treat the register named :samp:`{reg}` as an allocable register saved by
  functions.  It may be allocated even for temporaries or variables that
  live across a call.  Functions compiled this way save and restore
  the register :samp:`{reg}` if they use it.

  It is an error to use this flag with the frame pointer or stack pointer.
  Use of this flag for other registers that have fixed pervasive roles in
  the machine's execution model produces disastrous results.

  A different sort of disaster results from the use of this flag for
  a register in which function values may be returned.

  This flag does not have a negative form, because it specifies a
  three-way choice.

.. option:: -fpack-struct[={n}]

  Without a value specified, pack all structure members together without
  holes.  When a value is specified (which must be a small power of two), pack
  structure members according to this value, representing the maximum
  alignment (that is, objects with default alignment requirements larger than
  this are output potentially unaligned at the next fitting location.

  .. warning::

    The :option:`-fpack-struct` switch causes GCC to generate
    code that is not binary compatible with code generated without that switch.
    Additionally, it makes the code suboptimal.
    Use it to conform to a non-default application binary interface.

.. option:: -fleading-underscore

  This option and its counterpart, :option:`-fno-leading-underscore`, forcibly
  change the way C symbols are represented in the object file.  One use
  is to help link with legacy assembly code.

  .. warning::

    The :option:`-fleading-underscore` switch causes GCC to
    generate code that is not binary compatible with code generated without that
    switch.  Use it to conform to a non-default application binary interface.
    Not all targets provide complete support for this switch.

.. option:: -ftls-model={model}

  Alter the thread-local storage model to be used (see :ref:`thread-local`).
  The :samp:`{model}` argument should be one of :samp:`global-dynamic`,
  :samp:`local-dynamic`, :samp:`initial-exec` or :samp:`local-exec`.
  Note that the choice is subject to optimization: the compiler may use
  a more efficient model for symbols not visible outside of the translation
  unit, or if :option:`-fpic` is not given on the command line.

  The default without :option:`-fpic` is :samp:`initial-exec`; with
  :option:`-fpic` the default is :samp:`global-dynamic`.

.. option:: -ftrampolines

  For targets that normally need trampolines for nested functions, always
  generate them instead of using descriptors.  Otherwise, for targets that
  do not need them, like for example HP-PA or IA-64, do nothing.

  A trampoline is a small piece of code that is created at run time on the
  stack when the address of a nested function is taken, and is used to call
  the nested function indirectly.  Therefore, it requires the stack to be
  made executable in order for the program to work properly.

  :option:`-fno-trampolines` is enabled by default on a language by language
  basis to let the compiler avoid generating them, if it computes that this
  is safe, and replace them with descriptors.  Descriptors are made up of data
  only, but the generated code must be prepared to deal with them.  As of this
  writing, :option:`-fno-trampolines` is enabled by default only for Ada.

  Moreover, code compiled with :option:`-ftrampolines` and code compiled with
  :option:`-fno-trampolines` are not binary compatible if nested functions are
  present.  This option must therefore be used on a program-wide basis and be
  manipulated with extreme care.

  For languages other than Ada, the ``-ftrampolines`` and
  ``-fno-trampolines`` options currently have no effect, and
  trampolines are always generated on platforms that need them
  for nested functions.

.. option:: -fvisibility=[default|internal|hidden|protected]

  Set the default ELF image symbol visibility to the specified option---all
  symbols are marked with this unless overridden within the code.
  Using this feature can very substantially improve linking and
  load times of shared object libraries, produce more optimized
  code, provide near-perfect API export and prevent symbol clashes.
  It is **strongly** recommended that you use this in any shared objects
  you distribute.

  Despite the nomenclature, :samp:`default` always means public; i.e.,
  available to be linked against from outside the shared object.
  :samp:`protected` and :samp:`internal` are pretty useless in real-world
  usage so the only other commonly used option is :samp:`hidden`.
  The default if :option:`-fvisibility` isn't specified is
  :samp:`default`, i.e., make every symbol public.

  A good explanation of the benefits offered by ensuring ELF
  symbols have the correct visibility is given by 'How To Write
  Shared Libraries' by Ulrich Drepper (which can be found at
  https://www.akkadia.org/drepper/) --- however a superior
  solution made possible by this option to marking things hidden when
  the default is public is to make the default hidden and mark things
  public.  This is the norm with DLLs on Windows and with :option:`-fvisibility=hidden`
  and ``__attribute__ ((visibility("default")))`` instead of
  ``__declspec(dllexport)`` you get almost identical semantics with
  identical syntax.  This is a great boon to those working with
  cross-platform projects.

  For those adding visibility support to existing code, you may find
  ``#pragma GCC visibility`` of use.  This works by you enclosing
  the declarations you wish to set visibility for with (for example)
  ``#pragma GCC visibility push(hidden)`` and
  ``#pragma GCC visibility pop``.
  Bear in mind that symbol visibility should be viewed **as
  part of the API interface contract** and thus all new code should
  always specify visibility when it is not the default; i.e., declarations
  only for use within the local DSO should **always** be marked explicitly
  as hidden as so to avoid PLT indirection overheads---making this
  abundantly clear also aids readability and self-documentation of the code.
  Note that due to ISO C++ specification requirements, ``operator new`` and
  ``operator delete`` must always be of default visibility.

  Be aware that headers from outside your project, in particular system
  headers and headers from any other library you use, may not be
  expecting to be compiled with visibility other than the default.  You
  may need to explicitly say ``#pragma GCC visibility push(default)``
  before including any such headers.

  ``extern`` declarations are not affected by :option:`-fvisibility`, so
  a lot of code can be recompiled with :option:`-fvisibility=hidden` with
  no modifications.  However, this means that calls to ``extern``
  functions with no explicit visibility use the PLT, so it is more
  effective to use ``__attribute ((visibility))`` and/or
  ``#pragma GCC visibility`` to tell the compiler which ``extern``
  declarations should be treated as hidden.

  Note that :option:`-fvisibility` does affect C++ vague linkage
  entities. This means that, for instance, an exception class that is
  be thrown between DSOs must be explicitly marked with default
  visibility so that the :samp:`type_info` nodes are unified between
  the DSOs.

  An overview of these techniques, their benefits and how to use them
  is at https://gcc.gnu.org/wiki/Visibility.

.. option:: -fstrict-volatile-bitfields

  This option should be used if accesses to volatile bit-fields (or other
  structure fields, although the compiler usually honors those types
  anyway) should use a single access of the width of the
  field's type, aligned to a natural alignment if possible.  For
  example, targets with memory-mapped peripheral registers might require
  all such accesses to be 16 bits wide; with this flag you can
  declare all peripheral bit-fields as ``unsigned short`` (assuming short
  is 16 bits on these targets) to force GCC to use 16-bit accesses
  instead of, perhaps, a more efficient 32-bit access.

  If this option is disabled, the compiler uses the most efficient
  instruction.  In the previous example, that might be a 32-bit load
  instruction, even though that accesses bytes that do not contain
  any portion of the bit-field, or memory-mapped registers unrelated to
  the one being updated.

  In some cases, such as when the :var-attr:`packed` attribute is applied to a
  structure field, it may not be possible to access the field with a single
  read or write that is correctly aligned for the target machine.  In this
  case GCC falls back to generating multiple accesses rather than code that
  will fault or truncate the result at run time.

  .. note::

    Due to restrictions of the C/C++11 memory model, write accesses are
    not allowed to touch non bit-field members.  It is therefore recommended
    to define all bits of the field's type as bit-field members.

  The default value of this option is determined by the application binary
  interface for the target processor.

.. option:: -fsync-libcalls

  This option controls whether any out-of-line instance of the ``__sync``
  family of functions may be used to implement the C++11 ``__atomic``
  family of functions.

  The default value of this option is enabled, thus the only useful form
  of the option is :option:`-fno-sync-libcalls`.  This option is used in
  the implementation of the :samp:`libatomic` runtime library.
