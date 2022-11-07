..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: function entry and exit, prologue, epilogue

.. _function-entry:

Function Entry and Exit
^^^^^^^^^^^^^^^^^^^^^^^

This section describes the macros that output function entry
(:dfn:`prologue`) and exit (:dfn:`epilogue`) code.

.. function:: void TARGET_ASM_PRINT_PATCHABLE_FUNCTION_ENTRY (FILE *file, unsigned HOST_WIDE_INT patch_area_size, bool record_p)

  .. hook-start:TARGET_ASM_PRINT_PATCHABLE_FUNCTION_ENTRY

  Generate a patchable area at the function start, consisting of
  :samp:`{patch_area_size}` NOP instructions.  If the target supports named
  sections and if :samp:`{record_p}` is true, insert a pointer to the current
  location in the table of patchable functions.  The default implementation
  of the hook places the table of pointers in the special section named
  ``__patchable_function_entries``.

.. hook-end

.. function:: void TARGET_ASM_FUNCTION_PROLOGUE (FILE *file)

  .. hook-start:TARGET_ASM_FUNCTION_PROLOGUE

  If defined, a function that outputs the assembler code for entry to a
  function.  The prologue is responsible for setting up the stack frame,
  initializing the frame pointer register, saving registers that must be
  saved, and allocating :samp:`{size}` additional bytes of storage for the
  local variables.  :samp:`{file}` is a stdio stream to which the assembler
  code should be output.

  The label for the beginning of the function need not be output by this
  macro.  That has already been done when the macro is run.

  .. index:: regs_ever_live

  To determine which registers to save, the macro can refer to the array
  ``regs_ever_live`` : element :samp:`{r}` is nonzero if hard register
  :samp:`{r}` is used anywhere within the function.  This implies the function
  prologue should save register :samp:`{r}`, provided it is not one of the
  call-used registers.  (``TARGET_ASM_FUNCTION_EPILOGUE`` must likewise use
  ``regs_ever_live``.)

  On machines that have 'register windows', the function entry code does
  not save on the stack the registers that are in the windows, even if
  they are supposed to be preserved by function calls; instead it takes
  appropriate steps to 'push' the register stack, if any non-call-used
  registers are used in the function.

  .. index:: frame_pointer_needed

  On machines where functions may or may not have frame-pointers, the
  function entry code must vary accordingly; it must set up the frame
  pointer if one is wanted, and not otherwise.  To determine whether a
  frame pointer is in wanted, the macro can refer to the variable
  ``frame_pointer_needed``.  The variable's value will be 1 at run
  time in a function that needs a frame pointer.  See :ref:`elimination`.

  The function entry code is responsible for allocating any stack space
  required for the function.  This stack space consists of the regions
  listed below.  In most cases, these regions are allocated in the
  order listed, with the last listed region closest to the top of the
  stack (the lowest address if ``STACK_GROWS_DOWNWARD`` is defined, and
  the highest address if it is not defined).  You can use a different order
  for a machine if doing so is more convenient or required for
  compatibility reasons.  Except in cases where required by standard
  or by a debugger, there is no reason why the stack layout used by GCC
  need agree with that used by other compilers for a machine.

.. hook-end

.. function:: void TARGET_ASM_FUNCTION_END_PROLOGUE (FILE *file)

  .. hook-start:TARGET_ASM_FUNCTION_END_PROLOGUE

  If defined, a function that outputs assembler code at the end of a
  prologue.  This should be used when the function prologue is being
  emitted as RTL, and you have some extra assembler that needs to be
  emitted.  See :ref:`prologue-instruction-pattern`.

.. hook-end

.. function:: void TARGET_ASM_FUNCTION_BEGIN_EPILOGUE (FILE *file)

  .. hook-start:TARGET_ASM_FUNCTION_BEGIN_EPILOGUE

  If defined, a function that outputs assembler code at the start of an
  epilogue.  This should be used when the function epilogue is being
  emitted as RTL, and you have some extra assembler that needs to be
  emitted.  See :ref:`epilogue-instruction-pattern`.

.. hook-end

.. function:: void TARGET_ASM_FUNCTION_EPILOGUE (FILE *file)

  .. hook-start:TARGET_ASM_FUNCTION_EPILOGUE

  If defined, a function that outputs the assembler code for exit from a
  function.  The epilogue is responsible for restoring the saved
  registers and stack pointer to their values when the function was
  called, and returning control to the caller.  This macro takes the
  same argument as the macro ``TARGET_ASM_FUNCTION_PROLOGUE``, and the
  registers to restore are determined from ``regs_ever_live`` and
  ``CALL_USED_REGISTERS`` in the same way.

  On some machines, there is a single instruction that does all the work
  of returning from the function.  On these machines, give that
  instruction the name :samp:`return` and do not define the macro
  ``TARGET_ASM_FUNCTION_EPILOGUE`` at all.

  Do not define a pattern named :samp:`return` if you want the
  ``TARGET_ASM_FUNCTION_EPILOGUE`` to be used.  If you want the target
  switches to control whether return instructions or epilogues are used,
  define a :samp:`return` pattern with a validity condition that tests the
  target switches appropriately.  If the :samp:`return` pattern's validity
  condition is false, epilogues will be used.

  On machines where functions may or may not have frame-pointers, the
  function exit code must vary accordingly.  Sometimes the code for these
  two cases is completely different.  To determine whether a frame pointer
  is wanted, the macro can refer to the variable
  ``frame_pointer_needed``.  The variable's value will be 1 when compiling
  a function that needs a frame pointer.

  Normally, ``TARGET_ASM_FUNCTION_PROLOGUE`` and
  ``TARGET_ASM_FUNCTION_EPILOGUE`` must treat leaf functions specially.
  The C variable ``current_function_is_leaf`` is nonzero for such a
  function.  See :ref:`leaf-functions`.

  On some machines, some functions pop their arguments on exit while
  others leave that for the caller to do.  For example, the 68020 when
  given :option:`-mrtd` pops arguments in functions that take a fixed
  number of arguments.

  .. index:: pops_args, crtl->args.pops_args

  Your definition of the macro ``RETURN_POPS_ARGS`` decides which
  functions pop their own arguments.  ``TARGET_ASM_FUNCTION_EPILOGUE``
  needs to know what was decided.  The number of bytes of the current
  function's arguments that this function should pop is available in
  ``crtl->args.pops_args``.  See :ref:`scalar-return`.

.. hook-end

*
  .. index:: pretend_args_size, crtl->args.pretend_args_size

  A region of ``crtl->args.pretend_args_size`` bytes of
  uninitialized space just underneath the first argument arriving on the
  stack.  (This may not be at the very start of the allocated stack region
  if the calling sequence has pushed anything else since pushing the stack
  arguments.  But usually, on such machines, nothing else has been pushed
  yet, because the function prologue itself does all the pushing.)  This
  region is used on machines where an argument may be passed partly in
  registers and partly in memory, and, in some cases to support the
  features in ``<stdarg.h>``.

* An area of memory used to save certain registers used by the function.
  The size of this area, which may also include space for such things as
  the return address and pointers to previous stack frames, is
  machine-specific and usually depends on which registers have been used
  in the function.  Machines with register windows often do not require
  a save area.

* A region of at least :samp:`{size}` bytes, possibly rounded up to an allocation
  boundary, to contain the local variables of the function.  On some machines,
  this region and the save area may occur in the opposite order, with the
  save area closer to the top of the stack.

.. index:: ACCUMULATE_OUTGOING_ARGS and stack frames

* Optionally, when ``ACCUMULATE_OUTGOING_ARGS`` is defined, a region of
  ``crtl->outgoing_args_size`` bytes to be used for outgoing
  argument lists of the function.  See :ref:`stack-arguments`.

.. c:macro:: EXIT_IGNORE_STACK

  Define this macro as a C expression that is nonzero if the return
  instruction or the function epilogue ignores the value of the stack
  pointer; in other words, if it is safe to delete an instruction to
  adjust the stack pointer before a return from the function.  The
  default is 0.

  Note that this macro's value is relevant only for functions for which
  frame pointers are maintained.  It is never safe to delete a final
  stack adjustment in a function that has no frame pointer, and the
  compiler knows this regardless of ``EXIT_IGNORE_STACK``.

.. c:macro:: EPILOGUE_USES (regno)

  Define this macro as a C expression that is nonzero for registers that are
  used by the epilogue or the :samp:`return` pattern.  The stack and frame
  pointer registers are already assumed to be used as needed.

.. c:macro:: EH_USES (regno)

  Define this macro as a C expression that is nonzero for registers that are
  used by the exception handling mechanism, and so should be considered live
  on entry to an exception edge.

.. function:: void TARGET_ASM_OUTPUT_MI_THUNK (FILE *file, tree thunk_fndecl, HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset, tree function)

  .. hook-start:TARGET_ASM_OUTPUT_MI_THUNK

  A function that outputs the assembler code for a thunk
  function, used to implement C++ virtual function calls with multiple
  inheritance.  The thunk acts as a wrapper around a virtual function,
  adjusting the implicit object parameter before handing control off to
  the real function.

  First, emit code to add the integer :samp:`{delta}` to the location that
  contains the incoming first argument.  Assume that this argument
  contains a pointer, and is the one used to pass the ``this`` pointer
  in C++.  This is the incoming argument *before* the function prologue,
  e.g. :samp:`%o0` on a sparc.  The addition must preserve the values of
  all other incoming arguments.

  Then, if :samp:`{vcall_offset}` is nonzero, an additional adjustment should be
  made after adding ``delta``.  In particular, if :samp:`{p}` is the
  adjusted pointer, the following adjustment should be made:

  .. code-block:: c++

    p += (*((ptrdiff_t **)p))[vcall_offset/sizeof(ptrdiff_t)]

  After the additions, emit code to jump to :samp:`{function}`, which is a
  ``FUNCTION_DECL``.  This is a direct pure jump, not a call, and does
  not touch the return address.  Hence returning from :samp:`{FUNCTION}` will
  return to whoever called the current :samp:`thunk`.

  The effect must be as if :samp:`{function}` had been called directly with
  the adjusted first argument.  This macro is responsible for emitting all
  of the code for a thunk function; ``TARGET_ASM_FUNCTION_PROLOGUE``
  and ``TARGET_ASM_FUNCTION_EPILOGUE`` are not invoked.

  The :samp:`{thunk_fndecl}` is redundant.  (:samp:`{delta}` and :samp:`{function}`
  have already been extracted from it.)  It might possibly be useful on
  some targets, but probably not.

  If you do not define this macro, the target-independent code in the C++
  front end will generate a less efficient heavyweight thunk that calls
  :samp:`{function}` instead of jumping to it.  The generic approach does
  not support varargs.

.. hook-end

.. function:: bool TARGET_ASM_CAN_OUTPUT_MI_THUNK (const_tree thunk_fndecl, HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset, const_tree function)

  .. hook-start:TARGET_ASM_CAN_OUTPUT_MI_THUNK

  A function that returns true if TARGET_ASM_OUTPUT_MI_THUNK would be able
  to output the assembler code for the thunk function specified by the
  arguments it is passed, and false otherwise.  In the latter case, the
  generic approach will be used by the C++ front end, with the limitations
  previously exposed.

.. hook-end