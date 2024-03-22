.. Copyright (C) 2014-2024 Free Software Foundation, Inc.
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
   <https://www.gnu.org/licenses/>.

.. default-domain:: c

Compilation contexts
====================

.. type:: gcc_jit_context

The top-level of the API is the :c:type:`gcc_jit_context` type.

A :c:type:`gcc_jit_context` instance encapsulates the state of a
compilation.

You can set up options on it, and add types, functions and code.
Invoking :c:func:`gcc_jit_context_compile` on it gives you a
:c:type:`gcc_jit_result`.

Lifetime-management
-------------------
Contexts are the unit of lifetime-management within the API: objects
have their lifetime bounded by the context they are created within, and
cleanup of such objects is done for you when the context is released.

.. function:: gcc_jit_context *gcc_jit_context_acquire (void)

  This function acquires a new :c:expr:`gcc_jit_context *` instance,
  which is independent of any others that may be present within this
  process.

.. function:: void gcc_jit_context_release (gcc_jit_context *ctxt)

  This function releases all resources associated with the given context.
  Both the context itself and all of its :c:expr:`gcc_jit_object *`
  instances are cleaned up.  It should be called exactly once on a given
  context.

  It is invalid to use the context or any of its "contextual" objects
  after calling this.

  .. code-block:: c

    gcc_jit_context_release (ctxt);

.. function:: gcc_jit_context * gcc_jit_context_new_child_context (gcc_jit_context *parent_ctxt)

   Given an existing JIT context, create a child context.

   The child inherits a copy of all option-settings from the parent.

   The child can reference objects created within the parent, but not
   vice-versa.

   The lifetime of the child context must be bounded by that of the
   parent: you should release a child context before releasing the parent
   context.

   If you use a function from a parent context within a child context,
   you have to compile the parent context before you can compile the
   child context, and the gcc_jit_result of the parent context must
   outlive the gcc_jit_result of the child context.

   This allows caching of shared initializations.  For example, you could
   create types and declarations of global functions in a parent context
   once within a process, and then create child contexts whenever a
   function or loop becomes hot. Each such child context can be used for
   JIT-compiling just one function or loop, but can reference types
   and helper functions created within the parent context.

   Contexts can be arbitrarily nested, provided the above rules are
   followed, but it's probably not worth going above 2 or 3 levels, and
   there will likely be a performance hit for such nesting.


Thread-safety
-------------
Instances of :c:expr:`gcc_jit_context *` created via
:c:func:`gcc_jit_context_acquire` are independent from each other:
only one thread may use a given context at once, but multiple threads
could each have their own contexts without needing locks.

Contexts created via :c:func:`gcc_jit_context_new_child_context` are
related to their parent context.  They can be partitioned by their
ultimate ancestor into independent "family trees".   Only one thread
within a process may use a given "family tree" of such contexts at once,
and if you're using multiple threads you should provide your own locking
around entire such context partitions.

.. _error-handling:

Error-handling
--------------
Various kinds of errors are possible when using the API, such as
mismatched types in an assignment.  You can only compile and get code from
a context if no errors occur.

Errors are printed on stderr and can be queried using
:c:func:`gcc_jit_context_get_first_error`.

They typically contain the name of the API entrypoint where the error
occurred, and pertinent information on the problem:

.. code-block:: console

  ./buggy-program: error: gcc_jit_block_add_assignment: mismatching types: assignment to i (type: int) from "hello world" (type: const char *)

In general, if an error occurs when using an API entrypoint, the
entrypoint returns NULL.  You don't have to check everywhere for NULL
results, since the API handles a NULL being passed in for any
argument by issuing another error.  This typically leads to a cascade of
followup error messages, but is safe (albeit verbose).  The first error
message is usually the one to pay attention to, since it is likely to
be responsible for all of the rest:

.. function:: const char *\
              gcc_jit_context_get_first_error (gcc_jit_context *ctxt)

   Returns the first error message that occurred on the context.

   The returned string is valid for the rest of the lifetime of the
   context.

   If no errors occurred, this will be NULL.

If you are wrapping the C API for a higher-level language that supports
exception-handling, you may instead be interested in the last error that
occurred on the context, so that you can embed this in an exception:

.. function:: const char *\
              gcc_jit_context_get_last_error (gcc_jit_context *ctxt)

   Returns the last error message that occurred on the context.

   If no errors occurred, this will be NULL.

   If non-NULL, the returned string is only guaranteed to be valid until
   the next call to libgccjit relating to this context.

Debugging
---------

.. function:: void\
              gcc_jit_context_dump_to_file (gcc_jit_context *ctxt,\
                                            const char *path,\
                                            int update_locations)

   To help with debugging: dump a C-like representation to the given path,
   describing what's been set up on the context.

   If "update_locations" is true, then also set up :type:`gcc_jit_location`
   information throughout the context, pointing at the dump file as if it
   were a source file.  This may be of use in conjunction with
   :macro:`GCC_JIT_BOOL_OPTION_DEBUGINFO` to allow stepping through the
   code in a debugger.

.. function:: void\
              gcc_jit_context_set_logfile (gcc_jit_context *ctxt,\
                                           FILE *logfile,\
                                           int flags,\
                                           int verbosity)

   To help with debugging; enable ongoing logging of the context's
   activity to the given file.

   For example, the following will enable logging to stderr.

   .. code-block:: c

      gcc_jit_context_set_logfile (ctxt, stderr, 0, 0);

   Examples of information logged include:

   * API calls

   * the various steps involved within compilation

   * activity on any :c:type:`gcc_jit_result` instances created by
     the context

   * activity within any child contexts

   An example of a log can be seen :ref:`here <example-of-log-file>`,
   though the precise format and kinds of information logged is subject
   to change.

   The caller remains responsible for closing `logfile`, and it must not
   be closed until all users are released.  In particular, note that
   child contexts and :c:type:`gcc_jit_result` instances created by
   the context will use the logfile.

   There may a performance cost for logging.

   You can turn off logging on `ctxt` by passing `NULL` for `logfile`.
   Doing so only affects the context; it does not affect child contexts
   or :c:type:`gcc_jit_result` instances already created by
   the context.

   The parameters "flags" and "verbosity" are reserved for future
   expansion, and must be zero for now.

To contrast the above: :c:func:`gcc_jit_context_dump_to_file` dumps the
current state of a context to the given path, whereas
:c:func:`gcc_jit_context_set_logfile` enables on-going logging of
future activies on a context to the given `FILE *`.


.. function:: void\
              gcc_jit_context_dump_reproducer_to_file (gcc_jit_context *ctxt,\
                                                       const char *path)

   Write C source code into `path` that can be compiled into a
   self-contained executable (i.e. with libgccjit as the only dependency).
   The generated code will attempt to replay the API calls that have been
   made into the given context.

   This may be useful when debugging the library or client code, for
   reducing a complicated recipe for reproducing a bug into a simpler
   form.  For example, consider client code that parses some source file
   into some internal representation, and then walks this IR, calling into
   libgccjit.  If this encounters a bug, a call to
   `gcc_jit_context_dump_reproducer_to_file` will write out C code for
   a much simpler executable that performs the equivalent calls into
   libgccjit, without needing the client code and its data.

   Typically you need to supply :option:`-Wno-unused-variable` when
   compiling the generated file (since the result of each API call is
   assigned to a unique variable within the generated C source, and not
   all are necessarily then used).

.. function:: void\
              gcc_jit_context_enable_dump (gcc_jit_context *ctxt,\
                                           const char *dumpname, \
                                           char **out_ptr)

   Enable the dumping of a specific set of internal state from the
   compilation, capturing the result in-memory as a buffer.

   Parameter "dumpname" corresponds to the equivalent gcc command-line
   option, without the "-fdump-" prefix.
   For example, to get the equivalent of :option:`-fdump-tree-vrp1`,
   supply ``"tree-vrp1"``:

   .. code-block:: c

      static char *dump_vrp1;

      void
      create_code (gcc_jit_context *ctxt)
      {
         gcc_jit_context_enable_dump (ctxt, "tree-vrp1", &dump_vrp1);
         /* (other API calls omitted for brevity) */
      }

   The context directly stores the dumpname as a ``(const char *)``, so
   the passed string must outlive the context.

   :func:`gcc_jit_context_compile` will capture the dump as a
   dynamically-allocated buffer, writing it to ``*out_ptr``.

   The caller becomes responsible for calling:

   .. code-block:: c

      free (*out_ptr)

   each time that :func:`gcc_jit_context_compile` is called.
   ``*out_ptr`` will be written to, either with the address of a buffer,
   or with ``NULL`` if an error occurred.

   .. warning::

      This API entrypoint is likely to be less stable than the others.
      In particular, both the precise dumpnames, and the format and content
      of the dumps are subject to change.

      It exists primarily for writing the library's own test suite.

Options
-------

Options present in the initial release of libgccjit were handled using
enums, whereas those added subsequently have their own per-option API
entrypoints.

Adding entrypoints for each new option means that client code that use
the new options can be identified directly from binary metadata, which
would not be possible if we instead extended the various
``enum gcc_jit_*_option``.

String Options
**************

.. function:: void gcc_jit_context_set_str_option(gcc_jit_context *ctxt, \
                                                  enum gcc_jit_str_option opt, \
                                                  const char *value)

   Set a string option of the context.

   .. enum:: gcc_jit_str_option

   The parameter ``value`` can be NULL.   If non-NULL, the call takes a
   copy of the underlying string, so it is valid to pass in a pointer to
   an on-stack buffer.

   There is just one string option specified this way:

   .. macro:: GCC_JIT_STR_OPTION_PROGNAME

      The name of the program, for use as a prefix when printing error
      messages to stderr.  If `NULL`, or default, "libgccjit.so" is used.

Boolean options
***************

.. function:: void gcc_jit_context_set_bool_option(gcc_jit_context *ctxt, \
				                   enum gcc_jit_bool_option opt, \
				                   int value)

  Set a boolean option of the context.
  Zero is "false" (the default), non-zero is "true".

  .. enum:: gcc_jit_bool_option

  .. macro:: GCC_JIT_BOOL_OPTION_DEBUGINFO

     If true, :func:`gcc_jit_context_compile` will attempt to do the right
     thing so that if you attach a debugger to the process, it will
     be able to inspect variables and step through your code.

     Note that you can't step through code unless you set up source
     location information for the code (by creating and passing in
     :type:`gcc_jit_location` instances).

  .. macro:: GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE

     If true, :func:`gcc_jit_context_compile` will dump its initial
     "tree" representation of your code to stderr (before any
     optimizations).

     Here's some sample output (from the `square` example)::

        <statement_list 0x7f4875a62cc0
           type <void_type 0x7f4875a64bd0 VOID
               align 8 symtab 0 alias set -1 canonical type 0x7f4875a64bd0
               pointer_to_this <pointer_type 0x7f4875a64c78>>
           side-effects head 0x7f4875a761e0 tail 0x7f4875a761f8 stmts 0x7f4875a62d20 0x7f4875a62d00

           stmt <label_expr 0x7f4875a62d20 type <void_type 0x7f4875a64bd0>
               side-effects
               arg 0 <label_decl 0x7f4875a79080 entry type <void_type 0x7f4875a64bd0>
                   VOID file (null) line 0 col 0
                   align 1 context <function_decl 0x7f4875a77500 square>>>
           stmt <return_expr 0x7f4875a62d00
               type <integer_type 0x7f4875a645e8 public SI
                   size <integer_cst 0x7f4875a623a0 constant 32>
                   unit size <integer_cst 0x7f4875a623c0 constant 4>
                   align 32 symtab 0 alias set -1 canonical type 0x7f4875a645e8 precision 32 min <integer_cst 0x7f4875a62340 -2147483648> max <integer_cst 0x7f4875a62360 2147483647>
                   pointer_to_this <pointer_type 0x7f4875a6b348>>
               side-effects
               arg 0 <modify_expr 0x7f4875a72a78 type <integer_type 0x7f4875a645e8>
                   side-effects arg 0 <result_decl 0x7f4875a7a000 D.54>
                   arg 1 <mult_expr 0x7f4875a72a50 type <integer_type 0x7f4875a645e8>
                       arg 0 <parm_decl 0x7f4875a79000 i> arg 1 <parm_decl 0x7f4875a79000 i>>>>>

  .. macro:: GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE

     If true, :func:`gcc_jit_context_compile` will dump the "gimple"
     representation of your code to stderr, before any optimizations
     are performed.  The dump resembles C code:

     .. code-block:: c

       square (signed int i)
       {
         signed int D.56;

         entry:
         D.56 = i * i;
         return D.56;
       }

  .. macro:: GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE

     If true, :func:`gcc_jit_context_compile` will dump the final
     generated code to stderr, in the form of assembly language:

     .. code-block:: gas

           .file    "fake.c"
           .text
           .globl    square
           .type    square, @function
       square:
       .LFB0:
           .cfi_startproc
           pushq    %rbp
           .cfi_def_cfa_offset 16
           .cfi_offset 6, -16
           movq    %rsp, %rbp
           .cfi_def_cfa_register 6
           movl    %edi, -4(%rbp)
       .L2:
           movl    -4(%rbp), %eax
           imull    -4(%rbp), %eax
           popq    %rbp
           .cfi_def_cfa 7, 8
           ret
           .cfi_endproc
       .LFE0:
           .size    square, .-square
           .ident    "GCC: (GNU) 4.9.0 20131023 (Red Hat 0.2)"
           .section    .note.GNU-stack,"",@progbits


  .. macro:: GCC_JIT_BOOL_OPTION_DUMP_SUMMARY

     If true, :func:`gcc_jit_context_compile` will print information to stderr
     on the actions it is performing.

  .. macro:: GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING

     If true, :func:`gcc_jit_context_compile` will dump copious
     amount of information on what it's doing to various
     files within a temporary directory.  Use
     :macro:`GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES` (see below) to
     see the results.  The files are intended to be human-readable,
     but the exact files and their formats are subject to change.

  .. macro:: GCC_JIT_BOOL_OPTION_SELFCHECK_GC

     If true, libgccjit will aggressively run its garbage collector, to
     shake out bugs (greatly slowing down the compile).  This is likely
     to only be of interest to developers *of* the library.  It is
     used when running the selftest suite.

  .. macro:: GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES

     If true, the :type:`gcc_jit_context` will not clean up intermediate files
     written to the filesystem, and will display their location on stderr.

.. function:: void \
              gcc_jit_context_set_bool_allow_unreachable_blocks (gcc_jit_context *ctxt, \
                                                                 int bool_value)

   By default, libgccjit will issue an error about unreachable blocks
   within a function.

   This entrypoint can be used to disable that error.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_2`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_allow_unreachable_blocks

.. function:: void \
              gcc_jit_context_set_bool_use_external_driver (gcc_jit_context *ctxt, \
                                                            int bool_value)

   libgccjit internally generates assembler, and uses "driver" code
   for converting it to other formats (e.g. shared libraries).

   By default, libgccjit will use an embedded copy of the driver
   code.

   This option can be used to instead invoke an external driver executable
   as a subprocess.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_5`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_use_external_driver

.. function:: void \
              gcc_jit_context_set_bool_print_errors_to_stderr (gcc_jit_context *ctxt, \
                                                                 int enabled)

   By default, libgccjit will print errors to stderr.

   This entrypoint can be used to disable the printing.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_23`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_print_errors_to_stderr

Integer options
***************

.. function:: void gcc_jit_context_set_int_option (gcc_jit_context *ctxt, \
				                   enum gcc_jit_int_option opt, \
				                   int value)

  Set an integer option of the context.

  .. enum:: gcc_jit_int_option

  There is just one integer option specified this way:

  .. macro:: GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL

     How much to optimize the code.

     Valid values are 0-3, corresponding to GCC's command-line options
     -O0 through -O3.

     The default value is 0 (unoptimized).

Additional command-line options
*******************************

.. function:: void gcc_jit_context_add_command_line_option (gcc_jit_context *ctxt,\
                                                            const char *optname)

   Add an arbitrary gcc command-line option to the context, for use
   by :func:`gcc_jit_context_compile` and
   :func:`gcc_jit_context_compile_to_file`.

   The parameter ``optname`` must be non-NULL.  The underlying buffer is
   copied, so that it does not need to outlive the call.

   Extra options added by `gcc_jit_context_add_command_line_option` are
   applied *after* the regular options above, potentially overriding them.
   Options from parent contexts are inherited by child contexts; options
   from the parent are applied *before* those from the child.

   For example:

   .. code-block:: c

      gcc_jit_context_add_command_line_option (ctxt, "-ffast-math");
      gcc_jit_context_add_command_line_option (ctxt, "-fverbose-asm");

   Note that only some options are likely to be meaningful; there is no
   "frontend" within libgccjit, so typically only those affecting
   optimization and code-generation are likely to be useful.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_1`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option

.. function:: void gcc_jit_context_add_driver_option (gcc_jit_context *ctxt,\
						      const char *optname)

   Add an arbitrary gcc driver option to the context, for use by
   :func:`gcc_jit_context_compile` and
   :func:`gcc_jit_context_compile_to_file`.

   The parameter ``optname`` must be non-NULL.  The underlying buffer is
   copied, so that it does not need to outlive the call.

   Extra options added by `gcc_jit_context_add_driver_option` are
   applied *after* all other options potentially overriding them.
   Options from parent contexts are inherited by child contexts; options
   from the parent are applied *before* those from the child.

   For example:

   .. code-block:: c

      gcc_jit_context_add_driver_option (ctxt, "-lm");
      gcc_jit_context_add_driver_option (ctxt, "-fuse-linker-plugin");

      gcc_jit_context_add_driver_option (ctxt, "obj.o");

      gcc_jit_context_add_driver_option (ctxt, "-L.");
      gcc_jit_context_add_driver_option (ctxt, "-lwhatever");

   Note that only some options are likely to be meaningful; there is no
   "frontend" within libgccjit, so typically only those affecting
   assembler and linker are likely to be useful.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_11`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_context_add_driver_option
