.. Copyright (C) 2014 Free Software Foundation, Inc.
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

  This function acquires a new :c:type:`gcc_jit_object *` instance,
  which is independent of any others that may be present within this
  process.

.. function:: void gcc_jit_context_release (gcc_jit_context *ctxt)

  This function releases all resources associated with the given context.
  Both the context itself and all of its :c:type:`gcc_jit_object *`
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
Instances of :c:type:`gcc_jit_object *` created via
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
followup error messages, but is safe (albeit verbose).

.. function:: const char *\
              gcc_jit_context_get_first_error (gcc_jit_context *ctxt)

   Returns the first error message that occurred on the context.

   The returned string is valid for the rest of the lifetime of the
   context.

   If no errors occurred, this will be NULL.

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


Options
-------

String Options
**************

.. function:: void gcc_jit_context_set_str_option(gcc_jit_context *ctxt, \
                                                  enum gcc_jit_str_option opt, \
                                                  const char *value)

   Set a string option of the context.

   .. type:: enum gcc_jit_str_option

   There is currently just one string option:

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

  .. type:: enum gcc_jit_bool_option

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
           .ident    "GCC: (GNU) 4.9.0 20131023 (Red Hat 0.1-%{gcc_release})"
           .section    .note.GNU-stack,"",@progbits


  .. macro:: GCC_JIT_BOOL_OPTION_DUMP_SUMMARY

     If true, :func:`gcc_jit_context_compile` will print information to stderr
     on the actions it is performing, followed by a profile showing
     the time taken and memory usage of each phase.

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

Integer options
***************

.. function:: void gcc_jit_context_set_int_option (gcc_jit_context *ctxt, \
				                   enum gcc_jit_int_option opt, \
				                   int value)

  Set an integer option of the context.

  .. type:: enum gcc_jit_int_option

  There is currently just one integer option:

  .. macro:: GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL

     How much to optimize the code.

     Valid values are 0-3, corresponding to GCC's command-line options
     -O0 through -O3.

     The default value is 0 (unoptimized).
