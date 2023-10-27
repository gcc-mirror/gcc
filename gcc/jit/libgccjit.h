/* A pure C API to enable client code to embed GCC as a JIT-compiler.
   Copyright (C) 2013-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef LIBGCCJIT_H
#define LIBGCCJIT_H

#include <stdio.h>
#ifdef __has_include
#if __has_include (<sys/types.h>)
#include <sys/types.h>
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/**********************************************************************
 Data structures.
 **********************************************************************/
/* All structs within the API are opaque. */

/* A gcc_jit_context encapsulates the state of a compilation.
   You can set up options on it, and add types, functions and code, using
   the API below.

   Invoking gcc_jit_context_compile on it gives you a gcc_jit_result *
   (or NULL), representing in-memory machine code.

   You can call gcc_jit_context_compile repeatedly on one context, giving
   multiple independent results.

   Similarly, you can call gcc_jit_context_compile_to_file on a context
   to compile to disk.

   Eventually you can call gcc_jit_context_release to clean up the
   context; any in-memory results created from it are still usable, and
   should be cleaned up via gcc_jit_result_release.  */
typedef struct gcc_jit_context gcc_jit_context;

/* A gcc_jit_result encapsulates the result of an in-memory compilation.  */
typedef struct gcc_jit_result gcc_jit_result;

/* An object created within a context.  Such objects are automatically
   cleaned up when the context is released.

   The class hierarchy looks like this:

     +- gcc_jit_object
	 +- gcc_jit_location
	 +- gcc_jit_type
	    +- gcc_jit_struct
	    +- gcc_jit_function_type
	    +- gcc_jit_vector_type
	 +- gcc_jit_field
	 +- gcc_jit_function
	 +- gcc_jit_block
	 +- gcc_jit_rvalue
	     +- gcc_jit_lvalue
		 +- gcc_jit_param
	 +- gcc_jit_case
	 +- gcc_jit_extended_asm
*/
typedef struct gcc_jit_object gcc_jit_object;

/* A gcc_jit_location encapsulates a source code location, so that
   you can (optionally) associate locations in your language with
   statements in the JIT-compiled code, allowing the debugger to
   single-step through your language.

   Note that to do so, you also need to enable
     GCC_JIT_BOOL_OPTION_DEBUGINFO
   on the gcc_jit_context.

   gcc_jit_location instances are optional; you can always pass
   NULL.  */
typedef struct gcc_jit_location gcc_jit_location;

/* A gcc_jit_type encapsulates a type e.g. "int" or a "struct foo*".  */
typedef struct gcc_jit_type gcc_jit_type;

/* A gcc_jit_field encapsulates a field within a struct; it is used
   when creating a struct type (using gcc_jit_context_new_struct_type).
   Fields cannot be shared between structs.  */
typedef struct gcc_jit_field gcc_jit_field;

/* A gcc_jit_struct encapsulates a struct type, either one that we have
   the layout for, or an opaque type.  */
typedef struct gcc_jit_struct gcc_jit_struct;

/* A gcc_jit_function_type encapsulates a function type.  */
typedef struct gcc_jit_function_type gcc_jit_function_type;

/* A gcc_jit_vector_type encapsulates a vector type.  */
typedef struct gcc_jit_vector_type gcc_jit_vector_type;

/* A gcc_jit_function encapsulates a function: either one that you're
   creating yourself, or a reference to one that you're dynamically
   linking to within the rest of the process.  */
typedef struct gcc_jit_function gcc_jit_function;

/* A gcc_jit_block encapsulates a "basic block" of statements within a
   function (i.e. with one entry point and one exit point).

   Every block within a function must be terminated with a conditional,
   a branch, or a return.

   The blocks within a function form a directed graph.

   The entrypoint to the function is the first block created within
   it.

   All of the blocks in a function must be reachable via some path from
   the first block.

   It's OK to have more than one "return" from a function (i.e. multiple
   blocks that terminate by returning).  */
typedef struct gcc_jit_block gcc_jit_block;

/* A gcc_jit_rvalue is an expression within your code, with some type.  */
typedef struct gcc_jit_rvalue gcc_jit_rvalue;

/* A gcc_jit_lvalue is a storage location within your code (e.g. a
   variable, a parameter, etc).  It is also a gcc_jit_rvalue; use
   gcc_jit_lvalue_as_rvalue to cast.  */
typedef struct gcc_jit_lvalue gcc_jit_lvalue;

/* A gcc_jit_param is a function parameter, used when creating a
   gcc_jit_function.  It is also a gcc_jit_lvalue (and thus also an
   rvalue); use gcc_jit_param_as_lvalue to convert.  */
typedef struct gcc_jit_param gcc_jit_param;

/* A gcc_jit_case is for use when building multiway branches via
   gcc_jit_block_end_with_switch and represents a range of integer
   values (or an individual integer value) together with an associated
   destination block.  */
typedef struct gcc_jit_case gcc_jit_case;

/* A gcc_jit_extended_asm represents an assembly language statement,
   analogous to an extended "asm" statement in GCC's C front-end: a series
   of low-level instructions inside a function that convert inputs to
   outputs.  */
typedef struct gcc_jit_extended_asm gcc_jit_extended_asm;

/* Acquire a JIT-compilation context.  */
extern gcc_jit_context *
gcc_jit_context_acquire (void);

/* Release the context.  After this call, it's no longer valid to use
   the ctxt.  */
extern void
gcc_jit_context_release (gcc_jit_context *ctxt);

/* Options present in the initial release of libgccjit.
   These were handled using enums.  */

/* Options taking string values. */
enum gcc_jit_str_option
{
  /* The name of the program, for use as a prefix when printing error
     messages to stderr.  If NULL, or default, "libgccjit.so" is used.  */
  GCC_JIT_STR_OPTION_PROGNAME,

  /* Special characters to allow in function names.  */
  GCC_JIT_STR_OPTION_SPECIAL_CHARS_IN_FUNC_NAMES,

  GCC_JIT_NUM_STR_OPTIONS
};

/* Options taking int values. */
enum gcc_jit_int_option
{
  /* How much to optimize the code.
     Valid values are 0-3, corresponding to GCC's command-line options
     -O0 through -O3.

     The default value is 0 (unoptimized).  */
  GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL,

  GCC_JIT_NUM_INT_OPTIONS
};

/* Options taking boolean values.
   These all default to "false".  */
enum gcc_jit_bool_option
{
  /* If true, gcc_jit_context_compile will attempt to do the right
     thing so that if you attach a debugger to the process, it will
     be able to inspect variables and step through your code.

     Note that you can't step through code unless you set up source
     location information for the code (by creating and passing in
     gcc_jit_location instances).  */
  GCC_JIT_BOOL_OPTION_DEBUGINFO,

  /* If true, gcc_jit_context_compile will dump its initial "tree"
     representation of your code to stderr (before any
     optimizations).  */
  GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE,

  /* If true, gcc_jit_context_compile will dump the "gimple"
     representation of your code to stderr, before any optimizations
     are performed.  The dump resembles C code.  */
  GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE,

  /* If true, gcc_jit_context_compile will dump the final
     generated code to stderr, in the form of assembly language.  */
  GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE,

  /* If true, gcc_jit_context_compile will print information to stderr
     on the actions it is performing, followed by a profile showing
     the time taken and memory usage of each phase.
   */
  GCC_JIT_BOOL_OPTION_DUMP_SUMMARY,

  /* If true, gcc_jit_context_compile will dump copious
     amount of information on what it's doing to various
     files within a temporary directory.  Use
     GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES (see below) to
     see the results.  The files are intended to be human-readable,
     but the exact files and their formats are subject to change.
  */
  GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING,

  /* If true, libgccjit will aggressively run its garbage collector, to
     shake out bugs (greatly slowing down the compile).  This is likely
     to only be of interest to developers *of* the library.  It is
     used when running the selftest suite.  */
  GCC_JIT_BOOL_OPTION_SELFCHECK_GC,

  /* If true, gcc_jit_context_release will not clean up
     intermediate files written to the filesystem, and will display
     their location on stderr.  */
  GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES,

  GCC_JIT_NUM_BOOL_OPTIONS
};

/* Set a string option on the given context.

   The context takes a copy of the string, so the
   (const char *) buffer is not needed anymore after the call
   returns.  */
extern void
gcc_jit_context_set_str_option (gcc_jit_context *ctxt,
				enum gcc_jit_str_option opt,
				const char *value);

/* Set an int option on the given context.  */
extern void
gcc_jit_context_set_int_option (gcc_jit_context *ctxt,
				enum gcc_jit_int_option opt,
				int value);

/* Set a boolean option on the given context.

   Zero is "false" (the default), non-zero is "true".  */
extern void
gcc_jit_context_set_bool_option (gcc_jit_context *ctxt,
				 enum gcc_jit_bool_option opt,
				 int value);

/* Options added after the initial release of libgccjit.
   These are handled by providing an entrypoint per option,
   rather than by extending the enum gcc_jit_*_option,
   so that client code that use these new options can be identified
   from binary metadata.  */

/* By default, libgccjit will issue an error about unreachable blocks
   within a function.

   This option can be used to disable that error.

   This entrypoint was added in LIBGCCJIT_ABI_2; you can test for
   its presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_allow_unreachable_blocks
*/

extern void
gcc_jit_context_set_bool_allow_unreachable_blocks (gcc_jit_context *ctxt,
						   int bool_value);

/* Pre-canned feature macro to indicate the presence of
   gcc_jit_context_set_bool_allow_unreachable_blocks.  This can be
   tested for with #ifdef.  */
#define LIBGCCJIT_HAVE_gcc_jit_context_set_bool_allow_unreachable_blocks

/* By default, libgccjit will print errors to stderr.

   This option can be used to disable the printing.

   This entrypoint was added in LIBGCCJIT_ABI_23; you can test for
   its presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_print_errors_to_stderr
*/

extern void
gcc_jit_context_set_bool_print_errors_to_stderr (gcc_jit_context *ctxt,
						 int enabled);

/* Pre-canned feature macro to indicate the presence of
   gcc_jit_context_set_bool_print_errors_to_stderr.  This can be
   tested for with #ifdef.  */
#define LIBGCCJIT_HAVE_gcc_jit_context_set_bool_print_errors_to_stderr

/* Implementation detail:
   libgccjit internally generates assembler, and uses "driver" code
   for converting it to other formats (e.g. shared libraries).

   By default, libgccjit will use an embedded copy of the driver
   code.

   This option can be used to instead invoke an external driver executable
   as a subprocess.

   This entrypoint was added in LIBGCCJIT_ABI_5; you can test for
   its presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_use_external_driver
*/

extern void
gcc_jit_context_set_bool_use_external_driver (gcc_jit_context *ctxt,
					      int bool_value);

/* Pre-canned feature macro to indicate the presence of
   gcc_jit_context_set_bool_use_external_driver.  This can be
   tested for with #ifdef.  */
#define LIBGCCJIT_HAVE_gcc_jit_context_set_bool_use_external_driver

/* Add an arbitrary gcc command-line option to the context.
   The context takes a copy of the string, so the
   (const char *) optname is not needed anymore after the call
   returns.

   Note that only some options are likely to be meaningful; there is no
   "frontend" within libgccjit, so typically only those affecting
   optimization and code-generation are likely to be useful.

   This entrypoint was added in LIBGCCJIT_ABI_1; you can test for
   its presence using
   #ifdef LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option
*/

extern void
gcc_jit_context_add_command_line_option (gcc_jit_context *ctxt,
					 const char *optname);

/* Pre-canned feature-test macro for detecting the presence of
   gcc_jit_context_add_command_line_option within libgccjit.h.  */

#define LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option

/* Add an arbitrary gcc driver option to the context.
   The context takes a copy of the string, so the
   (const char *) optname is not needed anymore after the call
   returns.

   Note that only some options are likely to be meaningful; there is no
   "frontend" within libgccjit, so typically only those affecting
   assembler and linker are likely to be useful.

   This entrypoint was added in LIBGCCJIT_ABI_11; you can test for
   its presence using
   #ifdef LIBGCCJIT_HAVE_gcc_jit_context_add_driver_option
*/
extern void
gcc_jit_context_add_driver_option (gcc_jit_context *ctxt,
				   const char *optname);

/* Pre-canned feature-test macro for detecting the presence of
   gcc_jit_context_add_driver_option within libgccjit.h.  */

#define LIBGCCJIT_HAVE_gcc_jit_context_add_driver_option

/* Compile the context to in-memory machine code.

   This can be called more that once on a given context,
   although any errors that occur will block further compilation.  */

extern gcc_jit_result *
gcc_jit_context_compile (gcc_jit_context *ctxt);

/* Kinds of ahead-of-time compilation, for use with
   gcc_jit_context_compile_to_file.  */

enum gcc_jit_output_kind
{
  /* Compile the context to an assembler file.  */
  GCC_JIT_OUTPUT_KIND_ASSEMBLER,

  /* Compile the context to an object file.  */
  GCC_JIT_OUTPUT_KIND_OBJECT_FILE,

  /* Compile the context to a dynamic library.  */
  GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY,

  /* Compile the context to an executable.  */
  GCC_JIT_OUTPUT_KIND_EXECUTABLE
};

/* Compile the context to a file of the given kind.

   This can be called more that once on a given context,
   although any errors that occur will block further compilation.  */

extern void
gcc_jit_context_compile_to_file (gcc_jit_context *ctxt,
				 enum gcc_jit_output_kind output_kind,
				 const char *output_path);

/* To help with debugging: dump a C-like representation to the given path,
   describing what's been set up on the context.

   If "update_locations" is true, then also set up gcc_jit_location
   information throughout the context, pointing at the dump file as if it
   were a source file.  This may be of use in conjunction with
   GCC_JIT_BOOL_OPTION_DEBUGINFO to allow stepping through the code in a
   debugger.  */
extern void
gcc_jit_context_dump_to_file (gcc_jit_context *ctxt,
			      const char *path,
			      int update_locations);

/* To help with debugging; enable ongoing logging of the context's
   activity to the given FILE *.

   The caller remains responsible for closing "logfile".

   Params "flags" and "verbosity" are reserved for future use, and
   must both be 0 for now.  */
extern void
gcc_jit_context_set_logfile (gcc_jit_context *ctxt,
			     FILE *logfile,
			     int flags,
			     int verbosity);

/* To be called after any API call, this gives the first error message
   that occurred on the context.

   The returned string is valid for the rest of the lifetime of the
   context.

   If no errors occurred, this will be NULL.  */
extern const char *
gcc_jit_context_get_first_error (gcc_jit_context *ctxt);

/* To be called after any API call, this gives the last error message
   that occurred on the context.

   If no errors occurred, this will be NULL.

   If non-NULL, the returned string is only guaranteed to be valid until
   the next call to libgccjit relating to this context. */
extern const char *
gcc_jit_context_get_last_error (gcc_jit_context *ctxt);

/* Locate a given function within the built machine code.
   This will need to be cast to a function pointer of the
   correct type before it can be called. */
extern void *
gcc_jit_result_get_code (gcc_jit_result *result,
			 const char *funcname);

/* Locate a given global within the built machine code.
   It must have been created using GCC_JIT_GLOBAL_EXPORTED.
   This is a ptr to the global, so e.g. for an int this is an int *.  */
extern void *
gcc_jit_result_get_global (gcc_jit_result *result,
			   const char *name);

/* Once we're done with the code, this unloads the built .so file.
   This cleans up the result; after calling this, it's no longer
   valid to use the result.  */
extern void
gcc_jit_result_release (gcc_jit_result *result);


/**********************************************************************
 Functions for creating "contextual" objects.

 All objects created by these functions share the lifetime of the context
 they are created within, and are automatically cleaned up for you when
 you call gcc_jit_context_release on the context.

 Note that this means you can't use references to them after you've
 released their context.

 All (const char *) string arguments passed to these functions are
 copied, so you don't need to keep them around.

 You create code by adding a sequence of statements to blocks.
**********************************************************************/

/**********************************************************************
 The base class of "contextual" object.
 **********************************************************************/
/* Which context is "obj" within?  */
extern gcc_jit_context *
gcc_jit_object_get_context (gcc_jit_object *obj);

/* Get a human-readable description of this object.
   The string buffer is created the first time this is called on a given
   object, and persists until the object's context is released.  */
extern const char *
gcc_jit_object_get_debug_string (gcc_jit_object *obj);

/**********************************************************************
 Debugging information.
 **********************************************************************/

/* Creating source code locations for use by the debugger.
   Line and column numbers are 1-based.  */
extern gcc_jit_location *
gcc_jit_context_new_location (gcc_jit_context *ctxt,
			      const char *filename,
			      int line,
			      int column);

/* Upcasting from location to object.  */
extern gcc_jit_object *
gcc_jit_location_as_object (gcc_jit_location *loc);


/**********************************************************************
 Types.
 **********************************************************************/

/* Upcasting from type to object.  */
extern gcc_jit_object *
gcc_jit_type_as_object (gcc_jit_type *type);

/* Access to specific types.  */
enum gcc_jit_types
{
  /* C's "void" type.  */
  GCC_JIT_TYPE_VOID,

  /* "void *".  */
  GCC_JIT_TYPE_VOID_PTR,

  /* C++'s bool type; also C99's "_Bool" type, aka "bool" if using
     stdbool.h.  */
  GCC_JIT_TYPE_BOOL,

  /* Various integer types.  */

  /* C's "char" (of some signedness) and the variants where the
     signedness is specified.  */
  GCC_JIT_TYPE_CHAR,
  GCC_JIT_TYPE_SIGNED_CHAR,
  GCC_JIT_TYPE_UNSIGNED_CHAR,

  /* C's "short" and "unsigned short".  */
  GCC_JIT_TYPE_SHORT, /* signed */
  GCC_JIT_TYPE_UNSIGNED_SHORT,

  /* C's "int" and "unsigned int".  */
  GCC_JIT_TYPE_INT, /* signed */
  GCC_JIT_TYPE_UNSIGNED_INT,

  /* C's "long" and "unsigned long".  */
  GCC_JIT_TYPE_LONG, /* signed */
  GCC_JIT_TYPE_UNSIGNED_LONG,

  /* C99's "long long" and "unsigned long long".  */
  GCC_JIT_TYPE_LONG_LONG, /* signed */
  GCC_JIT_TYPE_UNSIGNED_LONG_LONG,

  /* Floating-point types  */

  GCC_JIT_TYPE_FLOAT,
  GCC_JIT_TYPE_DOUBLE,
  GCC_JIT_TYPE_LONG_DOUBLE,

  /* C type: (const char *).  */
  GCC_JIT_TYPE_CONST_CHAR_PTR,

 /* The C "size_t" type.  */
  GCC_JIT_TYPE_SIZE_T,

 /* C type: (FILE *)  */
  GCC_JIT_TYPE_FILE_PTR,

  /* Complex numbers.  */
  GCC_JIT_TYPE_COMPLEX_FLOAT,
  GCC_JIT_TYPE_COMPLEX_DOUBLE,
  GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE,

  /* Sized integer types.  */
  GCC_JIT_TYPE_UINT8_T,
  GCC_JIT_TYPE_UINT16_T,
  GCC_JIT_TYPE_UINT32_T,
  GCC_JIT_TYPE_UINT64_T,
  GCC_JIT_TYPE_UINT128_T,
  GCC_JIT_TYPE_INT8_T,
  GCC_JIT_TYPE_INT16_T,
  GCC_JIT_TYPE_INT32_T,
  GCC_JIT_TYPE_INT64_T,
  GCC_JIT_TYPE_INT128_T,

  GCC_JIT_TYPE_BFLOAT16,
};

extern gcc_jit_type *
gcc_jit_context_get_type (gcc_jit_context *ctxt,
			  enum gcc_jit_types type_);

/* Get the integer type of the given size and signedness.  */
extern gcc_jit_type *
gcc_jit_context_get_int_type (gcc_jit_context *ctxt,
			      int num_bytes, int is_signed);

/* Constructing new types. */

/* Given type "T", get type "T*".  */
extern gcc_jit_type *
gcc_jit_type_get_pointer (gcc_jit_type *type);

/* Given type "T", get type "const T".  */
extern gcc_jit_type *
gcc_jit_type_get_const (gcc_jit_type *type);

/* Given type "T", get type "volatile T".  */
extern gcc_jit_type *
gcc_jit_type_get_volatile (gcc_jit_type *type);

#define LIBGCCJIT_HAVE_gcc_jit_type_get_restrict

/* Given type "T", get type "restrict T".
   This API entrypoint was added in LIBGCCJIT_ABI_25; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_type_get_restrict  */
extern gcc_jit_type *
gcc_jit_type_get_restrict (gcc_jit_type *type);

#define LIBGCCJIT_HAVE_SIZED_INTEGERS

/* Given types LTYPE and RTYPE, return non-zero if they are compatible.
   This API entrypoint was added in LIBGCCJIT_ABI_20; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_SIZED_INTEGERS  */
extern int
gcc_jit_compatible_types (gcc_jit_type *ltype,
			  gcc_jit_type *rtype);

/* Given type "T", get its size.
   This API entrypoint was added in LIBGCCJIT_ABI_20; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_SIZED_INTEGERS  */
extern ssize_t
gcc_jit_type_get_size (gcc_jit_type *type);

/* Given type "T", get type "T[N]" (for a constant N).  */
extern gcc_jit_type *
gcc_jit_context_new_array_type (gcc_jit_context *ctxt,
				gcc_jit_location *loc,
				gcc_jit_type *element_type,
				int num_elements);

/* Struct-handling.  */

/* Create a field, for use within a struct or union.  */
extern gcc_jit_field *
gcc_jit_context_new_field (gcc_jit_context *ctxt,
			   gcc_jit_location *loc,
			   gcc_jit_type *type,
			   const char *name);

#define LIBGCCJIT_HAVE_gcc_jit_context_new_bitfield

/* Create a bit field, for use within a struct or union.

   This API entrypoint was added in LIBGCCJIT_ABI_12; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_bitfield
*/
extern gcc_jit_field *
gcc_jit_context_new_bitfield (gcc_jit_context *ctxt,
			      gcc_jit_location *loc,
			      gcc_jit_type *type,
			      int width,
			      const char *name);

/* Upcasting from field to object.  */
extern gcc_jit_object *
gcc_jit_field_as_object (gcc_jit_field *field);

/* Create a struct type from an array of fields.  */
extern gcc_jit_struct *
gcc_jit_context_new_struct_type (gcc_jit_context *ctxt,
				 gcc_jit_location *loc,
				 const char *name,
				 int num_fields,
				 gcc_jit_field **fields);

/* Create an opaque struct type.  */
extern gcc_jit_struct *
gcc_jit_context_new_opaque_struct (gcc_jit_context *ctxt,
				   gcc_jit_location *loc,
				   const char *name);

/* Upcast a struct to a type.  */
extern gcc_jit_type *
gcc_jit_struct_as_type (gcc_jit_struct *struct_type);

/* Populating the fields of a formerly-opaque struct type.
   This can only be called once on a given struct type.  */
extern void
gcc_jit_struct_set_fields (gcc_jit_struct *struct_type,
			   gcc_jit_location *loc,
			   int num_fields,
			   gcc_jit_field **fields);

/* Get a field by index.  */
extern gcc_jit_field *
gcc_jit_struct_get_field (gcc_jit_struct *struct_type,
			   size_t index);

/* Get the number of fields.  */
extern size_t
gcc_jit_struct_get_field_count (gcc_jit_struct *struct_type);

/* Unions work similarly to structs.  */
extern gcc_jit_type *
gcc_jit_context_new_union_type (gcc_jit_context *ctxt,
				gcc_jit_location *loc,
				const char *name,
				int num_fields,
				gcc_jit_field **fields);

/* Function pointers. */

extern gcc_jit_type *
gcc_jit_context_new_function_ptr_type (gcc_jit_context *ctxt,
				       gcc_jit_location *loc,
				       gcc_jit_type *return_type,
				       int num_params,
				       gcc_jit_type **param_types,
				       int is_variadic);

/**********************************************************************
 Constructing functions.
 **********************************************************************/
/* Create a function param.  */
extern gcc_jit_param *
gcc_jit_context_new_param (gcc_jit_context *ctxt,
			   gcc_jit_location *loc,
			   gcc_jit_type *type,
			   const char *name);

/* Upcasting from param to object.  */
extern gcc_jit_object *
gcc_jit_param_as_object (gcc_jit_param *param);

/* Upcasting from param to lvalue.  */
extern gcc_jit_lvalue *
gcc_jit_param_as_lvalue (gcc_jit_param *param);

/* Upcasting from param to rvalue.  */
extern gcc_jit_rvalue *
gcc_jit_param_as_rvalue (gcc_jit_param *param);

/* Kinds of function.  */
enum gcc_jit_function_kind
{
  /* Function is defined by the client code and visible
     by name outside of the JIT.  */
  GCC_JIT_FUNCTION_EXPORTED,

  /* Function is defined by the client code, but is invisible
     outside of the JIT.  Analogous to a "static" function.  */
  GCC_JIT_FUNCTION_INTERNAL,

  /* Function is not defined by the client code; we're merely
     referring to it.  Analogous to using an "extern" function from a
     header file.  */
  GCC_JIT_FUNCTION_IMPORTED,

  /* Function is only ever inlined into other functions, and is
     invisible outside of the JIT.

     Analogous to prefixing with "inline" and adding
     __attribute__((always_inline)).

     Inlining will only occur when the optimization level is
     above 0; when optimization is off, this is essentially the
     same as GCC_JIT_FUNCTION_INTERNAL.  */
  GCC_JIT_FUNCTION_ALWAYS_INLINE
};

/* Thread local storage model.  */
enum gcc_jit_tls_model
{
  GCC_JIT_TLS_MODEL_NONE,
  GCC_JIT_TLS_MODEL_GLOBAL_DYNAMIC,
  GCC_JIT_TLS_MODEL_LOCAL_DYNAMIC,
  GCC_JIT_TLS_MODEL_INITIAL_EXEC,
  GCC_JIT_TLS_MODEL_LOCAL_EXEC,
};

/* Create a function.  */
extern gcc_jit_function *
gcc_jit_context_new_function (gcc_jit_context *ctxt,
			      gcc_jit_location *loc,
			      enum gcc_jit_function_kind kind,
			      gcc_jit_type *return_type,
			      const char *name,
			      int num_params,
			      gcc_jit_param **params,
			      int is_variadic);

/* Create a reference to a builtin function (sometimes called
   intrinsic functions).  */
extern gcc_jit_function *
gcc_jit_context_get_builtin_function (gcc_jit_context *ctxt,
				      const char *name);

/* Upcasting from function to object.  */
extern gcc_jit_object *
gcc_jit_function_as_object (gcc_jit_function *func);

/* Get a specific param of a function by index.  */
extern gcc_jit_param *
gcc_jit_function_get_param (gcc_jit_function *func, int index);

/* Emit the function in graphviz format.  */
extern void
gcc_jit_function_dump_to_dot (gcc_jit_function *func,
			      const char *path);

/* Create a block.

   The name can be NULL, or you can give it a meaningful name, which
   may show up in dumps of the internal representation, and in error
   messages.  */
extern gcc_jit_block *
gcc_jit_function_new_block (gcc_jit_function *func,
			    const char *name);

/* Upcasting from block to object.  */
extern gcc_jit_object *
gcc_jit_block_as_object (gcc_jit_block *block);

/* Which function is this block within?  */
extern gcc_jit_function *
gcc_jit_block_get_function (gcc_jit_block *block);

/**********************************************************************
 lvalues, rvalues and expressions.
 **********************************************************************/
enum gcc_jit_global_kind
{
  /* Global is defined by the client code and visible
     by name outside of this JIT context via gcc_jit_result_get_global.  */
  GCC_JIT_GLOBAL_EXPORTED,

  /* Global is defined by the client code, but is invisible
     outside of this JIT context.  Analogous to a "static" global.  */
  GCC_JIT_GLOBAL_INTERNAL,

  /* Global is not defined by the client code; we're merely
     referring to it.  Analogous to using an "extern" global from a
     header file.  */
  GCC_JIT_GLOBAL_IMPORTED
};

extern gcc_jit_lvalue *
gcc_jit_context_new_global (gcc_jit_context *ctxt,
			    gcc_jit_location *loc,
			    enum gcc_jit_global_kind kind,
			    gcc_jit_type *type,
			    const char *name);

#define LIBGCCJIT_HAVE_CTORS

/* Create a constructor for a struct as an rvalue.

   Returns NULL on error.  The two parameter arrays are copied and
   do not have to outlive the context.

   `type` specifies what the constructor will build and has to be
   a struct.

   `num_values` specifies the number of elements in `values`.

   `fields` need to have the same length as `values`, or be NULL.

   If `fields` is null, the values are applied in definition order.

   Otherwise, each field in `fields` specifies which field in the struct to
   set to the corresponding value in `values`.  `fields` and `values`
   are paired by index.

   Each value has to have the same unqualified type as the field
   it is applied to.

   A NULL value element  in `values` is a shorthand for zero initialization
   of the corresponding field.

   The fields in `fields` have to be in definition order, but there
   can be gaps.  Any field in the struct that is not specified in
   `fields` will be zeroed.

   The fields in `fields` need to be the same objects that were used
   to create the struct.

   If `num_values` is 0, the array parameters will be
   ignored and zero initialization will be used.

   The constructor rvalue can be used for assignment to locals.
   It can be used to initialize global variables with
   gcc_jit_global_set_initializer_rvalue.  It can also be used as a
   temporary value for function calls and return values.

   The constructor can contain nested constructors.

   This entrypoint was added in LIBGCCJIT_ABI_19; you can test for its
   presence using:
   #ifdef LIBGCCJIT_HAVE_CTORS
*/

extern gcc_jit_rvalue *
gcc_jit_context_new_struct_constructor (gcc_jit_context *ctxt,
					gcc_jit_location *loc,
					gcc_jit_type *type,
					size_t num_values,
					gcc_jit_field **fields,
					gcc_jit_rvalue **values);

/* Create a constructor for a union as an rvalue.

   Returns NULL on error.

   `type` specifies what the constructor will build and has to be
   an union.

   `field` specifies which field to set.  If it is NULL, the first
   field in the union will be set.  `field` need to be the same
   object that were used to create the union.

   `value` specifies what value to set the corresponding field to.
   If `value` is NULL, zero initialization will be used.

   Each value has to have the same unqualified type as the field
   it is applied to.

   `field` need to be the same objects that were used
   to create the union.

   The constructor rvalue can be used for assignment to locals.
   It can be used to initialize global variables with
   gcc_jit_global_set_initializer_rvalue.  It can also be used as a
   temporary value for function calls and return values.

   The constructor can contain nested constructors.

   This entrypoint was added in LIBGCCJIT_ABI_19; you can test for its
   presence using:
   #ifdef LIBGCCJIT_HAVE_CTORS
*/

extern gcc_jit_rvalue *
gcc_jit_context_new_union_constructor (gcc_jit_context *ctxt,
				       gcc_jit_location *loc,
				       gcc_jit_type *type,
				       gcc_jit_field *field,
				       gcc_jit_rvalue *value);

/* Create a constructor for an array as an rvalue.

   Returns NULL on error.  `values` are copied and
   do not have to outlive the context.

   `type` specifies what the constructor will build and has to be
   an array.

   `num_values` specifies the number of elements in `values` and
   it can't have more elements than the array type.

   Each value in `values` sets the corresponding value in the array.
   If the array type itself has more elements than `values`, the
   left-over elements will be zeroed.

   Each value in `values` need to be the same unqualified type as the
   array type's element type.

   If `num_values` is 0, the `values` parameter will be
   ignored and zero initialization will be used.

   Note that a string literal rvalue can't be used to construct a char
   array.  It needs one rvalue for each char.

   This entrypoint was added in LIBGCCJIT_ABI_19; you can test for its
   presence using:
   #ifdef LIBGCCJIT_HAVE_CTORS
*/

extern gcc_jit_rvalue *
gcc_jit_context_new_array_constructor (gcc_jit_context *ctxt,
				       gcc_jit_location *loc,
				       gcc_jit_type *type,
				       size_t num_values,
				       gcc_jit_rvalue **values);

/* Set the initial value of a global of any type with an rvalue.

   The rvalue needs to be a constant expression, e.g. no function calls.

   The global can't have the 'kind' GCC_JIT_GLOBAL_IMPORTED.

   Use together with gcc_jit_context_new_constructor () to
   initialize structs, unions and arrays.

   On success, returns the 'global' parameter unchanged.  Otherwise, NULL.

   'values' is copied and does not have to outlive the context.

   This entrypoint was added in LIBGCCJIT_ABI_19; you can test for its
   presence using:
     #ifdef LIBGCCJIT_HAVE_CTORS
*/

extern gcc_jit_lvalue *
gcc_jit_global_set_initializer_rvalue (gcc_jit_lvalue *global,
				       gcc_jit_rvalue *init_value);

#define LIBGCCJIT_HAVE_gcc_jit_context_get_target_builtin_function

/* Create a reference to a machine-specific builtin function (sometimes called
   intrinsic functions).

   This API entrypoint was added in LIBGCCJIT_ABI_32; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_context_get_target_builtin_function
*/
extern gcc_jit_function *
gcc_jit_context_get_target_builtin_function (gcc_jit_context *ctxt,
					     const char *name);

#define LIBGCCJIT_HAVE_gcc_jit_global_set_initializer

/* Set an initial value for a global, which must be an array of
   integral type.  Return the global itself.

   This API entrypoint was added in LIBGCCJIT_ABI_14; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_global_set_initializer
*/

extern gcc_jit_lvalue *
gcc_jit_global_set_initializer (gcc_jit_lvalue *global,
				const void *blob,
				size_t num_bytes);

extern void
gcc_jit_global_set_readonly (gcc_jit_lvalue *global);

#define LIBGCCJIT_HAVE_gcc_jit_global_set_readonly

/* Upcasting.  */
extern gcc_jit_object *
gcc_jit_lvalue_as_object (gcc_jit_lvalue *lvalue);

extern gcc_jit_rvalue *
gcc_jit_lvalue_as_rvalue (gcc_jit_lvalue *lvalue);

extern gcc_jit_object *
gcc_jit_rvalue_as_object (gcc_jit_rvalue *rvalue);

extern gcc_jit_type *
gcc_jit_rvalue_get_type (gcc_jit_rvalue *rvalue);

/* Integer constants. */
extern gcc_jit_rvalue *
gcc_jit_context_new_rvalue_from_int (gcc_jit_context *ctxt,
				     gcc_jit_type *numeric_type,
				     int value);

extern gcc_jit_rvalue *
gcc_jit_context_new_rvalue_from_long (gcc_jit_context *ctxt,
				      gcc_jit_type *numeric_type,
				      long value);

extern gcc_jit_rvalue *
gcc_jit_context_zero (gcc_jit_context *ctxt,
		      gcc_jit_type *numeric_type);

extern gcc_jit_rvalue *
gcc_jit_context_one (gcc_jit_context *ctxt,
		     gcc_jit_type *numeric_type);

/* Floating-point constants.  */
extern gcc_jit_rvalue *
gcc_jit_context_new_rvalue_from_double (gcc_jit_context *ctxt,
					gcc_jit_type *numeric_type,
					double value);

/* Pointers.  */
extern gcc_jit_rvalue *
gcc_jit_context_new_rvalue_from_ptr (gcc_jit_context *ctxt,
				     gcc_jit_type *pointer_type,
				     void *value);

extern gcc_jit_rvalue *
gcc_jit_context_null (gcc_jit_context *ctxt,
		      gcc_jit_type *pointer_type);

#define LIBGCCJIT_HAVE_gcc_jit_context_new_sizeof

/* Generates an rvalue that is equal to the size of type.

   This API entrypoint was added in LIBGCCJIT_ABI_27; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_sizeof  */

extern gcc_jit_rvalue *
gcc_jit_context_new_sizeof (gcc_jit_context *ctxt,
			    gcc_jit_type *type);

#define LIBGCCJIT_HAVE_gcc_jit_context_new_alignof

/* Generates an rvalue that is equal to the alignment of type.

   This API entrypoint was added in LIBGCCJIT_ABI_38; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_alignof  */

extern gcc_jit_rvalue *
gcc_jit_context_new_alignof (gcc_jit_context *ctxt,
			     gcc_jit_type *type);


/* String literals. */
extern gcc_jit_rvalue *
gcc_jit_context_new_string_literal (gcc_jit_context *ctxt,
				    const char *value);

enum gcc_jit_unary_op
{
  /* Negate an arithmetic value; analogous to:
       -(EXPR)
     in C.  */
  GCC_JIT_UNARY_OP_MINUS,

  /* Bitwise negation of an integer value (one's complement); analogous
     to:
       ~(EXPR)
     in C.  */
  GCC_JIT_UNARY_OP_BITWISE_NEGATE,

  /* Logical negation of an arithmetic or pointer value; analogous to:
       !(EXPR)
     in C.  */
  GCC_JIT_UNARY_OP_LOGICAL_NEGATE,

  /* Absolute value of an arithmetic expression; analogous to:
       abs (EXPR)
     in C.  */
  GCC_JIT_UNARY_OP_ABS

};

extern gcc_jit_rvalue *
gcc_jit_context_new_unary_op (gcc_jit_context *ctxt,
			      gcc_jit_location *loc,
			      enum gcc_jit_unary_op op,
			      gcc_jit_type *result_type,
			      gcc_jit_rvalue *rvalue);

enum gcc_jit_binary_op
{
  /* Addition of arithmetic values; analogous to:
       (EXPR_A) + (EXPR_B)
     in C.
     For pointer addition, use gcc_jit_context_new_array_access.  */
  GCC_JIT_BINARY_OP_PLUS,

  /* Subtraction of arithmetic values; analogous to:
       (EXPR_A) - (EXPR_B)
     in C.  */
  GCC_JIT_BINARY_OP_MINUS,

  /* Multiplication of a pair of arithmetic values; analogous to:
       (EXPR_A) * (EXPR_B)
     in C.  */
  GCC_JIT_BINARY_OP_MULT,

  /* Quotient of division of arithmetic values; analogous to:
       (EXPR_A) / (EXPR_B)
     in C.
     The result type affects the kind of division: if the result type is
     integer-based, then the result is truncated towards zero, whereas
     a floating-point result type indicates floating-point division.  */
  GCC_JIT_BINARY_OP_DIVIDE,

  /* Remainder of division of arithmetic values; analogous to:
       (EXPR_A) % (EXPR_B)
     in C.  */
  GCC_JIT_BINARY_OP_MODULO,

  /* Bitwise AND; analogous to:
       (EXPR_A) & (EXPR_B)
     in C.  */
  GCC_JIT_BINARY_OP_BITWISE_AND,

  /* Bitwise exclusive OR; analogous to:
       (EXPR_A) ^ (EXPR_B)
     in C.  */
  GCC_JIT_BINARY_OP_BITWISE_XOR,

  /* Bitwise inclusive OR; analogous to:
       (EXPR_A) | (EXPR_B)
     in C.  */
  GCC_JIT_BINARY_OP_BITWISE_OR,

  /* Logical AND; analogous to:
       (EXPR_A) && (EXPR_B)
     in C.  */
  GCC_JIT_BINARY_OP_LOGICAL_AND,

  /* Logical OR; analogous to:
       (EXPR_A) || (EXPR_B)
     in C.  */
  GCC_JIT_BINARY_OP_LOGICAL_OR,

  /* Left shift; analogous to:
       (EXPR_A) << (EXPR_B)
     in C.  */
  GCC_JIT_BINARY_OP_LSHIFT,

  /* Right shift; analogous to:
       (EXPR_A) >> (EXPR_B)
     in C.  */
  GCC_JIT_BINARY_OP_RSHIFT
};

extern gcc_jit_rvalue *
gcc_jit_context_new_binary_op (gcc_jit_context *ctxt,
			       gcc_jit_location *loc,
			       enum gcc_jit_binary_op op,
			       gcc_jit_type *result_type,
			       gcc_jit_rvalue *a, gcc_jit_rvalue *b);

/* (Comparisons are treated as separate from "binary_op" to save
   you having to specify the result_type).  */

enum gcc_jit_comparison
{
  /* (EXPR_A) == (EXPR_B).  */
  GCC_JIT_COMPARISON_EQ,

  /* (EXPR_A) != (EXPR_B).  */
  GCC_JIT_COMPARISON_NE,

  /* (EXPR_A) < (EXPR_B).  */
  GCC_JIT_COMPARISON_LT,

  /* (EXPR_A) <=(EXPR_B).  */
  GCC_JIT_COMPARISON_LE,

  /* (EXPR_A) > (EXPR_B).  */
  GCC_JIT_COMPARISON_GT,

  /* (EXPR_A) >= (EXPR_B).  */
  GCC_JIT_COMPARISON_GE
};

extern gcc_jit_rvalue *
gcc_jit_context_new_comparison (gcc_jit_context *ctxt,
				gcc_jit_location *loc,
				enum gcc_jit_comparison op,
				gcc_jit_rvalue *a, gcc_jit_rvalue *b);

/* Function calls.  */

/* Call of a specific function.  */
extern gcc_jit_rvalue *
gcc_jit_context_new_call (gcc_jit_context *ctxt,
			  gcc_jit_location *loc,
			  gcc_jit_function *func,
			  int numargs , gcc_jit_rvalue **args);

/* Call through a function pointer.  */
extern gcc_jit_rvalue *
gcc_jit_context_new_call_through_ptr (gcc_jit_context *ctxt,
				      gcc_jit_location *loc,
				      gcc_jit_rvalue *fn_ptr,
				      int numargs, gcc_jit_rvalue **args);

/* Type-coercion.

   Currently only a limited set of conversions are possible:
     int <-> float
     int <-> bool  */
extern gcc_jit_rvalue *
gcc_jit_context_new_cast (gcc_jit_context *ctxt,
			  gcc_jit_location *loc,
			  gcc_jit_rvalue *rvalue,
			  gcc_jit_type *type);

#define LIBGCCJIT_HAVE_gcc_jit_context_new_bitcast

/* Reinterpret a value as another type.

   The types must be of the same size.

   This API entrypoint was added in LIBGCCJIT_ABI_21; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_bitcast  */
extern gcc_jit_rvalue *
gcc_jit_context_new_bitcast (gcc_jit_context *ctxt,
			     gcc_jit_location *loc,
			     gcc_jit_rvalue *rvalue,
			     gcc_jit_type *type);

#define LIBGCCJIT_HAVE_ALIGNMENT

/* Set the alignment of a variable.

   This API entrypoint was added in LIBGCCJIT_ABI_24; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_ALIGNMENT  */
extern void
gcc_jit_lvalue_set_alignment (gcc_jit_lvalue *lvalue,
			      unsigned bytes);

/* Get the alignment of a variable.

   This API entrypoint was added in LIBGCCJIT_ABI_24; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_ALIGNMENT  */
extern unsigned
gcc_jit_lvalue_get_alignment (gcc_jit_lvalue *lvalue);

extern gcc_jit_lvalue *
gcc_jit_context_new_array_access (gcc_jit_context *ctxt,
				  gcc_jit_location *loc,
				  gcc_jit_rvalue *ptr,
				  gcc_jit_rvalue *index);

#define LIBGCCJIT_HAVE_gcc_jit_context_convert_vector

/* Given a vector rvalue, cast it to the type ``type``, doing an element-wise
   conversion.  */
extern gcc_jit_rvalue *
gcc_jit_context_convert_vector (gcc_jit_context *ctxt,
				gcc_jit_location *loc,
				gcc_jit_rvalue *vector,
				gcc_jit_type *type);

/* Build a permutation vector rvalue from an 3 arrays of elements.

   "vec_type" should be a vector type, created using gcc_jit_type_get_vector.

   This API entrypoint was added in LIBGCCJIT_ABI_31; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_VECTOR_OPERATIONS
*/
extern gcc_jit_rvalue *
gcc_jit_context_new_rvalue_vector_perm (gcc_jit_context *ctxt,
					gcc_jit_location *loc,
					gcc_jit_rvalue *elements1,
					gcc_jit_rvalue *elements2,
					gcc_jit_rvalue *mask);

#define LIBGCCJIT_HAVE_VECTOR_OPERATIONS

/* Get the element at INDEX in VECTOR.

   This API entrypoint was added in LIBGCCJIT_ABI_31; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_VECTOR_OPERATIONS
*/
extern gcc_jit_lvalue *
gcc_jit_context_new_vector_access (gcc_jit_context *ctxt,
				   gcc_jit_location *loc,
				   gcc_jit_rvalue *vector,
				   gcc_jit_rvalue *index);

/* Field access is provided separately for both lvalues and rvalues.  */

/* Accessing a field of an lvalue of struct type, analogous to:
      (EXPR).field = ...;
   in C.  */
extern gcc_jit_lvalue *
gcc_jit_lvalue_access_field (gcc_jit_lvalue *struct_or_union,
			     gcc_jit_location *loc,
			     gcc_jit_field *field);

/* Accessing a field of an rvalue of struct type, analogous to:
      (EXPR).field
   in C.  */
extern gcc_jit_rvalue *
gcc_jit_rvalue_access_field (gcc_jit_rvalue *struct_or_union,
			     gcc_jit_location *loc,
			     gcc_jit_field *field);

/* Accessing a field of an rvalue of pointer type, analogous to:
      (EXPR)->field
   in C, itself equivalent to (*EXPR).FIELD  */
extern gcc_jit_lvalue *
gcc_jit_rvalue_dereference_field (gcc_jit_rvalue *ptr,
				  gcc_jit_location *loc,
				  gcc_jit_field *field);

/* Dereferencing a pointer; analogous to:
     *(EXPR)
*/
extern gcc_jit_lvalue *
gcc_jit_rvalue_dereference (gcc_jit_rvalue *rvalue,
			    gcc_jit_location *loc);

/* Taking the address of an lvalue; analogous to:
     &(EXPR)
   in C.  */
extern gcc_jit_rvalue *
gcc_jit_lvalue_get_address (gcc_jit_lvalue *lvalue,
			    gcc_jit_location *loc);

#define LIBGCCJIT_HAVE_gcc_jit_lvalue_set_tls_model

/* Set the thread-local storage model of a global variable

   This API entrypoint was added in LIBGCCJIT_ABI_17; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_lvalue_set_tls_model  */
extern void
gcc_jit_lvalue_set_tls_model (gcc_jit_lvalue *lvalue,
			    enum gcc_jit_tls_model model);

#define LIBGCCJIT_HAVE_gcc_jit_lvalue_set_link_section

/* Set the link section of a global variable; analogous to:
     __attribute__((section(".section_name")))
   in C.

   This API entrypoint was added in LIBGCCJIT_ABI_18; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_lvalue_set_link_section
*/
extern void
gcc_jit_lvalue_set_link_section (gcc_jit_lvalue *lvalue,
			    const char *section_name);

#define LIBGCCJIT_HAVE_gcc_jit_lvalue_set_register_name

/* Make this variable a register variable and set its register name.

   This API entrypoint was added in LIBGCCJIT_ABI_22; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_lvalue_set_register_name
*/
void
gcc_jit_lvalue_set_register_name (gcc_jit_lvalue *lvalue,
				  const char *reg_name);

extern gcc_jit_lvalue *
gcc_jit_function_new_local (gcc_jit_function *func,
			    gcc_jit_location *loc,
			    gcc_jit_type *type,
			    const char *name);

extern gcc_jit_lvalue *
gcc_jit_function_new_temp (gcc_jit_function *func,
			   gcc_jit_location *loc,
			   gcc_jit_type *type);

#define LIBGCCJIT_HAVE_gcc_jit_function_new_temp

/**********************************************************************
 Statement-creation.
 **********************************************************************/

/* Add evaluation of an rvalue, discarding the result
   (e.g. a function call that "returns" void).

   This is equivalent to this C code:

     (void)expression;
*/
extern void
gcc_jit_block_add_eval (gcc_jit_block *block,
			gcc_jit_location *loc,
			gcc_jit_rvalue *rvalue);

/* Add evaluation of an rvalue, assigning the result to the given
   lvalue.

   This is roughly equivalent to this C code:

     lvalue = rvalue;
*/
extern void
gcc_jit_block_add_assignment (gcc_jit_block *block,
			      gcc_jit_location *loc,
			      gcc_jit_lvalue *lvalue,
			      gcc_jit_rvalue *rvalue);

/* Add evaluation of an rvalue, using the result to modify an
   lvalue.

   This is analogous to "+=" and friends:

     lvalue += rvalue;
     lvalue *= rvalue;
     lvalue /= rvalue;
   etc  */
extern void
gcc_jit_block_add_assignment_op (gcc_jit_block *block,
				 gcc_jit_location *loc,
				 gcc_jit_lvalue *lvalue,
				 enum gcc_jit_binary_op op,
				 gcc_jit_rvalue *rvalue);

/* Add a no-op textual comment to the internal representation of the
   code.  It will be optimized away, but will be visible in the dumps
   seen via
     GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE
   and
     GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE,
   and thus may be of use when debugging how your project's internal
   representation gets converted to the libgccjit IR.  */
extern void
gcc_jit_block_add_comment (gcc_jit_block *block,
			   gcc_jit_location *loc,
			   const char *text);

/* Terminate a block by adding evaluation of an rvalue, branching on the
   result to the appropriate successor block.

   This is roughly equivalent to this C code:

     if (boolval)
       goto on_true;
     else
       goto on_false;

   block, boolval, on_true, and on_false must be non-NULL.  */
extern void
gcc_jit_block_end_with_conditional (gcc_jit_block *block,
				    gcc_jit_location *loc,
				    gcc_jit_rvalue *boolval,
				    gcc_jit_block *on_true,
				    gcc_jit_block *on_false);

/* Terminate a block by adding a jump to the given target block.

   This is roughly equivalent to this C code:

      goto target;
*/
extern void
gcc_jit_block_end_with_jump (gcc_jit_block *block,
			     gcc_jit_location *loc,
			     gcc_jit_block *target);

/* Terminate a block by adding evaluation of an rvalue, returning the value.

   This is roughly equivalent to this C code:

      return expression;
*/
extern void
gcc_jit_block_end_with_return (gcc_jit_block *block,
			       gcc_jit_location *loc,
			       gcc_jit_rvalue *rvalue);

/* Terminate a block by adding a valueless return, for use within a function
   with "void" return type.

   This is equivalent to this C code:

      return;
*/
extern void
gcc_jit_block_end_with_void_return (gcc_jit_block *block,
				    gcc_jit_location *loc);

/* Create a new gcc_jit_case instance for use in a switch statement.
   min_value and max_value must be constants of integer type.

   This API entrypoint was added in LIBGCCJIT_ABI_3; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS
*/
extern gcc_jit_case *
gcc_jit_context_new_case (gcc_jit_context *ctxt,
			  gcc_jit_rvalue *min_value,
			  gcc_jit_rvalue *max_value,
			  gcc_jit_block *dest_block);

/* Upcasting from case to object.

   This API entrypoint was added in LIBGCCJIT_ABI_3; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS
*/

extern gcc_jit_object *
gcc_jit_case_as_object (gcc_jit_case *case_);

/* Terminate a block by adding evalation of an rvalue, then performing
   a multiway branch.

   This is roughly equivalent to this C code:

     switch (expr)
       {
       default:
	 goto default_block;

       case C0.min_value ... C0.max_value:
	 goto C0.dest_block;

       case C1.min_value ... C1.max_value:
	 goto C1.dest_block;

       ...etc...

       case C[N - 1].min_value ... C[N - 1].max_value:
	 goto C[N - 1].dest_block;
     }

   block, expr, default_block and cases must all be non-NULL.

   expr must be of the same integer type as all of the min_value
   and max_value within the cases.

   num_cases must be >= 0.

   The ranges of the cases must not overlap (or have duplicate
   values).

   This API entrypoint was added in LIBGCCJIT_ABI_3; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS
*/

extern void
gcc_jit_block_end_with_switch (gcc_jit_block *block,
			       gcc_jit_location *loc,
			       gcc_jit_rvalue *expr,
			       gcc_jit_block *default_block,
			       int num_cases,
			       gcc_jit_case **cases);

/* Pre-canned feature macro to indicate the presence of
   gcc_jit_block_end_with_switch, gcc_jit_case_as_object, and
   gcc_jit_context_new_case.

   This can be tested for with #ifdef.  */
#define LIBGCCJIT_HAVE_SWITCH_STATEMENTS

/**********************************************************************
 Nested contexts.
 **********************************************************************/

/* Given an existing JIT context, create a child context.

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
   there will likely be a performance hit for such nesting.  */

extern gcc_jit_context *
gcc_jit_context_new_child_context (gcc_jit_context *parent_ctxt);

/**********************************************************************
 Implementation support.
 **********************************************************************/

/* Write C source code into "path" that can be compiled into a
   self-contained executable (i.e. with libgccjit as the only dependency).
   The generated code will attempt to replay the API calls that have been
   made into the given context.

   This may be useful when debugging the library or client code, for
   reducing a complicated recipe for reproducing a bug into a simpler
   form.

   Typically you need to supply the option "-Wno-unused-variable" when
   compiling the generated file (since the result of each API call is
   assigned to a unique variable within the generated C source, and not
   all are necessarily then used).  */

extern void
gcc_jit_context_dump_reproducer_to_file (gcc_jit_context *ctxt,
					 const char *path);

/* Enable the dumping of a specific set of internal state from the
   compilation, capturing the result in-memory as a buffer.

   Parameter "dumpname" corresponds to the equivalent gcc command-line
   option, without the "-fdump-" prefix.
   For example, to get the equivalent of "-fdump-tree-vrp1", supply
   "tree-vrp1".
   The context directly stores the dumpname as a (const char *), so the
   passed string must outlive the context.

   gcc_jit_context_compile and gcc_jit_context_to_file
   will capture the dump as a dynamically-allocated buffer, writing
   it to ``*out_ptr``.

   The caller becomes responsible for calling
      free (*out_ptr)
   each time that gcc_jit_context_compile or gcc_jit_context_to_file
   are called.  *out_ptr will be written to, either with the address of a
   buffer, or with NULL if an error occurred.

   This API entrypoint is likely to be less stable than the others.
   In particular, both the precise dumpnames, and the format and content
   of the dumps are subject to change.

   It exists primarily for writing the library's own test suite.  */

extern void
gcc_jit_context_enable_dump (gcc_jit_context *ctxt,
			     const char *dumpname,
			     char **out_ptr);

/**********************************************************************
 Timing support.
 **********************************************************************/

/* The timing API was added in LIBGCCJIT_ABI_4; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_TIMING_API
*/
#define LIBGCCJIT_HAVE_TIMING_API

typedef struct gcc_jit_timer gcc_jit_timer;

/* Create a gcc_jit_timer instance, and start timing.

   This API entrypoint was added in LIBGCCJIT_ABI_4; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_TIMING_API
*/
extern gcc_jit_timer *
gcc_jit_timer_new (void);

/* Release a gcc_jit_timer instance.

   This API entrypoint was added in LIBGCCJIT_ABI_4; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_TIMING_API
*/
extern void
gcc_jit_timer_release (gcc_jit_timer *timer);

/* Associate a gcc_jit_timer instance with a context.

   This API entrypoint was added in LIBGCCJIT_ABI_4; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_TIMING_API
*/
extern void
gcc_jit_context_set_timer (gcc_jit_context *ctxt,
			   gcc_jit_timer *timer);

/* Get the timer associated with a context (if any).

   This API entrypoint was added in LIBGCCJIT_ABI_4; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_TIMING_API
*/

extern gcc_jit_timer *
gcc_jit_context_get_timer (gcc_jit_context *ctxt);

/* Push the given item onto the timing stack.

   This API entrypoint was added in LIBGCCJIT_ABI_4; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_TIMING_API
*/

extern void
gcc_jit_timer_push (gcc_jit_timer *timer,
		    const char *item_name);

/* Pop the top item from the timing stack.

   This API entrypoint was added in LIBGCCJIT_ABI_4; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_TIMING_API
*/

extern void
gcc_jit_timer_pop (gcc_jit_timer *timer,
		   const char *item_name);

/* Print timing information to the given stream about activity since
   the timer was started.

   This API entrypoint was added in LIBGCCJIT_ABI_4; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_TIMING_API
*/

extern void
gcc_jit_timer_print (gcc_jit_timer *timer,
		     FILE *f_out);


#define LIBGCCJIT_HAVE_gcc_jit_rvalue_set_bool_require_tail_call

/* Mark/clear a call as needing tail-call optimization.

   This API entrypoint was added in LIBGCCJIT_ABI_6; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_rvalue_set_bool_require_tail_call
*/
extern void
gcc_jit_rvalue_set_bool_require_tail_call (gcc_jit_rvalue *call,
					   int require_tail_call);

#define LIBGCCJIT_HAVE_gcc_jit_type_get_aligned

/* Given type "T", get type:

     T __attribute__ ((aligned (ALIGNMENT_IN_BYTES)))

   The alignment must be a power of two.

   This API entrypoint was added in LIBGCCJIT_ABI_7; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_type_get_aligned
*/
extern gcc_jit_type *
gcc_jit_type_get_aligned (gcc_jit_type *type,
			  size_t alignment_in_bytes);

#define LIBGCCJIT_HAVE_gcc_jit_type_get_vector

/* Given type "T", get type:

     T  __attribute__ ((vector_size (sizeof(T) * num_units))

   T must be integral/floating point; num_units must be a power of two.

   This API entrypoint was added in LIBGCCJIT_ABI_8; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_type_get_vector
*/
extern gcc_jit_type *
gcc_jit_type_get_vector (gcc_jit_type *type, size_t num_units);


#define LIBGCCJIT_HAVE_gcc_jit_function_get_address

/* Get the address of a function as an rvalue, of function pointer
   type.

   This API entrypoint was added in LIBGCCJIT_ABI_9; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_function_get_address
*/
extern gcc_jit_rvalue *
gcc_jit_function_get_address (gcc_jit_function *fn,
			      gcc_jit_location *loc);


#define LIBGCCJIT_HAVE_gcc_jit_context_new_rvalue_from_vector

/* Build a vector rvalue from an array of elements.

   "vec_type" should be a vector type, created using gcc_jit_type_get_vector.

   This API entrypoint was added in LIBGCCJIT_ABI_10; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_rvalue_from_vector
*/
extern gcc_jit_rvalue *
gcc_jit_context_new_rvalue_from_vector (gcc_jit_context *ctxt,
					gcc_jit_location *loc,
					gcc_jit_type *vec_type,
					size_t num_elements,
					gcc_jit_rvalue **elements);

#define LIBGCCJIT_HAVE_gcc_jit_version

/* Functions to retrieve libgccjit version.
   Analogous to __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__ in C code.

   These API entrypoints were added in LIBGCCJIT_ABI_13; you can test for their
   presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_version
 */
extern int
gcc_jit_version_major (void);
extern int
gcc_jit_version_minor (void);
extern int
gcc_jit_version_patchlevel (void);

/**********************************************************************
 Asm support.
 **********************************************************************/

/* Functions for adding inline assembler code, analogous to GCC's
   "extended asm" syntax.

   See https://gcc.gnu.org/onlinedocs/gcc/Using-Assembly-Language-with-C.html

   These API entrypoints were added in LIBGCCJIT_ABI_15; you can test for their
   presence using
     #ifdef LIBGCCJIT_HAVE_ASM_STATEMENTS
*/

#define LIBGCCJIT_HAVE_ASM_STATEMENTS

/* Create a gcc_jit_extended_asm for an extended asm statement
   with no control flow (i.e. without the goto qualifier).

   The asm_template parameter  corresponds to the AssemblerTemplate
   within C's extended asm syntax.  It must be non-NULL.  */

extern gcc_jit_extended_asm *
gcc_jit_block_add_extended_asm (gcc_jit_block *block,
				gcc_jit_location *loc,
				const char *asm_template);

/* Create a gcc_jit_extended_asm for an extended asm statement
   that may perform jumps, and use it to terminate the given block.
   This is equivalent to the "goto" qualifier in C's extended asm
   syntax.  */

extern gcc_jit_extended_asm *
gcc_jit_block_end_with_extended_asm_goto (gcc_jit_block *block,
					  gcc_jit_location *loc,
					  const char *asm_template,
					  int num_goto_blocks,
					  gcc_jit_block **goto_blocks,
					  gcc_jit_block *fallthrough_block);

/* Upcasting from extended asm to object.  */

extern gcc_jit_object *
gcc_jit_extended_asm_as_object (gcc_jit_extended_asm *ext_asm);

/* Set whether the gcc_jit_extended_asm has side-effects, equivalent to
   the "volatile" qualifier in C's extended asm syntax.  */

extern void
gcc_jit_extended_asm_set_volatile_flag (gcc_jit_extended_asm *ext_asm,
					int flag);

/* Set the equivalent of the "inline" qualifier in C's extended asm
   syntax.  */

extern void
gcc_jit_extended_asm_set_inline_flag (gcc_jit_extended_asm *ext_asm,
				      int flag);

/* Add an output operand to the extended asm statement.
   "asm_symbolic_name" can be NULL.
   "constraint" and "dest" must be non-NULL.
   This function can't be called on an "asm goto" as such instructions
   can't have outputs  */

extern void
gcc_jit_extended_asm_add_output_operand (gcc_jit_extended_asm *ext_asm,
					 const char *asm_symbolic_name,
					 const char *constraint,
					 gcc_jit_lvalue *dest);

/* Add an input operand to the extended asm statement.
   "asm_symbolic_name" can be NULL.
   "constraint" and "src" must be non-NULL.  */

extern void
gcc_jit_extended_asm_add_input_operand (gcc_jit_extended_asm *ext_asm,
					const char *asm_symbolic_name,
					const char *constraint,
					gcc_jit_rvalue *src);

/* Add "victim" to the list of registers clobbered by the extended
   asm statement.  It must be non-NULL.  */

extern void
gcc_jit_extended_asm_add_clobber (gcc_jit_extended_asm *ext_asm,
				  const char *victim);

/* Add "asm_stmts", a set of top-level asm statements, analogous to
   those created by GCC's "basic" asm syntax in C at file scope.  */

extern void
gcc_jit_context_add_top_level_asm (gcc_jit_context *ctxt,
				   gcc_jit_location *loc,
				   const char *asm_stmts);

#define LIBGCCJIT_HAVE_REFLECTION

/* Reflection functions to get the number of parameters, return type of
   a function and whether a type is a bool from the C API.

   This API entrypoint was added in LIBGCCJIT_ABI_16; you can test for its
   presence using
     #ifdef LIBGCCJIT_HAVE_REFLECTION
*/
/* Get the return type of a function.  */
extern gcc_jit_type *
gcc_jit_function_get_return_type (gcc_jit_function *func);

/* Get the number of params of a function.  */
extern size_t
gcc_jit_function_get_param_count (gcc_jit_function *func);

/* Get the element type of an array type or NULL if it's not an array.  */
extern gcc_jit_type *
gcc_jit_type_dyncast_array (gcc_jit_type *type);

/* Return non-zero if the type is a bool.  */
extern int
gcc_jit_type_is_bool (gcc_jit_type *type);

/* Return the function type if it is one or NULL.  */
extern gcc_jit_function_type *
gcc_jit_type_dyncast_function_ptr_type (gcc_jit_type *type);

/* Given a function type, return its return type.  */
extern gcc_jit_type *
gcc_jit_function_type_get_return_type (gcc_jit_function_type *function_type);

/* Given a function type, return its number of parameters.  */
extern size_t
gcc_jit_function_type_get_param_count (gcc_jit_function_type *function_type);

/* Given a function type, return the type of the specified parameter.  */
extern gcc_jit_type *
gcc_jit_function_type_get_param_type (gcc_jit_function_type *function_type,
				size_t index);

/* Return non-zero if the type is an integral.  */
extern int
gcc_jit_type_is_integral (gcc_jit_type *type);

/* Return the type pointed by the pointer type or NULL if it's not a
 * pointer.  */
extern gcc_jit_type *
gcc_jit_type_is_pointer (gcc_jit_type *type);

/* Given a type, return a dynamic cast to a vector type or NULL.  */
extern gcc_jit_vector_type *
gcc_jit_type_dyncast_vector (gcc_jit_type *type);

/* Given a type, return a dynamic cast to a struct type or NULL.  */
extern gcc_jit_struct *
gcc_jit_type_is_struct (gcc_jit_type *type);

/* Given a vector type, return the number of units it contains.  */
extern size_t
gcc_jit_vector_type_get_num_units (gcc_jit_vector_type *vector_type);

/* Given a vector type, return the type of its elements.  */
extern gcc_jit_type *
gcc_jit_vector_type_get_element_type (gcc_jit_vector_type *vector_type);

/* Given a type, return the unqualified type, removing "const", "volatile"
 * and alignment qualifiers.  */
extern gcc_jit_type *
gcc_jit_type_unqualified (gcc_jit_type *type);

#define LIBGCCJIT_HAVE_ATTRIBUTES

/* Function attributes.  */
enum gcc_jit_fn_attribute
{
  GCC_JIT_FN_ATTRIBUTE_ALIAS,
  GCC_JIT_FN_ATTRIBUTE_ALWAYS_INLINE,
  GCC_JIT_FN_ATTRIBUTE_INLINE,
  GCC_JIT_FN_ATTRIBUTE_NOINLINE,
  GCC_JIT_FN_ATTRIBUTE_TARGET,
  GCC_JIT_FN_ATTRIBUTE_USED,
  GCC_JIT_FN_ATTRIBUTE_VISIBILITY,
  GCC_JIT_FN_ATTRIBUTE_COLD,
  GCC_JIT_FN_ATTRIBUTE_RETURNS_TWICE,
  GCC_JIT_FN_ATTRIBUTE_PURE,
  GCC_JIT_FN_ATTRIBUTE_CONST,
  GCC_JIT_FN_ATTRIBUTE_WEAK,
  GCC_JIT_FN_ATTRIBUTE_NONNULL,

  /* Maximum value of this enum, should always be last. */
  GCC_JIT_FN_ATTRIBUTE_MAX,
};

/* Add an attribute to a function.  */
extern void
gcc_jit_function_add_attribute (gcc_jit_function *func,
				enum gcc_jit_fn_attribute attribute);

extern void
gcc_jit_function_add_string_attribute (gcc_jit_function *func,
				       enum gcc_jit_fn_attribute attribute,
				       const char* value);

extern void
gcc_jit_function_add_integer_array_attribute (
  gcc_jit_function *func,
  enum gcc_jit_fn_attribute attribute,
  const int* value,
  size_t length);

/* Variable attributes.  */
enum gcc_jit_variable_attribute
{
  GCC_JIT_VARIABLE_ATTRIBUTE_VISIBILITY,

  /* Maximum value of this enum, should always be last. */
  GCC_JIT_VARIABLE_ATTRIBUTE_MAX,
};

/* Add a string attribute to a variable.  */
extern void
gcc_jit_lvalue_add_string_attribute (gcc_jit_lvalue *variable,
				     enum gcc_jit_variable_attribute attribute,
				     const char* value);

extern void
gcc_jit_context_set_output_ident (gcc_jit_context *ctxt,
				  const char* output_ident);

#define LIBGCCJIT_HAVE_gcc_jit_context_set_output_ident

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif  /* LIBGCCJIT_H  */
