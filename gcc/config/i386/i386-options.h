/* Copyright (C) 1988-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_I386_OPTIONS_H
#define GCC_I386_OPTIONS_H

extern int ix86_omp_device_kind_arch_isa (enum omp_device_kind_arch_isa trait,
					  const char *name);

char *ix86_target_string (HOST_WIDE_INT isa, HOST_WIDE_INT isa2,
			  int flags, int flags2,
			  const char *arch, const char *tune,
			  enum fpmath_unit fpmath,
			  enum prefer_vector_width pvw, bool add_nl_p,
			  bool add_abi_p);

extern enum attr_cpu ix86_schedule;

extern enum processor_type ix86_tune;
extern enum processor_type ix86_arch;
extern unsigned char x86_prefetch_sse;
extern const struct processor_costs *ix86_tune_cost;

extern int ix86_tune_defaulted;
extern int ix86_arch_specified;

extern unsigned int ix86_default_incoming_stack_boundary;
extern HOST_WIDE_INT deferred_isa_values;
extern HOST_WIDE_INT deferred_isa_values2;

extern unsigned int ix86_preferred_stack_boundary;
extern unsigned int ix86_user_incoming_stack_boundary;
extern unsigned int ix86_default_incoming_stack_boundary;
extern unsigned int ix86_incoming_stack_boundary;

extern char *ix86_offload_options (void);
extern void ix86_option_override (void);
extern void ix86_override_options_after_change (void);
void ix86_set_current_function (tree fndecl);
bool ix86_function_naked (const_tree fn);
void ix86_simd_clone_adjust (struct cgraph_node *node);

extern tree (*ix86_veclib_handler) (combined_fn, tree, tree);
extern tree ix86_veclibabi_svml (combined_fn, tree, tree);
extern tree ix86_veclibabi_acml (combined_fn, tree, tree);

enum ix86_function_specific_strings
{
  IX86_FUNCTION_SPECIFIC_ARCH,
  IX86_FUNCTION_SPECIFIC_TUNE,
  IX86_FUNCTION_SPECIFIC_MAX
};

extern const char *stringop_alg_names[];

void ix86_add_new_builtins (HOST_WIDE_INT isa, HOST_WIDE_INT isa2);
void ix86_function_specific_save (struct cl_target_option *,
				  struct gcc_options *opts);
void ix86_function_specific_restore (struct gcc_options *opts,
				     struct cl_target_option *);
void ix86_function_specific_post_stream_in (struct cl_target_option *);
void ix86_function_specific_print (FILE *, int,
				   struct cl_target_option *);
bool ix86_valid_target_attribute_p (tree, tree, tree, int);

extern const struct attribute_spec ix86_attribute_table[];


#endif  /* GCC_I386_OPTIONS_H */
