/* Command line option handling.
   Copyright (C) 2002, 2003, 2004, 2005, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_OPTS_H
#define GCC_OPTS_H

#include "input.h"
#include "vec.h"

/* Specifies how a switch's VAR_VALUE relates to its FLAG_VAR.  */
enum cl_var_type {
  /* The switch is enabled when FLAG_VAR is nonzero.  */
  CLVC_BOOLEAN,

  /* The switch is enabled when FLAG_VAR == VAR_VALUE.  */
  CLVC_EQUAL,

  /* The switch is enabled when VAR_VALUE is not set in FLAG_VAR.  */
  CLVC_BIT_CLEAR,

  /* The switch is enabled when VAR_VALUE is set in FLAG_VAR.  */
  CLVC_BIT_SET,

  /* The switch takes a string argument and FLAG_VAR points to that
     argument.  */
  CLVC_STRING,

  /* The switch takes an enumerated argument (VAR_ENUM says what
     enumeration) and FLAG_VAR points to that argument.  */
  CLVC_ENUM,

  /* The switch should be stored in the VEC pointed to by FLAG_VAR for
     later processing.  */
  CLVC_DEFER
};

struct cl_option
{
  const char *opt_text;
  const char *help;
  const char *missing_argument_error;
  const char *warn_message;
  const char *alias_arg;
  const char *neg_alias_arg;
  unsigned short alias_target;
  unsigned short back_chain;
  unsigned char opt_len;
  int neg_index;
  unsigned int flags;
  unsigned short flag_var_offset;
  unsigned short var_enum;
  enum cl_var_type var_type;
  int var_value;
};

/* Records that the state of an option consists of SIZE bytes starting
   at DATA.  DATA might point to CH in some cases.  */
struct cl_option_state {
  const void *data;
  size_t size;
  char ch;
};

extern const struct cl_option cl_options[];
extern const unsigned int cl_options_count;
extern const char *const lang_names[];
extern const unsigned int cl_lang_count;

#define CL_PARAMS               (1 << 11) /* Fake entry.  Used to display --param info with --help.  */
#define CL_WARNING		(1 << 12) /* Enables an (optional) warning message.  */
#define CL_OPTIMIZATION		(1 << 13) /* Enables an (optional) optimization.  */
#define CL_DRIVER		(1 << 14) /* Driver option.  */
#define CL_TARGET		(1 << 15) /* Target-specific option.  */
#define CL_COMMON		(1 << 16) /* Language-independent.  */

#define CL_MIN_OPTION_CLASS	CL_PARAMS
#define CL_MAX_OPTION_CLASS	CL_COMMON

/* From here on the bits describe attributes of the options.
   Before this point the bits have described the class of the option.
   This distinction is important because --help will not list options
   which only have these higher bits set.  */

/* Options marked with CL_SEPARATE take a number of separate arguments
   (1 to 4) that is one more than the number in this bit-field.  */
#define CL_SEPARATE_NARGS_SHIFT	17
#define CL_SEPARATE_NARGS_MASK	(3 << CL_SEPARATE_NARGS_SHIFT)

#define CL_SEPARATE_ALIAS	(1 << 19) /* Option is an alias when used with separate argument.  */
#define CL_NO_DRIVER_ARG	(1 << 20) /* Option takes no argument in the driver.  */
#define CL_REJECT_DRIVER	(1 << 21) /* Reject this option in the driver.  */
#define CL_SAVE			(1 << 22) /* Target-specific option for attribute.  */
#define CL_DISABLED		(1 << 23) /* Disabled in this configuration.  */
#define CL_REPORT		(1 << 24) /* Report argument with -fverbose-asm  */
#define CL_JOINED		(1 << 25) /* If takes joined argument.  */
#define CL_SEPARATE		(1 << 26) /* If takes a separate argument.  */
#define CL_REJECT_NEGATIVE	(1 << 27) /* Reject no- form.  */
#define CL_MISSING_OK		(1 << 28) /* Missing argument OK (joined).  */
#define CL_UINTEGER		(1 << 29) /* Argument is an integer >=0.  */
#define CL_UNDOCUMENTED		(1 << 30) /* Do not output with --help.  */

/* Flags for an enumerated option argument.  */
#define CL_ENUM_CANONICAL	(1 << 0) /* Canonical for this value.  */
#define CL_ENUM_DRIVER_ONLY	(1 << 1) /* Only accepted in the driver.  */

/* Structure describing an enumerated option argument.  */

struct cl_enum_arg
{
  /* The argument text, or NULL at the end of the array.  */
  const char *arg;

  /* The corresponding integer value.  */
  int value;

  /* Flags associated with this argument.  */
  unsigned int flags;
};

/* Structure describing an enumerated set of option arguments.  */

struct cl_enum
{
  /* Help text, or NULL if the values should not be listed in --help
     output.  */
  const char *help;

  /* Error message for unknown arguments, or NULL to use a generic
     error.  */
  const char *unknown_error;

  /* Array of possible values.  */
  const struct cl_enum_arg *values;

  /* The size of the type used to store a value.  */
  size_t var_size;

  /* Function to set a variable of this type.  */
  void (*set) (void *var, int value);

  /* Function to get the value of a variable of this type.  */
  int (*get) (const void *var);
};

extern const struct cl_enum cl_enums[];
extern const unsigned int cl_enums_count;

/* Possible ways in which a command-line option may be erroneous.
   These do not include not being known at all; an option index of
   OPT_SPECIAL_unknown is used for that.  */

#define CL_ERR_DISABLED		(1 << 0) /* Disabled in this configuration.  */
#define CL_ERR_MISSING_ARG	(1 << 1) /* Argument required but missing.  */
#define CL_ERR_WRONG_LANG	(1 << 2) /* Option for wrong language.  */
#define CL_ERR_UINT_ARG		(1 << 3) /* Bad unsigned integer argument.  */
#define CL_ERR_ENUM_ARG		(1 << 4) /* Bad enumerated argument.  */
#define CL_ERR_NEGATIVE		(1 << 5) /* Negative form of option
					    not permitted (together
					    with OPT_SPECIAL_unknown).  */

/* Structure describing the result of decoding an option.  */

struct cl_decoded_option
{
  /* The index of this option, or an OPT_SPECIAL_* value for
     non-options and unknown options.  */
  size_t opt_index;

  /* Any warning to give for use of this option, or NULL if none.  */
  const char *warn_message;

  /* The string argument, or NULL if none.  For OPT_SPECIAL_* cases,
     the option or non-option command-line argument.  */
  const char *arg;

  /* The original text of option plus arguments, with separate argv
     elements concatenated into one string with spaces separating
     them.  This is for such uses as diagnostics and
     -frecord-gcc-switches.  */
  const char *orig_option_with_args_text;

  /* The canonical form of the option and its argument, for when it is
     necessary to reconstruct argv elements (in particular, for
     processing specs and passing options to subprocesses from the
     driver).  */
  const char *canonical_option[4];

  /* The number of elements in the canonical form of the option and
     arguments; always at least 1.  */
  size_t canonical_option_num_elements;

  /* For a boolean option, 1 for the true case and 0 for the "no-"
     case.  For an unsigned integer option, the value of the
     argument.  1 in all other cases.  */
  int value;

  /* Any flags describing errors detected in this option.  */
  int errors;
};

/* Structure describing an option deferred for handling after the main
   option handlers.  */

typedef struct
{
  /* Elements from struct cl_decoded_option used for deferred
     options.  */
  size_t opt_index;
  const char *arg;
  int value;
} cl_deferred_option;
DEF_VEC_O(cl_deferred_option);
DEF_VEC_ALLOC_O(cl_deferred_option,heap);

/* Structure describing a single option-handling callback.  */

struct cl_option_handler_func
{
  /* The function called to handle the option.  */
  bool (*handler) (struct gcc_options *opts,
		   struct gcc_options *opts_set,
		   const struct cl_decoded_option *decoded,
		   unsigned int lang_mask, int kind, location_t loc,
		   const struct cl_option_handlers *handlers,
		   diagnostic_context *dc);

  /* The mask that must have some bit in common with the flags for the
     option for this particular handler to be used.  */
  unsigned int mask;
};

/* Structure describing the callbacks used in handling options.  */

struct cl_option_handlers
{
  /* Callback for an unknown option to determine whether to give an
     error for it, and possibly store information to diagnose the
     option at a later point.  Return true if an error should be
     given, false otherwise.  */
  bool (*unknown_option_callback) (const struct cl_decoded_option *decoded);

  /* Callback to handle, and possibly diagnose, an option for another
     language.  */
  void (*wrong_lang_callback) (const struct cl_decoded_option *decoded,
			       unsigned int lang_mask);

  /* Callback to call after the successful handling of any option.  */
  void (*post_handling_callback) (const struct cl_decoded_option *decoded,
				  unsigned int mask);

  /* The number of individual handlers.  */
  size_t num_handlers;

  /* The handlers themselves.  */
  struct cl_option_handler_func handlers[3];
};

/* Input file names.  */

extern const char **in_fnames;

/* The count of input filenames.  */

extern unsigned num_in_fnames;

size_t find_opt (const char *input, int lang_mask);
extern int integral_argument (const char *arg);
extern bool enum_value_to_arg (const struct cl_enum_arg *enum_args,
			       const char **argp, int value,
			       unsigned int lang_mask);
extern void decode_cmdline_options_to_array (unsigned int argc,
					     const char **argv, 
					     unsigned int lang_mask,
					     struct cl_decoded_option **decoded_options,
					     unsigned int *decoded_options_count);
extern void init_options_once (void);
extern void init_options_struct (struct gcc_options *opts,
				 struct gcc_options *opts_set);
extern void decode_cmdline_options_to_array_default_mask (unsigned int argc,
							  const char **argv, 
							  struct cl_decoded_option **decoded_options,
							  unsigned int *decoded_options_count);
extern void set_default_handlers (struct cl_option_handlers *handlers);
extern void decode_options (struct gcc_options *opts,
			    struct gcc_options *opts_set,
			    struct cl_decoded_option *decoded_options,
			    unsigned int decoded_options_count,
			    location_t loc,
			    diagnostic_context *dc);
extern int option_enabled (int opt_idx, void *opts);
extern bool get_option_state (struct gcc_options *, int,
			      struct cl_option_state *);
extern void set_option (struct gcc_options *opts,
			struct gcc_options *opts_set,
			int opt_index, int value, const char *arg, int kind,
			location_t loc, diagnostic_context *dc);
extern void *option_flag_var (int opt_index, struct gcc_options *opts);
bool handle_generated_option (struct gcc_options *opts,
			      struct gcc_options *opts_set,
			      size_t opt_index, const char *arg, int value,
			      unsigned int lang_mask, int kind, location_t loc,
			      const struct cl_option_handlers *handlers,
			      diagnostic_context *dc);
void generate_option (size_t opt_index, const char *arg, int value,
		      unsigned int lang_mask,
		      struct cl_decoded_option *decoded);
void generate_option_input_file (const char *file,
				 struct cl_decoded_option *decoded);
extern void read_cmdline_option (struct gcc_options *opts,
				 struct gcc_options *opts_set,
				 struct cl_decoded_option *decoded,
				 location_t loc,
				 unsigned int lang_mask,
				 const struct cl_option_handlers *handlers,
				 diagnostic_context *dc);
extern void control_warning_option (unsigned int opt_index, int kind,
				    bool imply, location_t loc,
				    unsigned int lang_mask,
				    const struct cl_option_handlers *handlers,
				    struct gcc_options *opts,
				    struct gcc_options *opts_set,
				    diagnostic_context *dc);
extern void print_ignored_options (void);
extern void handle_common_deferred_options (void);
extern bool common_handle_option (struct gcc_options *opts,
				  struct gcc_options *opts_set,
				  const struct cl_decoded_option *decoded,
				  unsigned int lang_mask, int kind,
				  location_t loc,
				  const struct cl_option_handlers *handlers,
				  diagnostic_context *dc);
extern bool target_handle_option (struct gcc_options *opts,
				  struct gcc_options *opts_set,
				  const struct cl_decoded_option *decoded,
				  unsigned int lang_mask, int kind,
				  location_t loc,
				  const struct cl_option_handlers *handlers,
				  diagnostic_context *dc);
extern void finish_options (struct gcc_options *opts,
			    struct gcc_options *opts_set,
			    location_t loc);
extern void default_options_optimization (struct gcc_options *opts,
					  struct gcc_options *opts_set,
					  struct cl_decoded_option *decoded_options,
					  unsigned int decoded_options_count,
					  location_t loc,
					  unsigned int lang_mask,
					  const struct cl_option_handlers *handlers,
					  diagnostic_context *dc);
extern void set_struct_debug_option (struct gcc_options *opts,
				     location_t loc,
				     const char *value);
#endif
