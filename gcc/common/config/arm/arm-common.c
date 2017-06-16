/* Common hooks for ARM.
   Copyright (C) 1991-2017 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "memmodel.h"
#include "tm_p.h"
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"
#include "sbitmap.h"
#include "diagnostic.h"

/* Set default optimization options.  */
static const struct default_options arm_option_optimization_table[] =
  {
    /* Enable section anchors by default at -O1 or higher.  */
    { OPT_LEVELS_1_PLUS, OPT_fsection_anchors, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fomit_frame_pointer, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fsched_pressure, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

/* Implement TARGET_EXCEPT_UNWIND_INFO.  */

enum unwind_info_type
arm_except_unwind_info (struct gcc_options *opts)
{
  /* Honor the --enable-sjlj-exceptions configure switch.  */
#ifdef CONFIG_SJLJ_EXCEPTIONS
  if (CONFIG_SJLJ_EXCEPTIONS)
    return UI_SJLJ;
#endif

  /* If not using ARM EABI unwind tables... */
  if (ARM_UNWIND_INFO)
    {
      /* For simplicity elsewhere in this file, indicate that all unwind
	 info is disabled if we're not emitting unwind tables.  */
      if (!opts->x_flag_exceptions && !opts->x_flag_unwind_tables)
	return UI_NONE;
      else
	return UI_TARGET;
    }

  /* ... we use sjlj exceptions for backwards compatibility.  */
  return UI_SJLJ;
}

#define ARM_CPU_NAME_LENGTH 20

/* Truncate NAME at the first '.' or '+' character seen, or return
   NAME unmodified.  */

const char *
arm_rewrite_selected_cpu (const char *name)
{
  static char output_buf[ARM_CPU_NAME_LENGTH + 1] = {0};
  char *arg_pos;

  strncpy (output_buf, name, ARM_CPU_NAME_LENGTH);
  output_buf[ARM_CPU_NAME_LENGTH] = 0;

  arg_pos = strchr (output_buf, '.');

  /* If we found a '.' truncate the entry at that point.  */
  if (arg_pos)
    *arg_pos = '\0';

  arg_pos = strchr (output_buf, '+');

  /* If we found a '+' truncate the entry at that point.  */
  if (arg_pos)
    *arg_pos = '\0';

  return output_buf;
}

/* Called by the driver to rewrite a name passed to the -mcpu
   argument in preparation to be passed to the assembler.  The
   names passed from the command line will be in ARGV, we want
   to use the right-most argument, which should be in
   ARGV[ARGC - 1].  ARGC should always be greater than 0.  */

const char *
arm_rewrite_mcpu (int argc, const char **argv)
{
  gcc_assert (argc);
  return arm_rewrite_selected_cpu (argv[argc - 1]);
}

/* Truncate NAME at the first '+' character seen, or return
   NAME unmodified.  Similar to arm_rewrite_selected_cpu, but we must
   preserve '.' as that is part of some architecture names.  */

const char *
arm_rewrite_selected_arch (const char *name)
{
  static char output_buf[ARM_CPU_NAME_LENGTH + 1] = {0};
  char *arg_pos;

  strncpy (output_buf, name, ARM_CPU_NAME_LENGTH);
  output_buf[ARM_CPU_NAME_LENGTH] = 0;

  arg_pos = strchr (output_buf, '+');

  /* If we found a '+' truncate the entry at that point.  */
  if (arg_pos)
    *arg_pos = '\0';

  return output_buf;
}

/* Called by the driver to rewrite a name passed to the -march
   argument in preparation to be passed to the assembler.  The
   names passed from the command line will be in ARGV, we want
   to use the right-most argument, which should be in
   ARGV[ARGC - 1].  ARGC should always be greater than 0.  */

const char *
arm_rewrite_march (int argc, const char **argv)
{
  gcc_assert (argc);
  return arm_rewrite_selected_arch (argv[argc - 1]);
}

#include "config/arm/arm-cpu-cdata.h"

/* Scan over a raw feature array BITS checking for BIT being present.
   This is slower than the normal bitmask checks, but we would spend longer
   initializing that than doing the check this way.  Returns true iff
   BIT is found.  */
static bool
check_isa_bits_for (const enum isa_feature* bits, enum isa_feature bit)
{
  while (*bits != isa_nobit)
    if (*bits++ == bit)
      return true;

  return false;
}

/* Called by the driver to check whether the target denoted by current
   command line options is a Thumb-only target.  ARGV is an array of
   tupples (normally only one) where the first element of the tupple
   is 'cpu' or 'arch' and the second is the option passed to the
   compiler for that.  An architecture tupple is always taken in
   preference to a cpu tupple and the last of each type always
   overrides any earlier setting.  */

const char *
arm_target_thumb_only (int argc, const char **argv)
{
  const char *arch = NULL;
  const char *cpu = NULL;

  if (argc % 2 != 0)
    fatal_error (input_location,
		 "%%:target_mode_check takes an even number of parameters");

  while (argc)
    {
      if (strcmp (argv[0], "arch") == 0)
	arch = argv[1];
      else if (strcmp (argv[0], "cpu") == 0)
	cpu = argv[1];
      else
	fatal_error (input_location,
		     "unrecognized option passed to %%:target_mode_check");
      argc -= 2;
      argv += 2;
    }

  /* No architecture, or CPU, has option extensions that change
     whether or not we have a Thumb-only device, so there is no need
     to scan any option extensions specified.  */

  /* If the architecture is specified, that overrides any CPU setting.  */
  if (arch)
    {
      const arch_option *arch_opt
	= arm_parse_arch_option_name (all_architectures, "-march", arch);

      if (arch_opt && !check_isa_bits_for (arch_opt->common.isa_bits,
					   isa_bit_notm))
	return "-mthumb";
    }
  else if (cpu)
    {
      const cpu_option *cpu_opt
	= arm_parse_cpu_option_name (all_cores, "-mcpu", cpu);

      if (cpu_opt && !check_isa_bits_for (cpu_opt->common.isa_bits,
					  isa_bit_notm))
	return "-mthumb";
    }

  return NULL;
}

/* List the permitted CPU option names.  If TARGET is a near miss for an
   entry, print out the suggested alternative.  */
static void
arm_print_hint_for_cpu_option (const char *target,
			       const cpu_option *list)
{
  auto_vec<const char*> candidates;
  for (; list->common.name != NULL; list++)
    candidates.safe_push (list->common.name);
  char *s;
  const char *hint = candidates_list_and_hint (target, s, candidates);
  if (hint)
    inform (input_location, "valid arguments are: %s; did you mean %qs?",
	    s, hint);
  else
    inform (input_location, "valid arguments are: %s", s);

  XDELETEVEC (s);
}

/* Parse the base component of a CPU selection in LIST.  Return a
   pointer to the entry in the architecture table.  OPTNAME is the
   name of the option we are parsing and can be used if a diagnostic
   is needed.  */
const cpu_option *
arm_parse_cpu_option_name (const cpu_option *list, const char *optname,
			   const char *target)
{
  const cpu_option *entry;
  const char *end  = strchr (target, '+');
  size_t len = end ? end - target : strlen (target);

  for (entry = list; entry->common.name != NULL; entry++)
    {
      if (strncmp (entry->common.name, target, len) == 0
	  && entry->common.name[len] == '\0')
	return entry;
    }

  error_at (input_location, "unrecognized %s target: %s", optname, target);
  arm_print_hint_for_cpu_option (target, list);
  return NULL;
}

/* List the permitted architecture option names.  If TARGET is a near
   miss for an entry, print out the suggested alternative.  */
static void
arm_print_hint_for_arch_option (const char *target,
			       const arch_option *list)
{
  auto_vec<const char*> candidates;
  for (; list->common.name != NULL; list++)
    candidates.safe_push (list->common.name);
  char *s;
  const char *hint = candidates_list_and_hint (target, s, candidates);
  if (hint)
    inform (input_location, "valid arguments are: %s; did you mean %qs?",
	    s, hint);
  else
    inform (input_location, "valid arguments are: %s", s);

  XDELETEVEC (s);
}

/* Parse the base component of a CPU or architecture selection in
   LIST.  Return a pointer to the entry in the architecture table.
   OPTNAME is the name of the option we are parsing and can be used if
   a diagnostic is needed.  */
const arch_option *
arm_parse_arch_option_name (const arch_option *list, const char *optname,
			    const char *target)
{
  const arch_option *entry;
  const char *end  = strchr (target, '+');
  size_t len = end ? end - target : strlen (target);

  for (entry = list; entry->common.name != NULL; entry++)
    {
      if (strncmp (entry->common.name, target, len) == 0
	  && entry->common.name[len] == '\0')
	return entry;
    }

  error_at (input_location, "unrecognized %s target: %s", optname, target);
  arm_print_hint_for_arch_option (target, list);
  return NULL;
}

/* Convert a static initializer array of feature bits to sbitmap
   representation.  */
void
arm_initialize_isa (sbitmap isa, const enum isa_feature *isa_bits)
{
  bitmap_clear (isa);
  while (*isa_bits != isa_nobit)
    bitmap_set_bit (isa, *(isa_bits++));
}

/* OPT isn't a recognized feature.  Print a suitable error message and
   suggest a possible value.  Always print the list of permitted
   values.  */
static void
arm_unrecognized_feature (const char *opt, size_t len,
			  const cpu_arch_option *target)
{
  char *this_opt = XALLOCAVEC (char, len+1);
  auto_vec<const char*> candidates;

  strncpy (this_opt, opt, len);
  this_opt[len] = 0;

  error_at (input_location, "%qs does not support feature %qs", target->name,
	    this_opt);
  for (const cpu_arch_extension *list = target->extensions;
       list->name != NULL;
       list++)
    candidates.safe_push (list->name);

  char *s;
  const char *hint = candidates_list_and_hint (this_opt, s, candidates);

  if (hint)
    inform (input_location, "valid feature names are: %s; did you mean %qs?",
	    s, hint);
  else
    inform (input_location, "valid feature names are: %s", s);

  XDELETEVEC (s);
}

/* Parse any feature extensions to add to (or remove from) the
   permitted ISA selection.  */
void
arm_parse_option_features (sbitmap isa, const cpu_arch_option *target,
			   const char *opts_in)
{
  const char *opts = opts_in;

  if (!opts)
    return;

  if (!target->extensions)
    {
      error_at (input_location, "%s does not take any feature options",
		target->name);
      return;
    }

  while (opts)
    {
      gcc_assert (*opts == '+');
      const struct cpu_arch_extension *entry;
      const char *end = strchr (++opts, '+');
      size_t len = end ? end - opts : strlen (opts);
      bool matched = false;

      for (entry = target->extensions;
	   !matched && entry->name != NULL;
	   entry++)
	{
	  if (strncmp (entry->name, opts, len) == 0
	      && entry->name[len] == '\0')
	    {
	      if (isa)
		{
		  const enum isa_feature *f = entry->isa_bits;
		  if (entry->remove)
		    {
		      while (*f != isa_nobit)
			bitmap_clear_bit (isa, *(f++));
		    }
		  else
		    {
		      while (*f != isa_nobit)
			bitmap_set_bit (isa, *(f++));
		    }
		}
	      matched = true;
	    }
	}

      if (!matched)
	arm_unrecognized_feature (opts, len, target);

      opts = end;
    }
}

#undef ARM_CPU_NAME_LENGTH


#undef  TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (TARGET_DEFAULT | MASK_SCHED_PROLOG)

#undef  TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE arm_option_optimization_table

#undef TARGET_EXCEPT_UNWIND_INFO
#define TARGET_EXCEPT_UNWIND_INFO  arm_except_unwind_info

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
