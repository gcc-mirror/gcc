/* Common hooks for ARM.
   Copyright (C) 1991-2023 Free Software Foundation, Inc.

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

#define INCLUDE_LIST
#define INCLUDE_VECTOR
#define INCLUDE_ALGORITHM
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

#include "configargs.h"

/* Set default optimization options.  */
static const struct default_options arm_option_optimization_table[] =
  {
    /* Enable section anchors by default at -O1 or higher.  */
    { OPT_LEVELS_1_PLUS, OPT_fsection_anchors, NULL, 1 },
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

  /* ... honor target configurations requesting DWARF2 EH...  */
#ifdef DWARF2_UNWIND_INFO
  if (DWARF2_UNWIND_INFO)
    return UI_DWARF2;
#endif

  /* ... or fallback to sjlj exceptions for backwards compatibility.  */
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

#ifdef HAVE_GAS_ARM_EXTENDED_ARCH
  return argv[argc - 1];
#else
  return arm_rewrite_selected_cpu (argv[argc - 1]);
#endif
}

/* Comparator for arm_rewrite_selected_arch.  Compare the two arch extension
   strings FIRST and SECOND and return TRUE if FIRST is less than SECOND
   alphabetically.  */

static bool
compare_opt_names (const char *first, const char *second)
{
  return strcmp (first, second) <= 0;
}

/* Rewrite the architecture string for passing to the assembler.
   Although the syntax is similar we cannot assume that it supports
   the newer FP related options.  So strip any option that only
   defines features in the standard -mfpu options out.  We'll generate
   a suitable -mfpu option elsewhere to carry that information.  NAME
   should already have been canonicalized, so we do not expect to
   encounter +no.. options that remove features.  A final problem is
   that the assembler expects the feature extensions to be listed
   alphabetically, so we build a list of required options and then
   sort them into canonical order in the resulting string.  */
const char *
arm_rewrite_selected_arch (const char *name)
{
  /* The result we return needs to be semi persistent, so handle being
     re-invoked.  */
  static char *asm_arch = NULL;

  if (asm_arch)
    {
      free (asm_arch);
      asm_arch = NULL;
    }

  const char *arg_pos = strchr (name, '+');

  /* No extension options? just return the original string.  */
  if (arg_pos == NULL)
    return name;

  const arch_option *arch_opt
    = arm_parse_arch_option_name (all_architectures, "-march", name);

  auto_sbitmap fpu_bits (isa_num_bits);
  static const enum isa_feature fpu_bitlist[]
    = { ISA_ALL_FPU_INTERNAL, isa_nobit };

  arm_initialize_isa (fpu_bits, fpu_bitlist);

  auto_sbitmap opt_bits (isa_num_bits);

  /* Ensure that the resulting string is large enough for the result.  We
     never add options, so using strdup here will ensure that.  */
  asm_arch = xstrdup (name);
  asm_arch[arg_pos - name] = '\0';

  std::vector<const char *>optlist;

  while (arg_pos)
    {
      const char *end = strchr (arg_pos + 1, '+');
      size_t len = end ? end - arg_pos : strlen (arg_pos);

      for (const cpu_arch_extension *entry = arch_opt->common.extensions;
	   entry->name != NULL;
	   entry++)
	{
	  if (strncmp (entry->name, arg_pos + 1, len - 1) == 0
	      && entry->name[len - 1] == '\0')
	    {
	      /* Don't expect removal options.  */
	      gcc_assert (!entry->remove);
	      arm_initialize_isa (opt_bits, entry->isa_bits);
	      if (!bitmap_subset_p (opt_bits, fpu_bits))
		optlist.push_back (entry->name);
	      bitmap_clear (opt_bits);
	      break;
	    }
	}

      arg_pos = end;
    }

  std::sort (optlist.begin (), optlist.end (), compare_opt_names);

  for (std::vector<const char *>::iterator opt_iter = optlist.begin ();
       opt_iter != optlist.end ();
       ++opt_iter)
    {
      strcat (asm_arch, "+");
      strcat (asm_arch, (*opt_iter));
    }

  return asm_arch;
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

#ifdef HAVE_GAS_ARM_EXTENDED_ARCH
  return argv[argc - 1];
#else
  return arm_rewrite_selected_arch (argv[argc - 1]);
#endif
}

#include "arm-cpu-cdata.h"

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

/* Look up NAME in the configuration defaults for this build of the
   the compiler.  Return the value associated with that name, or NULL
   if no value is found.  */
static const char *
arm_config_default (const char *name)
{
  unsigned i;

  if (configure_default_options[0].name == NULL)
    return NULL;

  for (i = 0; i < ARRAY_SIZE (configure_default_options); i++)
    if (strcmp (configure_default_options[i].name, name) == 0)
      return configure_default_options[i].value;

  return NULL;
}

/* Called by the driver to check whether the target denoted by current
   command line options is a Thumb-only, or ARM-only, target.  ARGV is
   an array of tupples (normally only one) where the first element of
   the tupple is 'cpu' or 'arch' and the second is the option passed
   to the compiler for that.  An architecture tupple is always taken
   in preference to a cpu tupple and the last of each type always
   overrides any earlier setting.  */

const char *
arm_target_mode (int argc, const char **argv)
{
  const char *arch = NULL;
  const char *cpu = NULL;

  if (argc % 2 != 0)
    fatal_error (input_location,
		 "%%:%<target_mode_check%> takes an even number of parameters");

  while (argc)
    {
      if (strcmp (argv[0], "arch") == 0)
	arch = argv[1];
      else if (strcmp (argv[0], "cpu") == 0)
	cpu = argv[1];
      else
	fatal_error (input_location, "unrecognized option passed to %%:"
		     "%<target_mode_check%>");
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
	= arm_parse_arch_option_name (all_architectures, "-march", arch,
				      false);

      if (arch_opt && !check_isa_bits_for (arch_opt->common.isa_bits,
					   isa_bit_notm))
	return "-mthumb";
      if (arch_opt && !check_isa_bits_for (arch_opt->common.isa_bits,
					   isa_bit_thumb))
	return "-marm";
    }
  else if (cpu)
    {
      const cpu_option *cpu_opt
	= arm_parse_cpu_option_name (all_cores, "-mcpu", cpu, false);

      if (cpu_opt && !check_isa_bits_for (cpu_opt->common.isa_bits,
					  isa_bit_notm))
	return "-mthumb";
      if (cpu_opt && !check_isa_bits_for (cpu_opt->common.isa_bits,
					   isa_bit_thumb))
	return "-marm";
    }

  const char *default_mode = arm_config_default ("mode");
  if (default_mode)
    {
      if (strcmp (default_mode, "thumb") == 0)
	return "-mthumb";
      else if (strcmp (default_mode, "arm") == 0)
	return "-marm";
      else
	gcc_unreachable ();
    }

  /* Compiler hasn't been configured with a default, and the CPU
     doesn't require Thumb, so default to ARM.  */
  return "-marm";
}

/* List the permitted CPU option names.  If TARGET is a near miss for an
   entry, print out the suggested alternative.  */
static void
arm_print_hint_for_cpu_option (const char *target,
			       const cpu_option *list)
{
  auto_vec<const char*> candidates;
  for (; list->common.name != NULL; list++)
    {
      candidates.safe_push (list->common.name);
      if (list->aliases)
	{
	  for (const cpu_alias *alias = list->aliases; alias->name != NULL;
	       alias++)
	    if (alias->visible)
	      candidates.safe_push (alias->name);
	}
    }

#ifdef HAVE_LOCAL_CPU_DETECT
  /* Add also "native" as possible value.  */
  candidates.safe_push ("native");
#endif

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
   is needed.  If COMPLAIN is true (the default) emit error
   messages and hints on invalid input.  */
const cpu_option *
arm_parse_cpu_option_name (const cpu_option *list, const char *optname,
			   const char *target, bool complain)
{
  const cpu_option *entry;
  const char *end  = strchr (target, '+');
  size_t len = end ? end - target : strlen (target);

  for (entry = list; entry->common.name != NULL; entry++)
    {
      if (strncmp (entry->common.name, target, len) == 0
	  && entry->common.name[len] == '\0')
	return entry;

      /* Match against any legal alias for this CPU candidate.  */
      if (entry->aliases)
	{
	  for (const cpu_alias *alias = entry->aliases; alias->name != NULL;
	       alias++)
	    if (strncmp (alias->name, target, len) == 0
		&& alias->name[len] == '\0')
	      return entry;
	}
    }

  if (complain)
    {
      error_at (input_location, "unrecognized %s target: %s", optname, target);
      arm_print_hint_for_cpu_option (target, list);
    }
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

#ifdef HAVE_LOCAL_CPU_DETECT
  /* Add also "native" as possible value.  */
  candidates.safe_push ("native");
#endif

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
   a diagnostic is needed.  If COMPLAIN is true (the default) emit error
   messages and hints on invalid input.  */
const arch_option *
arm_parse_arch_option_name (const arch_option *list, const char *optname,
			    const char *target, bool complain)
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

  if (complain)
    {
      error_at (input_location, "unrecognized %s target: %s", optname, target);
      arm_print_hint_for_arch_option (target, list);
    }
  return NULL;
}

/* List the permitted architecture option names.  If TARGET is a near
   miss for an entry, print out the suggested alternative.  */
static void
arm_print_hint_for_fpu_option (const char *target)
{
  auto_vec<const char*> candidates;
  for (int i = 0; i < TARGET_FPU_auto; i++)
    candidates.safe_push (all_fpus[i].name);
  char *s;
  const char *hint = candidates_list_and_hint (target, s, candidates);
  if (hint)
    inform (input_location, "valid arguments are: %s; did you mean %qs?",
	    s, hint);
  else
    inform (input_location, "valid arguments are: %s", s);

  XDELETEVEC (s);
}

static const arm_fpu_desc *
arm_parse_fpu_option (const char *opt)
{
  int i;

  for (i = 0; i < TARGET_FPU_auto; i++)
    {
      if (strcmp (all_fpus[i].name, opt) == 0)
	return all_fpus + i;
    }

  error_at (input_location, "unrecognized %<-mfpu%> target: %s", opt);
  arm_print_hint_for_fpu_option (opt);
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

class candidate_extension
{
public:
  const cpu_arch_extension *extension;
  sbitmap isa_bits;
  bool required;

  candidate_extension (const cpu_arch_extension *ext, sbitmap bits)
    : extension (ext), isa_bits (bits), required (true)
    {}
  ~candidate_extension ()
    {
      sbitmap_free (isa_bits);
    }
};

/* Generate a canonical representation of the -march option from the
   current -march string (if given) and other options on the command
   line that might affect the architecture.  This aids multilib selection
   by ensuring that:
   a) the option is always present
   b) only the minimal set of options are used
   c) when there are multiple extensions, they are in a consistent order.

   The options array consists of couplets of information where the
   first item in each couplet is the string describing which option
   name was selected (arch, cpu, fpu) and the second is the value
   passed for that option.

   arch_for_multilib is boolean variable taking value true or false.
   arch_for_multilib is false when the canonical representation is for -march
   option and it is true when canonical representation is for -mlibarch option.
   On passing arch_for_multilib true the canonical string generated will be
   without the compiler options which are not required for multilib linking.  */
static const char *
arm_canon_arch_option_1 (int argc, const char **argv, bool arch_for_multilib)
{
  const char *arch = NULL;
  const char *cpu = NULL;
  const char *fpu = NULL;
  const char *abi = NULL;
  static char *canonical_arch = NULL;

  /* Just in case we're called more than once.  */
  if (canonical_arch)
    {
      free (canonical_arch);
      canonical_arch = NULL;
    }

  if (argc & 1)
    fatal_error (input_location,
		 "%%:%<canon_for_mlib%> takes 1 or more pairs of parameters");

  while (argc)
    {
      if (strcmp (argv[0], "arch") == 0)
	arch = argv[1];
      else if (strcmp (argv[0], "cpu") == 0)
	cpu = argv[1];
      else if (strcmp (argv[0], "fpu") == 0)
	fpu = argv[1];
      else if (strcmp (argv[0], "abi") == 0)
	abi = argv[1];
      else
	fatal_error (input_location,
		     "unrecognized operand to %%:%<canon_for_mlib%>");

      argc -= 2;
      argv += 2;
    }

  auto_sbitmap target_isa (isa_num_bits);
  auto_sbitmap base_isa (isa_num_bits);
  auto_sbitmap fpu_isa (isa_num_bits);
  auto_sbitmap ignore_multilib_isa (isa_num_bits);

  bitmap_clear (fpu_isa);
  bitmap_clear (ignore_multilib_isa);

  const arch_option *selected_arch = NULL;

  /* At least one of these must be defined by either the specs or the
     user.  */
  gcc_assert (cpu || arch);

  if (!fpu)
    fpu = FPUTYPE_AUTO;

  if (!abi)
    {
      if (TARGET_DEFAULT_FLOAT_ABI == ARM_FLOAT_ABI_SOFT)
	abi = "soft";
      else if (TARGET_DEFAULT_FLOAT_ABI == ARM_FLOAT_ABI_SOFTFP)
	abi = "softfp";
      else if (TARGET_DEFAULT_FLOAT_ABI == ARM_FLOAT_ABI_HARD)
	abi = "hard";
    }

  /* First build up a bitmap describing the target architecture.  */
  if (arch)
    {
      selected_arch = arm_parse_arch_option_name (all_architectures, "-march",
						  arch, !arch_for_multilib);

      if (selected_arch == NULL)
	return "";

      arm_initialize_isa (target_isa, selected_arch->common.isa_bits);
      arm_parse_option_features (target_isa, &selected_arch->common,
				 strchr (arch, '+'));
      if (fpu && strcmp (fpu, "auto") != 0)
	{
	  /* We assume that architectures do not have any FPU bits
	     enabled by default.  If they did, we would need to strip
	     these out first.  */
	  const arm_fpu_desc *target_fpu = arm_parse_fpu_option (fpu);
	  if (target_fpu == NULL)
	    return "";

	  arm_initialize_isa (fpu_isa, target_fpu->isa_bits);
	  bitmap_ior (target_isa, target_isa, fpu_isa);
	}
    }
  else if (cpu)
    {
      const cpu_option *selected_cpu
	= arm_parse_cpu_option_name (all_cores, "-mcpu", cpu,
				     !arch_for_multilib);

      if (selected_cpu == NULL)
	return "";

      arm_initialize_isa (target_isa, selected_cpu->common.isa_bits);
      arm_parse_option_features (target_isa, &selected_cpu->common,
				 strchr (cpu, '+'));
      if (fpu && strcmp (fpu, "auto") != 0)
	{
	  /* The easiest and safest way to remove the default fpu
	     capabilities is to look for a '+no..' option that removes
	     the base FPU bit (isa_bit_vfpv2).  If that doesn't exist
	     then the best we can do is strip out all the bits that
	     might be part of the most capable FPU we know about,
	     which is "crypto-neon-fp-armv8".  */
	  bool default_fpu_found = false;
	  if (selected_cpu->common.extensions)
	    {
	      const cpu_arch_extension *ext;
	      for (ext = selected_cpu->common.extensions; ext->name != NULL;
		   ++ext)
		{
		  if (ext->remove
		      && check_isa_bits_for (ext->isa_bits, isa_bit_vfpv2))
		    {
		      arm_initialize_isa (fpu_isa, ext->isa_bits);
		      bitmap_and_compl (target_isa, target_isa, fpu_isa);
		      default_fpu_found = true;
		    }
		}

	    }

	  if (!default_fpu_found)
	    {
	      arm_initialize_isa
		(fpu_isa,
		 all_fpus[TARGET_FPU_crypto_neon_fp_armv8].isa_bits);
	      bitmap_and_compl (target_isa, target_isa, fpu_isa);
	    }

	  const arm_fpu_desc *target_fpu = arm_parse_fpu_option (fpu);
	  if (target_fpu == NULL)
	    return "";

	  arm_initialize_isa (fpu_isa, target_fpu->isa_bits);
	  bitmap_ior (target_isa, target_isa, fpu_isa);
	}

      selected_arch = all_architectures + selected_cpu->arch;
    }

  /* If we have a soft-float ABI, disable the FPU.  */
  if (abi && strcmp (abi, "soft") == 0)
    {
      /* Clearing the VFPv2 bit is sufficient to stop any extention that
	 builds on the FPU from matching.  */
      bitmap_clear_bit (target_isa, isa_bit_vfpv2);
    }

  /* Here we remove feature isa bits from -mlibarch string which are not
     necessary for multilib string comparsion.  */
  if ((arch || cpu) && arch_for_multilib)
    {
      const enum isa_feature removable_bits[] = {ISA_IGNORE_FOR_MULTILIB,
						 isa_nobit};
      arm_initialize_isa (ignore_multilib_isa, removable_bits);
      bitmap_and_compl (target_isa, target_isa, ignore_multilib_isa);
    }

  /* If we don't have a selected architecture by now, something's
     badly wrong.  */
  gcc_assert (selected_arch);

  arm_initialize_isa (base_isa, selected_arch->common.isa_bits);

  /* Architecture has no extension options, so just return the canonical
     architecture name.  */
  if (selected_arch->common.extensions == NULL)
    return selected_arch->common.name;

  /* We're only interested in extension bits.  */
  bitmap_and_compl (target_isa, target_isa, base_isa);

  /* There are no extensions needed.  Just return the canonical architecture
     name.  */
  if (bitmap_empty_p (target_isa))
    return selected_arch->common.name;

  /* What is left is the architecture that the compiler will target.  We
     now need to map that back into a suitable option+features list.

     The list is built in two passes.  First we scan every additive
     option feature supported by the architecture.  If the option
     provides a subset of the features we need we add it to the list
     of candidates.  We then scan backwards over the list of
     candidates and if we find a feature that adds nothing to one that
     was later in the list we mark it as redundant.  The result is a
     minimal list of required features for the target
     architecture.  */

  std::list<candidate_extension *> extensions;

  auto_sbitmap target_isa_unsatisfied (isa_num_bits);
  bitmap_copy (target_isa_unsatisfied, target_isa);

  sbitmap isa_bits = NULL;
  for (const cpu_arch_extension *cand = selected_arch->common.extensions;
       cand->name != NULL;
       cand++)
    {
      if (cand->remove || cand->alias)
	continue;

      if (isa_bits == NULL)
	isa_bits = sbitmap_alloc (isa_num_bits);

      arm_initialize_isa (isa_bits, cand->isa_bits);
      if (bitmap_subset_p (isa_bits, target_isa))
	{
	  extensions.push_back (new candidate_extension (cand, isa_bits));
	  bitmap_and_compl (target_isa_unsatisfied, target_isa_unsatisfied,
			    isa_bits);
	  isa_bits = NULL;
	}
    }

  /* There's one extra case to consider, which is that the user has
     specified an FPU that is less capable than this architecture
     supports.  In that case the code above will fail to find a
     suitable feature.  We handle this by scanning the list of options
     again, matching the first option that provides an FPU that is
     more capable than the selected FPU.

     Note that the other case (user specified a more capable FPU than
     this architecture supports) should end up selecting the most
     capable FPU variant that we do support.  This is sufficient for
     multilib selection.  */

  if (bitmap_bit_p (target_isa_unsatisfied, isa_bit_vfpv2)
      && bitmap_bit_p (fpu_isa, isa_bit_vfpv2))
    {
      std::list<candidate_extension *>::iterator ipoint = extensions.begin ();

      for (const cpu_arch_extension *cand = selected_arch->common.extensions;
	   cand->name != NULL;
	   cand++)
	{
	  if (cand->remove || cand->alias)
	    continue;

	  if (isa_bits == NULL)
	    isa_bits = sbitmap_alloc (isa_num_bits);

	  /* We need to keep the features in canonical order, so move the
	     insertion point if this feature is a candidate.  */
	  if (ipoint != extensions.end ()
	      && (*ipoint)->extension == cand)
	    ++ipoint;

	  arm_initialize_isa (isa_bits, cand->isa_bits);
	  if (bitmap_subset_p (fpu_isa, isa_bits))
	    {
	      extensions.insert (ipoint,
				 new candidate_extension (cand, isa_bits));
	      isa_bits = NULL;
	      break;
	    }
	}
    }

  if (isa_bits)
    sbitmap_free (isa_bits);

  bitmap_clear (target_isa);
  size_t len = 1;
  for (std::list<candidate_extension *>::reverse_iterator riter
	 = extensions.rbegin ();
       riter != extensions.rend (); ++riter)
    {
      if (bitmap_subset_p ((*riter)->isa_bits, target_isa))
	(*riter)->required = false;
      else
	{
	  bitmap_ior (target_isa, target_isa, (*riter)->isa_bits);
	  len += strlen ((*riter)->extension->name) + 1;
	}
    }

  canonical_arch
    = (char *) xmalloc (len + strlen (selected_arch->common.name));

  strcpy (canonical_arch, selected_arch->common.name);

  for (std::list<candidate_extension *>::iterator iter = extensions.begin ();
       iter != extensions.end (); ++iter)
    {
      if ((*iter)->required)
	{
	  strcat (canonical_arch, "+");
	  strcat (canonical_arch, (*iter)->extension->name);
	}
      delete (*iter);
    }

  return canonical_arch;
}

/* If building big-endian on a BE8 target generate a --be8 option for
   the linker.  Takes four types of option: "little" - little-endian;
   "big" - big-endian; "be8" - force be8 iff big-endian; and "arch"
   "<arch-name>" (two arguments) - the target architecture.  The
   parameter names are generated by the driver from the command-line
   options.  */
const char *
arm_be8_option (int argc, const char **argv)
{
  int endian = TARGET_ENDIAN_DEFAULT;
  const char *arch = NULL;
  int arg;
  bool force = false;

  for (arg = 0; arg < argc; arg++)
    {
      if (strcmp (argv[arg], "little") == 0)
	endian = 0;
      else if (strcmp (argv[arg], "big") == 0)
	endian = 1;
      else if (strcmp (argv[arg], "be8") == 0)
	force = true;
      else if (strcmp (argv[arg], "arch") == 0)
	{
	  arg++;
	  gcc_assert (arg < argc);
	  arch = argv[arg];
	}
      else
	gcc_unreachable ();
    }

  /* Little endian - no be8 option.  */
  if (!endian)
    return "";

  if (force)
    return "--be8";

  /* Arch might not be set iff arm_canon_arch (above) detected an
     error.  Do nothing in that case.  */
  if (!arch)
    return "";

  const arch_option *selected_arch
    = arm_parse_arch_option_name (all_architectures, "-march", arch);

  /* Similarly if the given arch option was itself invalid.  */
  if (!selected_arch)
    return "";

  if (check_isa_bits_for (selected_arch->common.isa_bits, isa_bit_be8))
    return "--be8";

  return "";
}

/* Generate a -mfpu= option for passing to the assembler.  This is
   only called when -mfpu was set (possibly defaulted) to auto and is
   needed to ensure that the assembler knows the correct FPU to use.
   It wouldn't really be needed except that the compiler can be used
   to invoke the assembler directly on hand-written files that lack
   the necessary internal .fpu directives.  We assume that the architecture
   canonicalization calls have already been made so that we have a final
   -march= option to derive the fpu from.  */
const char*
arm_asm_auto_mfpu (int argc, const char **argv)
{
  static char *auto_fpu = NULL;
  const char *arch = NULL;
  static const enum isa_feature fpu_bitlist[]
    = { ISA_ALL_FPU_INTERNAL, isa_nobit };
  const arch_option *selected_arch;
  static const char* fpuname = "softvfp";

  /* Handle multiple calls to this routine.  */
  if (auto_fpu)
    {
      free (auto_fpu);
      auto_fpu = NULL;
    }

  while (argc)
    {
      if (strcmp (argv[0], "arch") == 0)
	arch = argv[1];
      else
	fatal_error (input_location,
		     "unrecognized operand to %%:%<asm_auto_mfpu%>");
      argc -= 2;
      argv += 2;
    }

  auto_sbitmap target_isa (isa_num_bits);
  auto_sbitmap fpubits (isa_num_bits);

  gcc_assert (arch != NULL);
  selected_arch = arm_parse_arch_option_name (all_architectures,
					      "-march", arch);
  if (selected_arch == NULL)
    return "";

  arm_initialize_isa (target_isa, selected_arch->common.isa_bits);
  arm_parse_option_features (target_isa, &selected_arch->common,
			     strchr (arch, '+'));
  arm_initialize_isa (fpubits, fpu_bitlist);

  bitmap_and (fpubits, fpubits, target_isa);

  /* The logic below is essentially identical to that in
     arm.cc:arm_identify_fpu_from_isa(), but that only works in the main
     part of the compiler.  */

  /* If there are no FPU capability bits, we just pass -mfpu=softvfp.  */
  if (!bitmap_empty_p (fpubits))
    {
      unsigned int i;
      auto_sbitmap cand_fpubits (isa_num_bits);
      for (i = 0; i < TARGET_FPU_auto; i++)
	{
	  arm_initialize_isa (cand_fpubits, all_fpus[i].isa_bits);
	  if (bitmap_equal_p (fpubits, cand_fpubits))
	    {
	      fpuname = all_fpus[i].name;
	      break;
	    }
	}

      gcc_assert (i != TARGET_FPU_auto
		  || bitmap_bit_p (target_isa, isa_bit_vfp_base));
    }

  auto_fpu = (char *) xmalloc (strlen (fpuname) + sizeof ("-mfpu="));
  strcpy (auto_fpu, "-mfpu=");
  strcat (auto_fpu, fpuname);
  return auto_fpu;
}

#undef ARM_CPU_NAME_LENGTH


#undef  TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (TARGET_DEFAULT | MASK_SCHED_PROLOG)

#undef  TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE arm_option_optimization_table

#undef TARGET_EXCEPT_UNWIND_INFO
#define TARGET_EXCEPT_UNWIND_INFO  arm_except_unwind_info

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;

/* Returns a canonical representation of the -march option from the current
   -march string (if given) and other options on the command line that might
   affect the architecture.  */
const char *
arm_canon_arch_option (int argc, const char **argv)
{
  return arm_canon_arch_option_1 (argc, argv, false);
}

/* Returns a canonical representation of the -mlibarch option from the current
   -march string (if given) and other options on the command line that might
   affect the architecture after removing the compiler extension options which
   are not required for multilib linking.  */
const char *
arm_canon_arch_multilib_option (int argc, const char **argv)
{
  return arm_canon_arch_option_1 (argc, argv, true);
}
