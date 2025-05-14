/* Common hooks for AArch64.
   Copyright (C) 2012-2025 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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
#define INCLUDE_STRING
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "memmodel.h"
#include "tm_p.h"
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"
#include "diagnostic.h"
#include "config/aarch64/aarch64-feature-deps.h"
#include "config/arm/aarch-common.h"

#ifdef  TARGET_BIG_ENDIAN_DEFAULT
#undef  TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (MASK_BIG_END)
#endif

#undef  TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION aarch64_handle_option

#undef	TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE aarch_option_optimization_table

/* Set default optimization options.  */
static const struct default_options aarch_option_optimization_table[] =
  {
    /* Enable section anchors by default at -O1 or higher.  */
    { OPT_LEVELS_1_PLUS, OPT_fsection_anchors, NULL, 1 },
    /* Disable fomit-frame-pointer by default.  */
    { OPT_LEVELS_ALL, OPT_fomit_frame_pointer, NULL, 0 },
    /* Enable -fsched-pressure by default when optimizing.  */
    { OPT_LEVELS_1_PLUS, OPT_fsched_pressure, NULL, 1 },
    /* Except for -O3 and higher, disable early scheduling due to high
       compile-time overheads.  */
    { OPT_LEVELS_ALL, OPT_fschedule_insns, NULL, 0 },
    { OPT_LEVELS_3_PLUS, OPT_fschedule_insns, NULL, 1 },
    /* Enable redundant extension instructions removal at -O2 and higher.  */
    { OPT_LEVELS_2_PLUS, OPT_free, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_mearly_ra_, NULL, AARCH64_EARLY_RA_ALL },
#if (TARGET_DEFAULT_ASYNC_UNWIND_TABLES == 1)
    { OPT_LEVELS_ALL, OPT_fasynchronous_unwind_tables, NULL, 1 },
    { OPT_LEVELS_ALL, OPT_funwind_tables, NULL, 1},
#endif
    { OPT_LEVELS_ALL, OPT__param_stack_clash_protection_guard_size_, NULL,
      DEFAULT_STK_CLASH_GUARD_SIZE == 0 ? 16 : DEFAULT_STK_CLASH_GUARD_SIZE },

    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };


/* Set OPTS->x_aarch64_asm_isa_flags_<0..n> to FLAGS and update
   OPTS->x_aarch64_isa_flags_<0..n> accordingly.  */
void
aarch64_set_asm_isa_flags (gcc_options *opts, aarch64_feature_flags flags)
{
  opts->x_aarch64_asm_isa_flags_0 = flags.val[0];
  opts->x_aarch64_asm_isa_flags_1 = flags.val[1];

  if (opts->x_target_flags & MASK_GENERAL_REGS_ONLY)
    flags &= ~feature_deps::get_flags_off (AARCH64_FL_FP);

  opts->x_aarch64_isa_flags_0 = flags.val[0];
  opts->x_aarch64_isa_flags_1 = flags.val[1];
}

/* Implement TARGET_HANDLE_OPTION.
   This function handles the target specific options for CPU/target selection.

   -mcpu=CPU is shorthand for -march=ARCH_FOR_CPU, -mtune=CPU.
   If either of -march or -mtune is given, they override their
   respective component of -mcpu.  This logic is implemented
   in config/aarch64/aarch64.cc:aarch64_override_options.  */

bool
aarch64_handle_option (struct gcc_options *opts,
		       struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		       const struct cl_decoded_option *decoded,
		       location_t loc ATTRIBUTE_UNUSED)
{
  size_t code = decoded->opt_index;
  const char *arg = decoded->arg;
  int val = decoded->value;

  switch (code)
    {
    case OPT_march_:
      opts->x_aarch64_arch_string = arg;
      return true;

    case OPT_mcpu_:
      opts->x_aarch64_cpu_string = arg;
      return true;

    case OPT_mtune_:
      opts->x_aarch64_tune_string = arg;
      return true;

    case OPT_mgeneral_regs_only:
      opts->x_target_flags |= MASK_GENERAL_REGS_ONLY;
      aarch64_set_asm_isa_flags (opts, aarch64_get_asm_isa_flags (opts));
      return true;

    case OPT_mfix_cortex_a53_835769:
      opts->x_aarch64_fix_a53_err835769 = val;
      return true;

    case OPT_mstrict_align:
      if (val)
	opts->x_target_flags |= MASK_STRICT_ALIGN;
      else
	opts->x_target_flags &= ~MASK_STRICT_ALIGN;
      return true;

    case OPT_momit_leaf_frame_pointer:
      opts->x_flag_omit_leaf_frame_pointer = val;
      return true;

    case OPT_moutline_atomics:
      opts->x_aarch64_flag_outline_atomics = val;
      return true;

    default:
      return true;
    }
}


/* An ISA extension in the co-processor and main instruction set space.  */
struct aarch64_extension_info
{
  /* The extension name to pass on to the assembler.  */
  const char *name;
  /* The smallest set of feature bits to toggle to enable this option.  */
  aarch64_feature_flags flag_canonical;
  /* If this feature is turned on, these bits also need to be turned on.  */
  aarch64_feature_flags flags_on;
  /* If this feature is turned off, these bits also need to be turned off.  */
  aarch64_feature_flags flags_off;
  /* If this feature remains enabled, these bits must also remain enabled.  */
  aarch64_feature_flags flags_required;
};

/* ISA extensions in AArch64.  */
static constexpr aarch64_extension_info all_extensions[] =
{
#define AARCH64_OPT_EXTENSION(NAME, IDENT, C, D, E, FEATURE_STRING) \
  {NAME, AARCH64_FL_##IDENT, feature_deps::IDENT ().explicit_on, \
   feature_deps::get_flags_off (feature_deps::root_off_##IDENT), \
   feature_deps::IDENT ().enable},
#include "config/aarch64/aarch64-option-extensions.def"
  {NULL, 0, 0, 0, 0}
};

struct aarch64_arch_info
{
  const char *name;
  aarch64_arch arch;
  aarch64_feature_flags flags;
};

/* Map architecture revisions to their string representation.  */
static constexpr aarch64_arch_info all_architectures[] =
{
#define AARCH64_ARCH(NAME, B, ARCH_IDENT, D, E)	\
  {NAME, AARCH64_ARCH_##ARCH_IDENT, feature_deps::ARCH_IDENT ().enable},
#include "config/aarch64/aarch64-arches.def"
  {NULL, aarch64_no_arch, 0}
};

struct aarch64_processor_info
{
  const char *name;
  aarch64_cpu processor;
  aarch64_arch arch;
  aarch64_feature_flags flags;
};

/* Map processor names to the architecture revision they implement and
   the default set of architectural feature flags they support.  */
static constexpr aarch64_processor_info all_cores[] =
{
#define AARCH64_CORE(NAME, CORE_IDENT, C, ARCH_IDENT, E, F, G, H, I) \
  {NAME, AARCH64_CPU_##CORE_IDENT, AARCH64_ARCH_##ARCH_IDENT, \
   feature_deps::cpu_##CORE_IDENT},
#include "config/aarch64/aarch64-cores.def"
  {NULL, aarch64_no_cpu, aarch64_no_arch, 0}
};

/* Return the set of feature flags that are required to be enabled when the
   features in FLAGS are enabled.  */

aarch64_feature_flags
aarch64_get_required_features (aarch64_feature_flags flags)
{
  const struct aarch64_extension_info *opt;
  for (opt = all_extensions; opt->name != NULL; opt++)
    if (flags & opt->flag_canonical)
      flags |= opt->flags_required;
  return flags;
}

/* Print a list of CANDIDATES for an argument, and try to suggest a specific
   close match.  */

inline static void
aarch64_print_hint_candidates (const char *str,
			       const auto_vec<const char*> & candidates)
{
  char *s;
  const char *hint = candidates_list_and_hint (str, s, candidates);
  if (hint)
    inform (input_location, "valid arguments are: %s;"
			     " did you mean %qs?", s, hint);
  else
    inform (input_location, "valid arguments are: %s", s);

  XDELETEVEC (s);
}

/* Print a hint with a suggestion for an extension name
   that most closely resembles what the user passed in STR.  */

void
aarch64_print_hint_for_extensions (const char *str)
{
  auto_vec<const char *> candidates;
  const struct aarch64_extension_info *opt;
  for (opt = all_extensions; opt->name != NULL; opt++)
    candidates.safe_push (opt->name);

  aarch64_print_hint_candidates (str, candidates);
}

/* Print a hint with a suggestion for an architecture name that most closely
   resembles what the user passed in STR.  */

void
aarch64_print_hint_for_arch (const char *str)
{
  auto_vec<const char *> candidates;
  const struct aarch64_arch_info *entry = all_architectures;
  for (; entry->name != NULL; entry++)
    candidates.safe_push (entry->name);

#ifdef HAVE_LOCAL_CPU_DETECT
  /* Add also "native" as possible value.  */
  candidates.safe_push ("native");
#endif

  aarch64_print_hint_candidates (str, candidates);
}

/* Print a hint with a suggestion for a core name that most closely resembles
   what the user passed in STR.  */

void
aarch64_print_hint_for_core (const char *str)
{
  auto_vec<const char *> candidates;
  const struct aarch64_processor_info *entry = all_cores;
  for (; entry->name != NULL; entry++)
    candidates.safe_push (entry->name);
  aarch64_print_hint_candidates (str, candidates);
}


/* Parse the architecture extension string STR and update ISA_FLAGS
   with the architecture features turned on or off.  Return a
   aarch_parse_opt_result describing the result.
   When the STR string contains an invalid extension,
   a copy of the string is created and stored to INVALID_EXTENSION.  */

enum aarch_parse_opt_result
aarch64_parse_extension (const char *str, aarch64_feature_flags *isa_flags,
                         std::string *invalid_extension)
{
  /* The extension string is parsed left to right.  */
  const struct aarch64_extension_info *opt = NULL;

  /* Flag to say whether we are adding or removing an extension.  */
  int adding_ext = -1;

  while (str != NULL && *str != 0)
    {
      const char *ext;
      size_t len;

      str++;
      ext = strchr (str, '+');

      if (ext != NULL)
	len = ext - str;
      else
	len = strlen (str);

      if (len >= 2 && startswith (str, "no"))
	{
	  adding_ext = 0;
	  len -= 2;
	  str += 2;
	}
      else if (len > 0)
	adding_ext = 1;

      if (len == 0)
	return AARCH_PARSE_MISSING_ARG;


      /* Scan over the extensions table trying to find an exact match.  */
      for (opt = all_extensions; opt->name != NULL; opt++)
	{
	  if (strlen (opt->name) == len && strncmp (opt->name, str, len) == 0)
	    {
	      /* Add or remove the extension.  */
	      if (adding_ext)
		*isa_flags |= opt->flags_on;
	      else
		*isa_flags &= ~opt->flags_off;
	      break;
	    }
	}

      if (opt->name == NULL)
	{
	  /* Extension not found in list.  */
	  if (invalid_extension)
	    *invalid_extension = std::string (str, len);
	  return AARCH_PARSE_INVALID_FEATURE;
	}

      str = ext;
    };

  return AARCH_PARSE_OK;
}

/* Parse the TO_PARSE string and put the architecture that it
   selects into RES_ARCH and the architectural features into RES_FLAGS.
   Return an aarch_parse_opt_result describing the parse result.
   If there is an error parsing, RES_ARCH and RES_FLAGS are left unchanged.
   When the TO_PARSE string contains an invalid extension,
   a copy of the string is created and stored to INVALID_EXTENSION.  */

enum aarch_parse_opt_result
aarch64_parse_arch (const char *to_parse, aarch64_arch *res_arch,
		    aarch64_feature_flags *res_flags,
		    std::string *invalid_extension)
{
  const char *ext;
  const struct aarch64_arch_info *arch;
  size_t len;

  ext = strchr (to_parse, '+');

  if (ext != NULL)
    len = ext - to_parse;
  else
    len = strlen (to_parse);

  if (len == 0)
    return AARCH_PARSE_MISSING_ARG;


  /* Loop through the list of supported ARCHes to find a match.  */
  for (arch = all_architectures; arch->name != NULL; arch++)
    {
      if (strlen (arch->name) == len
	  && strncmp (arch->name, to_parse, len) == 0)
	{
	  auto isa_flags = arch->flags;

	  if (ext != NULL)
	    {
	      /* TO_PARSE string contains at least one extension.  */
	      enum aarch_parse_opt_result ext_res
		= aarch64_parse_extension (ext, &isa_flags, invalid_extension);

	      if (ext_res != AARCH_PARSE_OK)
		return ext_res;
	    }
	  /* Extension parsing was successful.  Confirm the result
	     arch and ISA flags.  */
	  *res_arch = arch->arch;
	  *res_flags = isa_flags;
	  return AARCH_PARSE_OK;
	}
    }

  /* ARCH name not found in list.  */
  return AARCH_PARSE_INVALID_ARG;
}

/* Parse the TO_PARSE string and put the result tuning in RES_CPU and the
   architecture flags in RES_FLAGS.  Return an aarch_parse_opt_result
   describing the parse result.  If there is an error parsing, RES_CPU and
   RES_FLAGS are left unchanged.
   When the TO_PARSE string contains an invalid extension,
   a copy of the string is created and stored to INVALID_EXTENSION.  */

enum aarch_parse_opt_result
aarch64_parse_cpu (const char *to_parse, aarch64_cpu *res_cpu,
		   aarch64_feature_flags *res_flags,
		   std::string *invalid_extension)
{
  const char *ext;
  const struct aarch64_processor_info *cpu;
  size_t len;

  ext = strchr (to_parse, '+');

  if (ext != NULL)
    len = ext - to_parse;
  else
    len = strlen (to_parse);

  if (len == 0)
    return AARCH_PARSE_MISSING_ARG;


  /* Loop through the list of supported CPUs to find a match.  */
  for (cpu = all_cores; cpu->name != NULL; cpu++)
    {
      if (strlen (cpu->name) == len && strncmp (cpu->name, to_parse, len) == 0)
	{
	  auto isa_flags = cpu->flags;

	  if (ext != NULL)
	    {
	      /* TO_PARSE string contains at least one extension.  */
	      enum aarch_parse_opt_result ext_res
		= aarch64_parse_extension (ext, &isa_flags, invalid_extension);

	      if (ext_res != AARCH_PARSE_OK)
		return ext_res;
	    }
	  /* Extension parsing was successfull.  Confirm the result
	     cpu and ISA flags.  */
	  *res_cpu = cpu->processor;
	  *res_flags = isa_flags;
	  return AARCH_PARSE_OK;
	}
    }

  /* CPU name not found in list.  */
  return AARCH_PARSE_INVALID_ARG;
}

/* Parse the TO_PARSE string and put the cpu it selects into RES_CPU.
   Return an aarch_parse_opt_result describing the parse result.
   If the parsing fails then RES_CPU does not change.  */

enum aarch_parse_opt_result
aarch64_parse_tune (const char *to_parse, aarch64_cpu *res_cpu)
{
  const struct aarch64_processor_info *cpu;

  /* Loop through the list of supported CPUs to find a match.  */
  for (cpu = all_cores; cpu->name != NULL; cpu++)
    {
      if (strcmp (cpu->name, to_parse) == 0)
	{
	  *res_cpu = cpu->processor;
	  return AARCH_PARSE_OK;
	}
    }

  /* CPU name not found in list.  */
  return AARCH_PARSE_INVALID_ARG;
}


/* Validate a command-line -march option.  Parse the arch and extensions
   (if any) specified in STR and throw errors if appropriate.  Put the
   results, if they are valid, in RES_ARCH and RES_FLAGS.  Return whether the
   option is valid.  */

bool
aarch64_validate_march (const char *str, aarch64_arch *res_arch,
			aarch64_feature_flags *res_flags)
{
  std::string invalid_extension;
  enum aarch_parse_opt_result parse_res
    = aarch64_parse_arch (str, res_arch, res_flags, &invalid_extension);

  if (parse_res == AARCH_PARSE_OK)
    return true;

  switch (parse_res)
    {
      case AARCH_PARSE_MISSING_ARG:
	error ("missing arch name in %<-march=%s%>", str);
	break;
      case AARCH_PARSE_INVALID_ARG:
	{
	  error ("unknown value %qs for %<-march%>", str);
	  aarch64_print_hint_for_arch (str);
	  /* A common user error is confusing -march and -mcpu.
	     If the -march string matches a known CPU suggest -mcpu.  */
	  aarch64_cpu temp_cpu;
	  aarch64_feature_flags temp_flags;
	  parse_res = aarch64_parse_cpu (str, &temp_cpu, &temp_flags,
					 &invalid_extension);
	  if (parse_res == AARCH_PARSE_OK)
	    inform (input_location, "did you mean %<-mcpu=%s%>?", str);
	  break;
	}
      case AARCH_PARSE_INVALID_FEATURE:
	error ("invalid feature modifier %qs in %<-march=%s%>",
	       invalid_extension.c_str (), str);
	aarch64_print_hint_for_extensions (invalid_extension.c_str ());
	break;
      default:
	gcc_unreachable ();
    }

  return false;
}

/* Validate a command-line -mcpu option.  Parse the cpu and extensions (if any)
   specified in STR and throw errors if appropriate.  Put the results if
   they are valid in RES_CPU and RES_FLAGS.  Return whether the option is
   valid.  */

bool
aarch64_validate_mcpu (const char *str, aarch64_cpu *res_cpu,
		       aarch64_feature_flags *res_flags)
{
  std::string invalid_extension;
  enum aarch_parse_opt_result parse_res
    = aarch64_parse_cpu (str, res_cpu, res_flags, &invalid_extension);

  if (parse_res == AARCH_PARSE_OK)
    return true;

  switch (parse_res)
    {
      case AARCH_PARSE_MISSING_ARG:
	error ("missing cpu name in %<-mcpu=%s%>", str);
	break;
      case AARCH_PARSE_INVALID_ARG:
	{
	  error ("unknown value %qs for %<-mcpu%>", str);
	  aarch64_print_hint_for_core (str);
	  /* A common user error is confusing -march and -mcpu.
	     If the -mcpu string matches a known architecture then suggest
	     -march=.  */
	  aarch64_arch temp_arch;
	  aarch64_feature_flags temp_flags;
	  parse_res = aarch64_parse_arch (str, &temp_arch, &temp_flags,
					  &invalid_extension);
	  if (parse_res == AARCH_PARSE_OK)
	    inform (input_location, "did you mean %<-march=%s%>?", str);
	  break;
	}
      case AARCH_PARSE_INVALID_FEATURE:
	error ("invalid feature modifier %qs in %<-mcpu=%s%>",
	       invalid_extension.c_str (), str);
	aarch64_print_hint_for_extensions (invalid_extension.c_str ());
	break;
      default:
	gcc_unreachable ();
    }

  return false;
}

/* Validate a command-line -mtune option.  Parse the cpu
   specified in STR and throw errors if appropriate.  Put the
   result, if it is valid, in RES_CPU.  Return whether the option is
   valid.  */

bool
aarch64_validate_mtune (const char *str, aarch64_cpu *res_cpu)
{
  enum aarch_parse_opt_result parse_res
    = aarch64_parse_tune (str, res_cpu);

  if (parse_res == AARCH_PARSE_OK)
    return true;

  switch (parse_res)
    {
      case AARCH_PARSE_MISSING_ARG:
	error ("missing cpu name in %<-mtune=%s%>", str);
	break;
      case AARCH_PARSE_INVALID_ARG:
	error ("unknown value %qs for %<-mtune%>", str);
	aarch64_print_hint_for_core (str);
	break;
      default:
	gcc_unreachable ();
    }
  return false;
}


/* Return a string representation of ISA_FLAGS.  DEFAULT_ARCH_FLAGS
   gives the default set of flags which are implied by whatever -march
   we'd put out.  Our job is to figure out the minimal set of "+" and
   "+no" feature flags to put out, and to put them out grouped such
   that all the "+" flags come before the "+no" flags.  */

std::string
aarch64_get_extension_string_for_isa_flags
  (aarch64_feature_flags isa_flags,
   aarch64_feature_flags default_arch_flags)
{
  std::string outstr = "";

  /* The CRYPTO bit should only be used to support the +crypto alias
     during option processing, and should be cleared at all other times.
     Verify this property for the supplied flags bitmask.  */
  gcc_assert (!(AARCH64_FL_CRYPTO & aarch64_isa_flags));
  aarch64_feature_flags current_flags = default_arch_flags;

  /* As a special case, do not assume that the assembler will enable CRC
     even if it is the default for the architecture.  This is required
     because some CPUs had an incorrect specification in older assemblers:
     even though CRC should be the default for these cases the -mcpu
     values would not turn it on.

     However, assemblers with Armv8-R AArch64 support should not have this
     issue, so we don't need this fix when targeting Armv8-R.  */
  auto explicit_flags = (!(current_flags & AARCH64_FL_V8R)
			 ? AARCH64_FL_CRC : 0);

  /* Add the features in isa_flags & ~current_flags using the smallest
     possible number of extensions.  We can do this by iterating over the
     array in reverse order, since the array is sorted topologically.
     But in order to make the output more readable, it seems better
     to add the strings in definition order.  */
  aarch64_feature_flags added = 0;
  auto flags_crypto = AARCH64_FL_AES | AARCH64_FL_SHA2;
  for (unsigned int i = ARRAY_SIZE (all_extensions); i-- > 0; )
    {
      auto &opt = all_extensions[i];

      /* As a special case, emit +crypto rather than +aes+sha2,
	 in order to support assemblers that predate the separate
	 per-feature crypto flags.  */
      auto flags = opt.flag_canonical;
      if (flags == AARCH64_FL_CRYPTO)
	flags = flags_crypto;

      if ((flags & isa_flags & (explicit_flags | ~current_flags)) == flags)
	{
	  current_flags |= opt.flags_on;
	  added |= opt.flag_canonical;
	}
    }
  for (auto &opt : all_extensions)
    if (added & opt.flag_canonical)
      {
	outstr += "+";
	outstr += opt.name;
      }

  /* Remove the features in current_flags & ~isa_flags.  If the feature does
     not have an HWCAPs then it shouldn't be taken into account for feature
     detection because one way or another we can't tell if it's available
     or not.  */

  for (auto &opt : all_extensions)
    {
      auto flags = opt.flag_canonical;
      /* As a special case, don't emit "+noaes" or "+nosha2" when we could emit
	 "+nocrypto" instead, in order to support assemblers that predate the
	 separate per-feature crypto flags.  Only allow "+nocrypto" when "sm4"
	 is not already enabled (to avoid dependending on whether "+nocrypto"
	 also disables "sm4").  */
      if (flags & flags_crypto
	  && (flags_crypto & current_flags & ~isa_flags) == flags_crypto
	  && !(current_flags & AARCH64_FL_SM4))
	  continue;

      if (flags == AARCH64_FL_CRYPTO)
	/* If either crypto flag needs removing here, then both do.  */
	flags = flags_crypto;

      if (flags & current_flags & ~isa_flags)
	{
	  current_flags &= ~opt.flags_off;
	  outstr += "+no";
	  outstr += opt.name;
	}
    }

  return outstr;
}

/* Generate an arch string to be passed to the assembler.  */

std::string
aarch64_get_arch_string_for_assembler (aarch64_arch arch,
				       aarch64_feature_flags flags)
{
  const struct aarch64_arch_info *entry;
  for (entry = all_architectures; entry->arch != aarch64_no_arch; entry++)
    if (entry->arch == arch)
	break;

  std::string outstr = entry->name
	+ aarch64_get_extension_string_for_isa_flags (flags, entry->flags);

  return outstr;
}

/* Called by the driver to rewrite a name passed to the -march
   argument in preparation to be passed to the assembler.  The
   names passed from the commend line will be in ARGV, we want
   to use the right-most argument, which should be in
   ARGV[ARGC - 1].  ARGC should always be greater than 0.  */

const char *
aarch64_rewrite_march (int argc, const char **argv)
{
  gcc_assert (argc);
  const char *name = argv[argc - 1];
  aarch64_arch arch;
  aarch64_feature_flags flags;

  aarch64_validate_march (name, &arch, &flags);

  std::string outstr = aarch64_get_arch_string_for_assembler (arch, flags);

  /* We are going to memory leak here, nobody elsewhere
     in the callchain is going to clean up after us.  The alternative is
     to allocate a static buffer, and assert that it is big enough for our
     modified string, which seems much worse!  */
  return xstrdup (outstr.c_str ());
}

/* Called by the driver to rewrite a name passed to the -mcpu argument
   to an equivalent -march value to be passed to the assembler.  The
   names passed from the commend line will be in ARGV, we want
   to use the right-most argument, which should be in
   ARGV[ARGC - 1].  ARGC should always be greater than 0.  */

const char *
aarch64_rewrite_mcpu (int argc, const char **argv)
{
  gcc_assert (argc);
  const char *name = argv[argc - 1];
  aarch64_cpu cpu;
  aarch64_feature_flags flags;

  aarch64_validate_mcpu (name, &cpu, &flags);

  const struct aarch64_processor_info *entry;
  for (entry = all_cores; entry->processor != aarch64_no_cpu; entry++)
    if (entry->processor == cpu)
      break;

  std::string outstr = aarch64_get_arch_string_for_assembler (entry->arch,
							      flags);

  /* We are going to memory leak here, nobody elsewhere
     in the callchain is going to clean up after us.  The alternative is
     to allocate a static buffer, and assert that it is big enough for our
     modified string, which seems much worse!  */
  return xstrdup (outstr.c_str ());
}

/* Checks to see if the host CPU may not be Cortex-A53 or an unknown Armv8-a
   baseline CPU.  */

const char *
is_host_cpu_not_armv8_base (int argc, const char **argv)
{
  gcc_assert (argc);

  /* Default to not knowing what we are if unspecified.  The SPEC file should
     have already mapped configure time options to here through
     OPTION_DEFAULT_SPECS so we don't need to check the configure variants
     manually.  */
  if (!argv[0])
    return NULL;

  const char *res = argv[0];

  /* No SVE system is baseline Armv8-A.  */
  if (strstr (res, "+sve"))
    return "";

  if (strstr (res, "cortex-a53") || strstr (res, "armv8-a"))
    return NULL;

  return "";
}

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;

#undef AARCH64_CPU_NAME_LENGTH

