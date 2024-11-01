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
    /* Disable early scheduling due to high compile-time overheads.  */
    { OPT_LEVELS_ALL, OPT_fschedule_insns, NULL, 0 },
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
struct aarch64_option_extension
{
  /* The extension name to pass on to the assembler.  */
  const char *name;
  /* The smallest set of feature bits to toggle to enable this option.  */
  aarch64_feature_flags flag_canonical;
  /* If this feature is turned on, these bits also need to be turned on.  */
  aarch64_feature_flags flags_on;
  /* If this feature is turned off, these bits also need to be turned off.  */
  aarch64_feature_flags flags_off;
};

/* ISA extensions in AArch64.  */
static constexpr aarch64_option_extension all_extensions[] =
{
#define AARCH64_OPT_EXTENSION(NAME, IDENT, C, D, E, FEATURE_STRING) \
  {NAME, AARCH64_FL_##IDENT, feature_deps::IDENT ().explicit_on, \
   feature_deps::get_flags_off (feature_deps::root_off_##IDENT)},
#include "config/aarch64/aarch64-option-extensions.def"
  {NULL, 0, 0, 0}
};

struct processor_name_to_arch
{
  const char *processor_name;
  aarch64_arch arch;
  aarch64_feature_flags flags;
};

struct arch_to_arch_name
{
  aarch64_arch arch;
  const char *arch_name;
  aarch64_feature_flags flags;
};

/* Map processor names to the architecture revision they implement and
   the default set of architectural feature flags they support.  */
static constexpr processor_name_to_arch all_cores[] =
{
#define AARCH64_CORE(NAME, CORE_IDENT, C, ARCH_IDENT, E, F, G, H, I) \
  {NAME, AARCH64_ARCH_##ARCH_IDENT, feature_deps::cpu_##CORE_IDENT},
#include "config/aarch64/aarch64-cores.def"
  {"generic", AARCH64_ARCH_V8A, feature_deps::V8A ().enable},
  {"", aarch64_no_arch, 0}
};

/* Map architecture revisions to their string representation.  */
static constexpr arch_to_arch_name all_architectures[] =
{
#define AARCH64_ARCH(NAME, B, ARCH_IDENT, D, E)	\
  {AARCH64_ARCH_##ARCH_IDENT, NAME, feature_deps::ARCH_IDENT ().enable},
#include "config/aarch64/aarch64-arches.def"
  {aarch64_no_arch, "", 0}
};

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
  const struct aarch64_option_extension *opt = NULL;

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

/* Append all architecture extension candidates to the CANDIDATES vector.  */

void
aarch64_get_all_extension_candidates (auto_vec<const char *> *candidates)
{
  const struct aarch64_option_extension *opt;
  for (opt = all_extensions; opt->name != NULL; opt++)
    candidates->safe_push (opt->name);
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

/* Attempt to rewrite NAME, which has been passed on the command line
   as a -mcpu option to an equivalent -march value.  If we can do so,
   return the new string, otherwise return an error.  */

const char *
aarch64_rewrite_selected_cpu (const char *name)
{
  std::string original_string (name);
  std::string extension_str;
  std::string processor;
  size_t extension_pos = original_string.find_first_of ('+');

  /* Strip and save the extension string.  */
  if (extension_pos != std::string::npos)
    {
      processor = original_string.substr (0, extension_pos);
      extension_str = original_string.substr (extension_pos,
					      std::string::npos);
    }
  else
    {
      /* No extensions.  */
      processor = original_string;
    }

  const struct processor_name_to_arch* p_to_a;
  for (p_to_a = all_cores;
       p_to_a->arch != aarch64_no_arch;
       p_to_a++)
    {
      if (p_to_a->processor_name == processor)
	break;
    }

  const struct arch_to_arch_name* a_to_an;
  for (a_to_an = all_architectures;
       a_to_an->arch != aarch64_no_arch;
       a_to_an++)
    {
      if (a_to_an->arch == p_to_a->arch)
	break;
    }

  /* We couldn't find that proceesor name, or the processor name we
     found does not map to an architecture we understand.  */
  if (p_to_a->arch == aarch64_no_arch
      || a_to_an->arch == aarch64_no_arch)
    fatal_error (input_location, "unknown value %qs for %<-mcpu%>", name);

  aarch64_feature_flags extensions = p_to_a->flags;
  aarch64_parse_extension (extension_str.c_str (), &extensions, NULL);

  std::string outstr = a_to_an->arch_name
	+ aarch64_get_extension_string_for_isa_flags (extensions,
						      a_to_an->flags);

  /* We are going to memory leak here, nobody elsewhere
     in the callchain is going to clean up after us.  The alternative is
     to allocate a static buffer, and assert that it is big enough for our
     modified string, which seems much worse!  */
  return xstrdup (outstr.c_str ());
}

/* Called by the driver to rewrite a name passed to the -mcpu
   argument in preparation to be passed to the assembler.  The
   names passed from the commend line will be in ARGV, we want
   to use the right-most argument, which should be in
   ARGV[ARGC - 1].  ARGC should always be greater than 0.  */

const char *
aarch64_rewrite_mcpu (int argc, const char **argv)
{
  gcc_assert (argc);
  return aarch64_rewrite_selected_cpu (argv[argc - 1]);
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

