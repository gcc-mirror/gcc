/* Subroutines for loongarch-specific option handling.
   Copyright (C) 2021-2023 Free Software Foundation, Inc.
   Contributed by Loongson Ltd.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "obstack.h"
#include "diagnostic-core.h"
#include "loongarch-cpu.h"
#include "loongarch-opts.h"
#include "loongarch-str.h"

struct loongarch_target la_target;

/* ABI-related configuration.  */
#define ABI_COUNT (sizeof(abi_priority_list)/sizeof(struct loongarch_abi))
static const struct loongarch_abi
abi_priority_list[] = {
    {ABI_BASE_LP64D, ABI_EXT_BASE},
    {ABI_BASE_LP64F, ABI_EXT_BASE},
    {ABI_BASE_LP64S, ABI_EXT_BASE},
};

/* Initialize enabled_abi_types from TM_MULTILIB_LIST.  */
#ifdef LA_DISABLE_MULTILIB
#define MULTILIB_LIST_LEN 1
#else
#define MULTILIB_LIST_LEN (sizeof (tm_multilib_list) / sizeof (int) / 2)
static const int tm_multilib_list[] = { TM_MULTILIB_LIST };
#endif
static int enabled_abi_types[N_ABI_BASE_TYPES][N_ABI_EXT_TYPES] = { 0 };

#define isa_required(ABI) (abi_minimal_isa[(ABI).base][(ABI).ext])
extern "C" const struct loongarch_isa
abi_minimal_isa[N_ABI_BASE_TYPES][N_ABI_EXT_TYPES];

static inline int
is_multilib_enabled (struct loongarch_abi abi)
{
  return enabled_abi_types[abi.base][abi.ext];
}

static void
init_enabled_abi_types ()
{
#ifdef LA_DISABLE_MULTILIB
  enabled_abi_types[DEFAULT_ABI_BASE][DEFAULT_ABI_EXT] = 1;
#else
  int abi_base, abi_ext;
  for (unsigned int i = 0; i < MULTILIB_LIST_LEN; i++)
    {
      abi_base = tm_multilib_list[i << 1];
      abi_ext = tm_multilib_list[(i << 1) + 1];
      enabled_abi_types[abi_base][abi_ext] = 1;
    }
#endif
}

/* Switch masks.  */
#undef M
#define M(NAME) OPTION_MASK_##NAME
const int loongarch_switch_mask[N_SWITCH_TYPES] = {
  /* SW_SOFT_FLOAT */    M(FORCE_SOFTF),
  /* SW_SINGLE_FLOAT */  M(FORCE_F32),
  /* SW_DOUBLE_FLOAT */  M(FORCE_F64),
};
#undef M

/* String processing.  */
static struct obstack msg_obstack;
#define APPEND_STRING(STR) obstack_grow (&msg_obstack, STR, strlen(STR));
#define APPEND1(CH) obstack_1grow(&msg_obstack, CH);

static const char* abi_str (struct loongarch_abi abi);
static const char* isa_str (const struct loongarch_isa *isa, char separator);
static const char* arch_str (const struct loongarch_target *target);
static const char* multilib_enabled_abi_list ();

/* Misc */
static struct loongarch_abi isa_default_abi (const struct loongarch_isa *isa);
static int isa_base_compat_p (const struct loongarch_isa *set1,
			      const struct loongarch_isa *set2);
static int isa_fpu_compat_p (const struct loongarch_isa *set1,
			     const struct loongarch_isa *set2);
static int abi_compat_p (const struct loongarch_isa *isa,
			 struct loongarch_abi abi);
static int abi_default_cpu_arch (struct loongarch_abi abi);

/* Checking configure-time defaults.  */
#ifndef DEFAULT_ABI_BASE
#error missing definition of DEFAULT_ABI_BASE in ${tm_defines}.
#endif

#ifndef DEFAULT_ABI_EXT
#error missing definition of DEFAULT_ABI_EXT in ${tm_defines}.
#endif

#ifndef DEFAULT_CPU_ARCH
#error missing definition of DEFAULT_CPU_ARCH in ${tm_defines}.
#endif

#ifndef DEFAULT_ISA_EXT_FPU
#error missing definition of DEFAULT_ISA_EXT_FPU in ${tm_defines}.
#endif

/* Handle combinations of -m machine option values
   (see loongarch.opt and loongarch-opts.h).  */
void
loongarch_config_target (struct loongarch_target *target,
			 HOST_WIDE_INT opt_switches,
			 int opt_arch, int opt_tune, int opt_fpu,
			 int opt_abi_base, int opt_abi_ext,
			 int opt_cmodel, int follow_multilib_list)
{
  struct loongarch_target t;

  if (!target)
    return;

  /* Initialization */
  init_enabled_abi_types ();
  obstack_init (&msg_obstack);

  struct {
    int arch, tune, fpu, abi_base, abi_ext, cmodel;
  } constrained = {
      M_OPT_ABSENT(opt_arch)     ? 0 : 1,
      M_OPT_ABSENT(opt_tune)     ? 0 : 1,
      M_OPT_ABSENT(opt_fpu)      ? 0 : 1,
      M_OPT_ABSENT(opt_abi_base) ? 0 : 1,
      M_OPT_ABSENT(opt_abi_ext)  ? 0 : 1,
      M_OPT_ABSENT(opt_cmodel)   ? 0 : 1,
  };

#define on(NAME) ((loongarch_switch_mask[(SW_##NAME)] & opt_switches) \
		  && (on_switch = (SW_##NAME), 1))
  int on_switch;

  /* 1.  Target ABI */
  t.abi.base = constrained.abi_base ? opt_abi_base : DEFAULT_ABI_BASE;

  t.abi.ext = constrained.abi_ext ? opt_abi_ext : DEFAULT_ABI_EXT;

  /* Extra switch handling.  */
  if (on (SOFT_FLOAT) || on (SINGLE_FLOAT) || on (DOUBLE_FLOAT))
    {
      switch (on_switch)
	{
	  case SW_SOFT_FLOAT:
	    opt_fpu = ISA_EXT_NOFPU;
	    break;

	  case SW_SINGLE_FLOAT:
	    opt_fpu = ISA_EXT_FPU32;
	    break;

	  case SW_DOUBLE_FLOAT:
	    opt_fpu = ISA_EXT_FPU64;
	    break;

	  default:
	    gcc_unreachable();
	}
      constrained.fpu = 1;

      /* The target ISA is not ready yet, but (isa_required (t.abi)
	 + forced fpu) is enough for computing the forced base ABI.  */
      struct loongarch_isa default_isa = isa_required (t.abi);
      struct loongarch_isa force_isa = default_isa;
      struct loongarch_abi force_abi = t.abi;
      force_isa.fpu = opt_fpu;
      force_abi.base = isa_default_abi (&force_isa).base;

      if (constrained.abi_base && (t.abi.base != force_abi.base))
	inform (UNKNOWN_LOCATION,
		"%<-m%s%> overrides %<-m%s=%s%>, adjusting ABI to %qs",
		loongarch_switch_strings[on_switch],
		OPTSTR_ABI_BASE, loongarch_abi_base_strings[t.abi.base],
		abi_str (force_abi));

      t.abi.base = force_abi.base;
    }

#ifdef LA_DISABLE_MULTILIB
  if (follow_multilib_list)
    if (t.abi.base != DEFAULT_ABI_BASE || t.abi.ext != DEFAULT_ABI_EXT)
      {
	static const struct loongarch_abi default_abi
	  = {DEFAULT_ABI_BASE, DEFAULT_ABI_EXT};

	warning (0, "ABI changed (%qs to %qs) while multilib is disabled",
		 abi_str (default_abi), abi_str (t.abi));
      }
#endif

  /* 2.  Target CPU */
  t.cpu_arch = constrained.arch ? opt_arch : DEFAULT_CPU_ARCH;

  t.cpu_tune = constrained.tune ? opt_tune
    : (constrained.arch ? DEFAULT_CPU_ARCH : DEFAULT_CPU_TUNE);

#ifdef __loongarch__
  /* For native compilers, gather local CPU information
     and fill the "CPU_NATIVE" index of arrays defined in
     loongarch-cpu.c.  */

  t.cpu_native = fill_native_cpu_config (t.cpu_arch == CPU_NATIVE,
					 t.cpu_tune == CPU_NATIVE);

#else
  if (t.cpu_arch == CPU_NATIVE)
    fatal_error (UNKNOWN_LOCATION,
		 "%qs does not work on a cross compiler",
		 "-m" OPTSTR_ARCH "=" STR_CPU_NATIVE);

  else if (t.cpu_tune == CPU_NATIVE)
    fatal_error (UNKNOWN_LOCATION,
		 "%qs does not work on a cross compiler",
		 "-m" OPTSTR_TUNE "=" STR_CPU_NATIVE);
#endif

  /* 3.  Target ISA */
config_target_isa:

  /* Get default ISA from "-march" or its default value.  */
  t.isa = loongarch_cpu_default_isa[LARCH_ACTUAL_ARCH];

  /* Apply incremental changes.  */
  /* "-march=native" overrides the default FPU type.  */
  t.isa.fpu = constrained.fpu ? opt_fpu :
    ((t.cpu_arch == CPU_NATIVE && constrained.arch) ?
     t.isa.fpu : DEFAULT_ISA_EXT_FPU);


  /* 4.  ABI-ISA compatibility */
  /* Note:
     - There IS a unique default -march value for each ABI type
       (config.gcc: triplet -> abi -> default arch).

     - If the base ABI is incompatible with the default arch,
       try using the default -march it implies (and mark it
       as "constrained" this time), then re-apply step 3.  */

  struct loongarch_abi abi_tmp;
  const struct loongarch_isa* isa_min;

  abi_tmp = t.abi;
  isa_min = &isa_required (abi_tmp);

  if (isa_base_compat_p (&t.isa, isa_min)); /* OK.  */
  else if (!constrained.arch)
    {
      /* Base architecture can only be implied by -march,
	 so we adjust that first if it is not constrained.  */
      int fallback_arch = abi_default_cpu_arch (t.abi);

      if (t.cpu_arch == CPU_NATIVE)
	warning (0, "your native CPU architecture (%qs) "
		 "does not support %qs ABI, falling back to %<-m%s=%s%>",
		 arch_str (&t), abi_str (t.abi), OPTSTR_ARCH,
		 loongarch_cpu_strings[fallback_arch]);
      else
	warning (0, "default CPU architecture (%qs) "
		 "does not support %qs ABI, falling back to %<-m%s=%s%>",
		 arch_str (&t), abi_str (t.abi), OPTSTR_ARCH,
		 loongarch_cpu_strings[fallback_arch]);

      t.cpu_arch = fallback_arch;
      constrained.arch = 1;
      goto config_target_isa;
    }
  else if (!constrained.abi_base)
    {
      /* If -march is given while -mabi is not,
	 try selecting another base ABI type.  */
      abi_tmp.base = isa_default_abi (&t.isa).base;
    }
  else
    goto fatal;

  if (isa_fpu_compat_p (&t.isa, isa_min)); /* OK.  */
  else if (!constrained.fpu)
    t.isa.fpu = isa_min->fpu;
  else if (!constrained.abi_base)
    /* If -march is compatible with the default ABI
       while -mfpu is not.  */
    abi_tmp.base = isa_default_abi (&t.isa).base;
  else
    goto fatal;

  if (0)
fatal:
    fatal_error (UNKNOWN_LOCATION,
		 "unable to implement ABI %qs with instruction set %qs",
		 abi_str (t.abi), isa_str (&t.isa, '/'));


  /* Using the fallback ABI.  */
  if (abi_tmp.base != t.abi.base || abi_tmp.ext != t.abi.ext)
    {
      /* This flag is only set in the GCC driver.  */
      if (follow_multilib_list)
	{

	  /* Continue falling back until we find a feasible ABI type
	     enabled by TM_MULTILIB_LIST.  */
	  if (!is_multilib_enabled (abi_tmp))
	    {
	      for (unsigned int i = 0; i < ABI_COUNT; i++)
		{
		  if (is_multilib_enabled (abi_priority_list[i])
		      && abi_compat_p (&t.isa, abi_priority_list[i]))
		    {
		      abi_tmp = abi_priority_list[i];

		      warning (0, "ABI %qs cannot be implemented due to "
			       "limited instruction set %qs, "
			       "falling back to %qs", abi_str (t.abi),
			       isa_str (&t.isa, '/'), abi_str (abi_tmp));

		      goto fallback;
		    }
		}

	      /* Otherwise, keep using abi_tmp with a warning.  */
#ifdef LA_DISABLE_MULTILIB
	      warning (0, "instruction set %qs cannot implement "
		       "default ABI %qs, falling back to %qs",
		       isa_str (&t.isa, '/'), abi_str (t.abi),
		       abi_str (abi_tmp));
#else
	      warning (0, "no multilib-enabled ABI (%qs) can be implemented "
		       "with instruction set %qs, falling back to %qs",
		       multilib_enabled_abi_list (),
		       isa_str (&t.isa, '/'), abi_str (abi_tmp));
#endif
	    }
	}

fallback:
      t.abi = abi_tmp;
    }
  else if (follow_multilib_list)
    {
      if (!is_multilib_enabled (t.abi))
	{
	  inform (UNKNOWN_LOCATION,
		  "ABI %qs is not enabled at configure-time, "
		  "the linker might report an error", abi_str (t.abi));

	  inform (UNKNOWN_LOCATION, "ABI with startfiles: %s",
		  multilib_enabled_abi_list ());
	}
    }


  /* 5.  Target code model */
  t.cmodel = constrained.cmodel ? opt_cmodel : CMODEL_NORMAL;

  switch (t.cmodel)
    {
    case CMODEL_TINY:
    case CMODEL_TINY_STATIC:
    case CMODEL_LARGE:
      warning (0, "%qs is not supported, now cmodel is set to %qs",
	       loongarch_cmodel_strings[t.cmodel], "normal");
      t.cmodel = CMODEL_NORMAL;
      break;

    case CMODEL_NORMAL:
    case CMODEL_MEDIUM:
    case CMODEL_EXTREME:
      break;

    default:
      gcc_unreachable ();
    }

  /* Cleanup and return.  */
  obstack_free (&msg_obstack, NULL);
  *target = t;
}

/* Returns the default ABI for the given instruction set.  */
static inline struct loongarch_abi
isa_default_abi (const struct loongarch_isa *isa)
{
  struct loongarch_abi abi;

  switch (isa->fpu)
    {
      case ISA_EXT_FPU64:
	if (isa->base == ISA_BASE_LA64V100)
	  abi.base = ABI_BASE_LP64D;
	break;

      case ISA_EXT_FPU32:
	if (isa->base == ISA_BASE_LA64V100)
	  abi.base = ABI_BASE_LP64F;
	break;

      case ISA_EXT_NOFPU:
	if (isa->base == ISA_BASE_LA64V100)
	  abi.base = ABI_BASE_LP64S;
	break;

      default:
	gcc_unreachable ();
    }

  abi.ext = ABI_EXT_BASE;
  return abi;
}

/* Check if set2 is a subset of set1.  */
static inline int
isa_base_compat_p (const struct loongarch_isa *set1,
		   const struct loongarch_isa *set2)
{
  switch (set2->base)
    {
      case ISA_BASE_LA64V100:
	return (set1->base == ISA_BASE_LA64V100);

      default:
	gcc_unreachable ();
    }
}

static inline int
isa_fpu_compat_p (const struct loongarch_isa *set1,
		  const struct loongarch_isa *set2)
{
  switch (set2->fpu)
    {
      case ISA_EXT_FPU64:
	return set1->fpu == ISA_EXT_FPU64;

      case ISA_EXT_FPU32:
	return set1->fpu == ISA_EXT_FPU32 || set1->fpu == ISA_EXT_FPU64;

      case ISA_EXT_NOFPU:
	return 1;

      default:
	gcc_unreachable ();
    }

}

static inline int
abi_compat_p (const struct loongarch_isa *isa, struct loongarch_abi abi)
{
  int compatible = 1;
  const struct loongarch_isa *isa2 = &isa_required (abi);

  /* Append conditionals for new ISA components below.  */
  compatible = compatible && isa_base_compat_p (isa, isa2);
  compatible = compatible && isa_fpu_compat_p (isa, isa2);
  return compatible;
}

/* The behavior of this function should be consistent
   with config.gcc.  */
static inline int
abi_default_cpu_arch (struct loongarch_abi abi)
{
  switch (abi.base)
    {
      case ABI_BASE_LP64D:
      case ABI_BASE_LP64F:
      case ABI_BASE_LP64S:
	if (abi.ext == ABI_EXT_BASE)
	  return CPU_LOONGARCH64;
    }
  gcc_unreachable ();
}

static const char*
abi_str (struct loongarch_abi abi)
{
  /* "/base" can be omitted.  */
  if (abi.ext == ABI_EXT_BASE)
    return (const char*)
      obstack_copy0 (&msg_obstack, loongarch_abi_base_strings[abi.base],
		     strlen (loongarch_abi_base_strings[abi.base]));
  else
    {
      APPEND_STRING (loongarch_abi_base_strings[abi.base])
      APPEND1 ('/')
      APPEND_STRING (loongarch_abi_ext_strings[abi.ext])
      APPEND1 ('\0')

      return XOBFINISH (&msg_obstack, const char *);
    }
}

static const char*
isa_str (const struct loongarch_isa *isa, char separator)
{
  APPEND_STRING (loongarch_isa_base_strings[isa->base])
  APPEND1 (separator)

  if (isa->fpu == ISA_EXT_NOFPU)
    {
      APPEND_STRING ("no" OPTSTR_ISA_EXT_FPU)
    }
  else
    {
      APPEND_STRING (OPTSTR_ISA_EXT_FPU)
      APPEND_STRING (loongarch_isa_ext_strings[isa->fpu])
    }
  APPEND1 ('\0')

  /* Add more here.  */

  return XOBFINISH (&msg_obstack, const char *);
}

static const char*
arch_str (const struct loongarch_target *target)
{
  if (target->cpu_arch == CPU_NATIVE)
    {
      if (target->cpu_native == CPU_NATIVE)
	{
	  /* Describe a native CPU with unknown PRID.  */
	  const char* isa_string = isa_str (&target->isa, ',');
	  APPEND_STRING ("PRID: 0x")
	  APPEND_STRING (get_native_prid_str ())
	  APPEND_STRING (", ISA features: ")
	  APPEND_STRING (isa_string)
	  APPEND1 ('\0')
	}
      else
	APPEND_STRING (loongarch_cpu_strings[target->cpu_native]);
    }
  else
    APPEND_STRING (loongarch_cpu_strings[target->cpu_arch]);

  APPEND1 ('\0')
  return XOBFINISH (&msg_obstack, const char *);
}

static const char*
multilib_enabled_abi_list ()
{
  int enabled_abi_idx[MULTILIB_LIST_LEN] = { 0 };
  const char* enabled_abi_str[MULTILIB_LIST_LEN] = { NULL };
  unsigned int j = 0;

  for (unsigned int i = 0; i < ABI_COUNT && j < MULTILIB_LIST_LEN; i++)
    {
      if (enabled_abi_types[abi_priority_list[i].base]
	  [abi_priority_list[i].ext])
	{
	  enabled_abi_idx[j++] = i;
	}
    }

  for (unsigned int k = 0; k < j; k++)
    {
      enabled_abi_str[k] = abi_str (abi_priority_list[enabled_abi_idx[k]]);
    }

  for (unsigned int k = 0; k < j - 1; k++)
    {
      APPEND_STRING (enabled_abi_str[k])
      APPEND1 (',')
      APPEND1 (' ')
    }
  APPEND_STRING (enabled_abi_str[j - 1])
  APPEND1 ('\0')

  return XOBFINISH (&msg_obstack, const char *);
}
