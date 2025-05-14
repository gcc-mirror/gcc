/* Subroutines for loongarch-specific option handling.
   Copyright (C) 2021-2025 Free Software Foundation, Inc.
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
#include "opts.h"
#include "diagnostic-core.h"

#include "loongarch-cpu.h"
#include "loongarch-opts.h"
#include "loongarch-str.h"
#include "loongarch-def.h"

/* Target configuration */
struct loongarch_target la_target;

/* RTL cost information */
const struct loongarch_rtx_cost_data *loongarch_cost;

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
static int abi_default_cpu_arch (struct loongarch_abi abi,
				 struct loongarch_isa *isa);
static int default_tune_for_arch (int arch, int fallback);

/* Mandatory configure-time defaults.  */
#ifndef DEFAULT_ABI_BASE
#error missing definition of DEFAULT_ABI_BASE in ${tm_defines}.
#endif

#ifndef DEFAULT_ABI_EXT
#error missing definition of DEFAULT_ABI_EXT in ${tm_defines}.
#endif

#ifndef DEFAULT_CPU_ARCH
#error missing definition of DEFAULT_CPU_ARCH in ${tm_defines}.
#endif

/* Optional configure-time defaults.  */
#ifdef DEFAULT_CPU_TUNE
static int with_default_tune = 1;
#else
#define DEFAULT_CPU_TUNE -1
static int with_default_tune = 0;
#endif

#ifdef DEFAULT_ISA_EXT_FPU
static int with_default_fpu = 1;
#else
#define DEFAULT_ISA_EXT_FPU -1
static int with_default_fpu = 0;
#endif

#ifdef DEFAULT_ISA_EXT_SIMD
static int with_default_simd = 1;
#else
#define DEFAULT_ISA_EXT_SIMD -1
static int with_default_simd = 0;
#endif


/* Initialize loongarch_target from separate option variables.  */

void
loongarch_init_target (struct loongarch_target *target,
		       int cpu_arch, int cpu_tune, int fpu, int simd,
		       int abi_base, int abi_ext, int cmodel,
		       int tls_dialect,
		       HOST_WIDE_INT isa_evolution,
		       HOST_WIDE_INT isa_evolution_set)
{
  if (!target)
    return;
  target->cpu_arch = cpu_arch;
  target->cpu_tune = cpu_tune;
  target->isa.fpu = fpu;
  target->isa.simd = simd;
  target->isa.evolution = isa_evolution;
  target->isa.evolution_set = isa_evolution_set;
  target->abi.base = abi_base;
  target->abi.ext = abi_ext;
  target->cmodel = cmodel;
  target->tls_dialect = tls_dialect;
}


/* Handle combinations of -m parameters
   (see loongarch.opt and loongarch-opts.h).  */

void
loongarch_config_target (struct loongarch_target *target,
			 struct loongarch_flags *flags,
			 int follow_multilib_list_p)
{
  struct loongarch_target t;

  if (!target)
    return;

  /* Initialization */
  init_enabled_abi_types ();
  obstack_init (&msg_obstack);

  struct {
    int arch, tune, fpu, simd, abi_base, abi_ext, cmodel,
	tls_dialect, abi_flt;
  } constrained = {
      M_OPT_ABSENT (target->cpu_arch)	  ? 0 : 1,
      M_OPT_ABSENT (target->cpu_tune)	  ? 0 : 1,
      M_OPT_ABSENT (target->isa.fpu)	  ? 0 : 1,
      M_OPT_ABSENT (target->isa.simd)	  ? 0 : 1,
      M_OPT_ABSENT (target->abi.base)	  ? 0 : 1,
      M_OPT_ABSENT (target->abi.ext)	  ? 0 : 1,
      M_OPT_ABSENT (target->cmodel)	  ? 0 : 1,
      M_OPT_ABSENT (target->tls_dialect)  ? 0 : 1,
      M_OPT_ABSENT (target->abi.base)	  ? 0 : 1,
  };

  int64_t isa_evolution = target->isa.evolution;
  int64_t isa_evolution_set = target->isa.evolution_set;

  /* 1.  Target ABI */
  if (constrained.abi_base)
    t.abi.base = target->abi.base;
  else
    t.abi.base = DEFAULT_ABI_BASE;

  t.abi.ext = constrained.abi_ext ? target->abi.ext : DEFAULT_ABI_EXT;

  /* Process -m*-float flags */
  if (flags && !M_OPT_ABSENT (flags->flt))
    {
      /* Modifying the original "target" here makes it easier to write the
	 t.isa.fpu assignment below, because otherwise there would be three
	 levels of precedence (-m*-float / -mfpu / -march) to be handled
	 (now the first two are merged). */

      target->isa.fpu = flags->flt;
      constrained.fpu = 1;

      /* The target ISA is not ready yet, but (isa_required (t.abi)
	 + forced fpu) is enough for computing the forced base ABI.  */

      struct loongarch_isa force_isa = isa_required (t.abi);
      force_isa.fpu = flags->flt;

      struct loongarch_abi force_abi;
      force_abi.base = isa_default_abi (&force_isa).base;

      if (constrained.abi_base && constrained.abi_flt
	  && (t.abi.base != force_abi.base))
	{
	  force_abi.ext = t.abi.ext;
	  inform (UNKNOWN_LOCATION,
		  "%<-m%s%> overrides %<-m%s=%s%>, adjusting ABI to %qs",
		  flags->flt_str, OPTSTR_ABI_BASE,
		  loongarch_abi_base_strings[t.abi.base],
		  abi_str (force_abi));
	}

      t.abi.base = force_abi.base;
      constrained.abi_flt = 1;
    }

#ifdef LA_DISABLE_MULTILIB
  if (follow_multilib_list_p)
    if (t.abi.base != DEFAULT_ABI_BASE || t.abi.ext != DEFAULT_ABI_EXT)
      {
	static const struct loongarch_abi default_abi
	  = {DEFAULT_ABI_BASE, DEFAULT_ABI_EXT};

	warning (0, "ABI changed (%qs to %qs) while multilib is disabled",
		 abi_str (default_abi), abi_str (t.abi));
      }
#endif

  /* 2.  Target CPU */
  t.cpu_arch = constrained.arch ? target->cpu_arch : DEFAULT_CPU_ARCH;

  /* If cpu_tune is not set using neither -mtune nor --with-tune,
     the current cpu_arch is used as its default.  */
  t.cpu_tune = constrained.tune ? target->cpu_tune
    : (constrained.arch
       ? default_tune_for_arch (target->cpu_arch, with_default_tune
				? DEFAULT_CPU_TUNE : TUNE_GENERIC)
       : (with_default_tune ? DEFAULT_CPU_TUNE
	  : default_tune_for_arch (DEFAULT_CPU_ARCH, TUNE_GENERIC)));


  /* Handle -march/tune=native */
#ifdef __loongarch__
  /* For native compilers, gather local CPU information
     and fill the "ARCH_NATIVE/TUNE_NATIVE" index of arrays
     defined in loongarch-cpu.c.  */

  fill_native_cpu_config (&t);

#else
  if (t.cpu_arch == ARCH_NATIVE)
    fatal_error (UNKNOWN_LOCATION,
		 "%qs does not work on a cross compiler",
		 "-m" OPTSTR_ARCH "=" STR_CPU_NATIVE);

  else if (t.cpu_tune == TUNE_NATIVE)
    fatal_error (UNKNOWN_LOCATION,
		 "%qs does not work on a cross compiler",
		 "-m" OPTSTR_TUNE "=" STR_CPU_NATIVE);
#endif

  /* Handle -march=abi-default */
  if (t.cpu_arch == ARCH_ABI_DEFAULT)
    {
      t.cpu_arch = abi_default_cpu_arch (t.abi, &(t.isa));
      loongarch_cpu_default_isa[t.cpu_arch] = t.isa;
    }

  /* 3.  Target base ISA */
config_target_isa:

  /* Get default ISA from "-march" or its default value.  */
  t.isa = loongarch_cpu_default_isa[t.cpu_arch];

  /* Apply incremental changes.  */
  /* "-march=native" overrides the default FPU type.  */

  t.isa.fpu = constrained.fpu ? target->isa.fpu :
    (constrained.arch ? t.isa.fpu :
    (with_default_fpu ? DEFAULT_ISA_EXT_FPU : t.isa.fpu));

  int simd_base = (constrained.arch ? t.isa.simd :
    (with_default_simd ? DEFAULT_ISA_EXT_SIMD : t.isa.simd));

  t.isa.simd = constrained.simd ? target->isa.simd : simd_base;

  /* If fallback_lsx is set, using -mno-lasx would cause
     a fall-back to -msimd=lsx instead of -msimd=none.  */

  int fallback_lsx = 0;
  if (t.isa.simd == ISA_EXT_SIMD_LSX || simd_base != ISA_EXT_NONE)
    fallback_lsx = 1;

  /* apply -m[no-]lsx and -m[no-]lasx flags */
  if (flags)
    for (int i = 0; i < 2; i++)
      {
	switch (SX_FLAG_TYPE (flags->sx[i]))
	  {
	  case ISA_EXT_SIMD_LSX:
	    constrained.simd = 1;

	    if (flags->sx[i] > 0)
	      fallback_lsx = 1;

	    if (flags->sx[i] > 0 && t.isa.simd != ISA_EXT_SIMD_LASX)
	      t.isa.simd = ISA_EXT_SIMD_LSX;
	    else if (flags->sx[i] < 0)
	      t.isa.simd = ISA_EXT_NONE;
	    break;

	  case ISA_EXT_SIMD_LASX:
	    constrained.simd = 1;
	    /* If -mlsx or simd=lsx (msimd or march-default) was not
	       involved, do not fall back to simd=lsx.  */
	    if (flags->sx[i] < 0 && t.isa.simd == ISA_EXT_SIMD_LASX)
	      t.isa.simd = fallback_lsx ? ISA_EXT_SIMD_LSX : ISA_EXT_NONE;
	    else if (flags->sx[i] > 0)
	      t.isa.simd = ISA_EXT_SIMD_LASX;
	    break;

	  case 0:
	    break;

	  default:
	    gcc_unreachable ();
	  }
      }

  /* All SIMD extensions imply a 64-bit FPU:
     - silently adjust t.isa.fpu to "fpu64" if it is unconstrained.
     - warn if -msingle-float / -msoft-float is on,
       then disable SIMD extensions (done in driver)
     - abort if -mfpu=0 / -mfpu=32 is forced.  */

  if (t.isa.simd != ISA_EXT_NONE && t.isa.fpu != ISA_EXT_FPU64)
    {
      if (!constrained.fpu)
	{
	  /* As long as the arch-default "t.isa.simd" is set to non-zero
	     for an element "t" in loongarch_cpu_default_isa, "t.isa.fpu"
	     should be set to "ISA_EXT_FPU64" accordingly.  Thus reaching
	     here must be the result of forcing -mlsx/-mlasx explicitly.  */
	  gcc_assert (constrained.simd);

	  inform (UNKNOWN_LOCATION,
		  "enabling %qs promotes %<%s%s%> to %<%s%s%>",
		  loongarch_isa_ext_strings[t.isa.simd],
		  OPTSTR_ISA_EXT_FPU, loongarch_isa_ext_strings[t.isa.fpu],
		  OPTSTR_ISA_EXT_FPU, loongarch_isa_ext_strings[ISA_EXT_FPU64]);

	  t.isa.fpu = ISA_EXT_FPU64;
	}
      else if (flags && (flags->flt == ISA_EXT_NONE
			 || flags->flt == ISA_EXT_FPU32))
	{
	  if (constrained.simd)
	    inform (UNKNOWN_LOCATION,
		    "%qs is disabled by %<-m%s%>, because it requires %<%s%s%>",
		    loongarch_isa_ext_strings[t.isa.simd], flags->flt_str,
		    OPTSTR_ISA_EXT_FPU,
		    loongarch_isa_ext_strings[ISA_EXT_FPU64]);

	  t.isa.simd = ISA_EXT_NONE;
	}
      else
	{
	  /* -mfpu=0 / -mfpu=32 is set.  */
	  if (constrained.simd)
	    fatal_error (UNKNOWN_LOCATION,
			 "%<-m%s=%s%> conflicts with %qs, "
			 "which requires %<%s%s%>",
			 OPTSTR_ISA_EXT_FPU,
			 loongarch_isa_ext_strings[t.isa.fpu],
			 loongarch_isa_ext_strings[t.isa.simd],
			 OPTSTR_ISA_EXT_FPU,
			 loongarch_isa_ext_strings[ISA_EXT_FPU64]);

	  /* Same as above.  */
	  t.isa.simd = ISA_EXT_NONE;
	}
    }

  /* Apply the ISA evolution feature switches from the user.  */
  HOST_WIDE_INT isa_evolution_orig = t.isa.evolution;
  t.isa.evolution &= ~(~isa_evolution & isa_evolution_set);
  t.isa.evolution |= isa_evolution & isa_evolution_set;

  /* evolution_set means "what's different from the -march default".  */
  t.isa.evolution_set = isa_evolution_orig ^ t.isa.evolution;

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
      int fallback_arch = abi_default_cpu_arch (t.abi, NULL);

      if (t.cpu_arch == ARCH_NATIVE)
	warning (0, "your native CPU architecture (%qs) "
		 "does not support %qs ABI, falling back to %<-m%s=%s%>",
		 arch_str (&t), abi_str (t.abi), OPTSTR_ARCH,
		 loongarch_arch_strings[fallback_arch]);
      else
	warning (0, "default CPU architecture (%qs) "
		 "does not support %qs ABI, falling back to %<-m%s=%s%>",
		 arch_str (&t), abi_str (t.abi), OPTSTR_ARCH,
		 loongarch_arch_strings[fallback_arch]);

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
      if (follow_multilib_list_p)
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
  else if (follow_multilib_list_p)
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
  t.cmodel = constrained.cmodel ? target->cmodel : CMODEL_NORMAL;

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

  t.tls_dialect = constrained.tls_dialect ? target->tls_dialect
	  : DEFAULT_TLS_TYPE;

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
	if (isa->base >= ISA_BASE_LA64)
	  abi.base = ABI_BASE_LP64D;
	break;

      case ISA_EXT_FPU32:
	if (isa->base >= ISA_BASE_LA64)
	  abi.base = ABI_BASE_LP64F;
	break;

      case ISA_EXT_NONE:
	if (isa->base >= ISA_BASE_LA64)
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
      case ISA_BASE_LA64:
	return (set1->base >= ISA_BASE_LA64);

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

      case ISA_EXT_NONE:
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
abi_default_cpu_arch (struct loongarch_abi abi,
		      struct loongarch_isa *isa)
{
  static struct loongarch_isa tmp;
  if (!isa)
    isa = &tmp;

  if (abi.ext == ABI_EXT_BASE)
    switch (abi.base)
      {
	case ABI_BASE_LP64D:
	case ABI_BASE_LP64F:
	case ABI_BASE_LP64S:
	  *isa = isa_required (abi);
	  return ARCH_LOONGARCH64;
      }
  gcc_unreachable ();
}

static inline int
default_tune_for_arch (int arch, int fallback)
{
  int ret;
  switch (arch)
    {

#define TUNE_FOR_ARCH(NAME) \
    case ARCH_##NAME: \
      ret = TUNE_##NAME; \
      break;

    TUNE_FOR_ARCH(NATIVE)
    TUNE_FOR_ARCH(LOONGARCH64)
    TUNE_FOR_ARCH(LA464)
    TUNE_FOR_ARCH(LA664)

#undef TUNE_FOR_ARCH

    case ARCH_ABI_DEFAULT:
    case ARCH_LA64V1_0:
    case ARCH_LA64V1_1:
      ret = fallback;
    }

  gcc_assert (0 <= ret && ret < N_TUNE_TYPES);
  return ret;
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
      /* This situation has not yet occurred, so in order to avoid the
	 -Warray-bounds warning during C++ syntax checking, this part
	 of the code is commented first.  */
      /*
      APPEND_STRING (loongarch_abi_base_strings[abi.base])
      APPEND1 ('/')
      APPEND_STRING (loongarch_abi_ext_strings[abi.ext])
      APPEND1 ('\0')

      return XOBFINISH (&msg_obstack, const char *);
      */
      gcc_unreachable ();
    }
}

static const char*
isa_str (const struct loongarch_isa *isa, char separator)
{
  APPEND_STRING (loongarch_isa_base_strings[isa->base])
  APPEND1 (separator)

  if (isa->fpu == ISA_EXT_NONE)
    {
      APPEND_STRING ("no" OPTSTR_ISA_EXT_FPU)
    }
  else
    {
      APPEND_STRING (OPTSTR_ISA_EXT_FPU)
      APPEND_STRING (loongarch_isa_ext_strings[isa->fpu])
    }

  switch (isa->simd)
    {
      case ISA_EXT_SIMD_LSX:
      case ISA_EXT_SIMD_LASX:
	APPEND1 (separator);
	APPEND_STRING (loongarch_isa_ext_strings[isa->simd]);
	break;

      default:
	gcc_assert (isa->simd == 0);
    }
  APPEND1 ('\0')

  /* Add more here.  */

  return XOBFINISH (&msg_obstack, const char *);
}

static const char*
arch_str (const struct loongarch_target *target)
{
  if (target->cpu_arch == ARCH_NATIVE)
    {
      /* Describe a native CPU with unknown PRID.  */
      const char* isa_string = isa_str (&target->isa, ',');
      APPEND_STRING ("PRID: 0x")
      APPEND_STRING (get_native_prid_str ())
      APPEND_STRING (", ISA features: ")
      APPEND_STRING (isa_string)
    }
  else
    APPEND_STRING (loongarch_arch_strings[target->cpu_arch]);

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

/* option status feedback for "gcc --help=target -Q" */
void
loongarch_update_gcc_opt_status (struct loongarch_target *target,
				 struct gcc_options *opts,
				 struct gcc_options *opts_set)
{
  (void) opts_set;

  /* status of -mabi */
  opts->x_la_opt_abi_base = target->abi.base;

  /* status of -march and -mtune */
  opts->x_la_opt_cpu_arch = target->cpu_arch;
  opts->x_la_opt_cpu_tune = target->cpu_tune;

  /* status of -mcmodel */
  opts->x_la_opt_cmodel = target->cmodel;

  /* status of -mtls-dialect */
  opts->x_la_opt_tls_dialect = target->tls_dialect;

  /* status of -mfpu */
  opts->x_la_opt_fpu = target->isa.fpu;

  /* status of -msimd */
  opts->x_la_opt_simd = target->isa.simd;

  /* ISA evolution features */
  opts->x_la_isa_evolution = target->isa.evolution;
}

/* -mrecip=<str> handling */
static struct
  {
    const char *string;	    /* option name.  */
    unsigned int mask;	    /* mask bits to set.  */
  }
const recip_options[] = {
      { "all",       RECIP_MASK_ALL },
      { "none",      RECIP_MASK_NONE },
      { "div",       RECIP_MASK_DIV },
      { "sqrt",      RECIP_MASK_SQRT },
      { "rsqrt",     RECIP_MASK_RSQRT },
      { "vec-div",   RECIP_MASK_VEC_DIV },
      { "vec-sqrt",  RECIP_MASK_VEC_SQRT },
      { "vec-rsqrt", RECIP_MASK_VEC_RSQRT },
};

/* Parser for -mrecip=<recip_string>.  */
unsigned int
loongarch_parse_mrecip_scheme (const char *recip_string)
{
  unsigned int result_mask = RECIP_MASK_NONE;

  if (recip_string)
    {
      char *p = ASTRDUP (recip_string);
      char *q;
      unsigned int mask, i;
      bool invert;

      while ((q = strtok (p, ",")) != NULL)
	{
	  p = NULL;
	  if (*q == '!')
	    {
	      invert = true;
	      q++;
	    }
	  else
	    invert = false;

	  if (!strcmp (q, "default"))
	    mask = RECIP_MASK_ALL;
	  else
	    {
	      for (i = 0; i < ARRAY_SIZE (recip_options); i++)
		if (!strcmp (q, recip_options[i].string))
		  {
		    mask = recip_options[i].mask;
		    break;
		  }

	      if (i == ARRAY_SIZE (recip_options))
		{
		  error ("unknown option for %<-mrecip=%s%>", q);
		  invert = false;
		  mask = RECIP_MASK_NONE;
		}
	    }

	  if (invert)
	    result_mask &= ~mask;
	  else
	    result_mask |= mask;
	}
    }
  return result_mask;
}

/* Generate -mrecip= argument based on the mask.  */
const char*
loongarch_generate_mrecip_scheme (unsigned int mask)
{
  static char recip_scheme_str[128];
  int p = 0, tmp;

  switch (mask)
    {
      case RECIP_MASK_ALL:
	return "all";

      case RECIP_MASK_NONE:
	return "none";
    }

  for (unsigned long i = 2; i < ARRAY_SIZE (recip_options); i++)
    {
      if (mask & recip_options[i].mask)
	{
	  if ((tmp = strlen (recip_options[i].string) + 1) >= 127 - p)
	    gcc_unreachable ();

	  recip_scheme_str[p] = ',';
	  strcpy (recip_scheme_str + p + 1, recip_options[i].string);
	  p += tmp;
	}
    }
  recip_scheme_str[p] = '\0';
  return recip_scheme_str + 1;
}



/* Refresh the switches acccording to the resolved loongarch_target struct.  */
void
loongarch_target_option_override (struct loongarch_target *target,
				  struct gcc_options *opts,
				  struct gcc_options *opts_set)
{
  loongarch_update_gcc_opt_status (target, opts, opts_set);

  /* If not optimizing for size, set the default
     alignment to what the target wants.  */
  if (!opts->x_optimize_size)
    {
      if (opts->x_flag_align_functions && !opts->x_str_align_functions)
	opts->x_str_align_functions
	  = loongarch_cpu_align[target->cpu_tune].function;

      if (opts->x_flag_align_loops && !opts->x_str_align_loops)
	opts->x_str_align_loops = loongarch_cpu_align[target->cpu_tune].loop;

      if (opts->x_flag_align_jumps && !opts->x_str_align_jumps)
	opts->x_str_align_jumps = loongarch_cpu_align[target->cpu_tune].jump;

      if (opts->x_flag_align_labels && !opts->x_str_align_labels)
	opts->x_str_align_labels = loongarch_cpu_align[target->cpu_tune].label;
    }

  /* Set up parameters to be used in prefetching algorithm.  */
  int simultaneous_prefetches
    = loongarch_cpu_cache[target->cpu_tune].simultaneous_prefetches;

  SET_OPTION_IF_UNSET (opts, opts_set, param_simultaneous_prefetches,
		       simultaneous_prefetches);

  SET_OPTION_IF_UNSET (opts, opts_set, param_l1_cache_line_size,
		       loongarch_cpu_cache[target->cpu_tune].l1d_line_size);

  SET_OPTION_IF_UNSET (opts, opts_set, param_l1_cache_size,
		       loongarch_cpu_cache[target->cpu_tune].l1d_size);

  SET_OPTION_IF_UNSET (opts, opts_set, param_l2_cache_size,
		       loongarch_cpu_cache[target->cpu_tune].l2d_size);

  /* Other arch-specific overrides.  */
  switch (target->cpu_arch)
    {
      case ARCH_LA664:
	/* Enable -mrecipe=all for LA664 by default.  */
	if (!opts_set->x_recip_mask)
	  {
	    opts->x_recip_mask = RECIP_MASK_ALL;
	    opts_set->x_recip_mask = 1;
	  }
    }

  /* -mrecip= */
  opts->x_la_recip_name
    = loongarch_generate_mrecip_scheme (opts->x_recip_mask);

  /* Decide which rtx_costs structure to use.  */
  if (opts->x_optimize_size)
    loongarch_cost = &loongarch_rtx_cost_optimize_size;
  else
    loongarch_cost = &loongarch_cpu_rtx_cost_data[target->cpu_tune];

  /* If the user hasn't specified a branch cost, use the processor's
     default.  */
  if (!opts_set->x_la_branch_cost)
    opts->x_la_branch_cost = loongarch_cost->branch_cost;

  if (!opts_set->x_la_addr_reg_reg_cost)
    opts->x_la_addr_reg_reg_cost = loongarch_cost->addr_reg_reg_cost;

  /* other stuff */
  if (ABI_LP64_P (target->abi.base))
    opts->x_flag_pcc_struct_return = 0;

  switch (target->cmodel)
    {
      case CMODEL_EXTREME:
	if (opts->x_flag_plt)
	  {
	    if (opts_set->x_flag_plt)
	      error ("code model %qs is not compatible with %s",
		     "extreme", "-fplt");
	    opts->x_flag_plt = 0;
	  }
	break;

      case CMODEL_TINY_STATIC:
      case CMODEL_MEDIUM:
      case CMODEL_NORMAL:
      case CMODEL_TINY:
      case CMODEL_LARGE:
	break;

      default:
	gcc_unreachable ();
    }
}


/* Resolve options that's not covered by la_target.  */
void
loongarch_init_misc_options (struct gcc_options *opts,
			     struct gcc_options *opts_set)
{
  if (opts->x_flag_pic)
    opts->x_g_switch_value = 0;

  /* -mrecip options.  */
  opts->x_recip_mask = loongarch_parse_mrecip_scheme (opts->x_la_recip_name);

#define INIT_TARGET_FLAG(NAME, INIT) \
  { \
    if (!(opts_set->x_target_flags & MASK_##NAME)) \
      { \
	if (INIT) \
	  opts->x_target_flags |= MASK_##NAME; \
	else \
	  opts->x_target_flags &= ~MASK_##NAME; \
      } \
  }

  /* Enable conditional moves for int and float by default.  */
  INIT_TARGET_FLAG (COND_MOVE_INT, 1)
  INIT_TARGET_FLAG (COND_MOVE_FLOAT, 1)

  /* Set mrelax default.  */
  INIT_TARGET_FLAG (LINKER_RELAXATION,
		    HAVE_AS_MRELAX_OPTION && HAVE_AS_COND_BRANCH_RELAXATION)

#undef INIT_TARGET_FLAG

  /* Set mexplicit-relocs default.  */
  if (opts->x_la_opt_explicit_relocs == M_OPT_UNSET)
    opts->x_la_opt_explicit_relocs = (HAVE_AS_EXPLICIT_RELOCS
				      ? (TARGET_LINKER_RELAXATION
					 ? EXPLICIT_RELOCS_AUTO
					 : EXPLICIT_RELOCS_ALWAYS)
				      : EXPLICIT_RELOCS_NONE);

  /* Enable sw prefetching at -O3 and higher.  */
  if (opts->x_flag_prefetch_loop_arrays < 0
      && (opts->x_optimize >= 3 || opts->x_flag_profile_use)
      && !opts->x_optimize_size)
    opts->x_flag_prefetch_loop_arrays = 1;

  if (TARGET_DIRECT_EXTERN_ACCESS_OPTS_P (opts) && opts->x_flag_shlib)
    error ("%qs cannot be used for compiling a shared library",
	   "-mdirect-extern-access");

  /* Enforce that interval is the same size as size so the mid-end does the
     right thing.  */
  SET_OPTION_IF_UNSET (opts, opts_set,
		       param_stack_clash_protection_probe_interval,
		       param_stack_clash_protection_guard_size);
}
