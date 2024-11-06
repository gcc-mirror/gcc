/* Subroutines for the gcc driver.
   Copyright (C) 2007-2024 Free Software Foundation, Inc.

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "diagnostic.h"
#include "opts.h"
#include <stdlib.h>

#ifdef _AIX
# include <sys/systemcfg.h>
#endif

#ifdef __linux__
# include <link.h>
#endif

#if defined (__APPLE__) || (__FreeBSD__)
# include <sys/types.h>
# include <sys/sysctl.h>
#endif

#ifdef __linux__
/* Canonical GCC cpu name table.  */
static const char *rs6000_supported_cpu_names[] =
{
#define RS6000_CPU(NAME, CPU, FLAGS) NAME,
#include "rs6000-cpus.def"
#undef RS6000_CPU
};

/* This table holds a list of cpus where their Linux AT_PLATFORM name differs
   from their GCC canonical name.  The first column in a row contains the GCC
   canonical cpu name and the other columns in that row contain AT_PLATFORM
   names that should be mapped to the canonical name.  */

static const char *linux_cpu_translation_table[][4] = {
  { "403", "ppc403", NULL },
  { "405", "ppc405", NULL },
  { "440", "ppc440", "ppc440gp", NULL },
  { "476", "ppc470", NULL },
  { "601", "ppc601", NULL },
  { "603", "ppc603", NULL },
  { "604", "ppc604", NULL },
  { "7400", "ppc7400", NULL },
  { "7450", "ppc7450", NULL },
  { "750", "ppc750", NULL },
  { "823", "ppc823", NULL },
  { "8540", "ppc8540", NULL },
  { "8548", "ppc8548", NULL },
  { "970", "ppc970", NULL },
  { "cell", "ppc-cell-be", NULL },
  { "e500mc", "ppce500mc", NULL },
  { "e5500", "ppce5500", NULL },
  { "e6500", "ppce6500", NULL },
  { "power7", "power7+", NULL },
  { NULL } /* End of table sentinel.  */
};
#endif

const char *host_detect_local_cpu (int argc, const char **argv);

#if GCC_VERSION >= 0

/* Returns parameters that describe L1_ASSOC associative cache of size
   L1_SIZEKB with lines of size L1_LINE, and L2_SIZEKB.  */

static char *
describe_cache (unsigned l1_sizekb, unsigned l1_line,
		unsigned l1_assoc ATTRIBUTE_UNUSED, unsigned l2_sizekb)
{
  char l1size[1000], line[1000], l2size[1000];

  /* At the moment, gcc middle-end does not use the information about the
     associativity of the cache.  */

  sprintf (l1size, "--param l1-cache-size=%u", l1_sizekb);
  sprintf (line, "--param l1-cache-line-size=%u", l1_line);
  sprintf (l2size, "--param l2-cache-size=%u", l2_sizekb);

  return concat (l1size, " ", line, " ", l2size, " ", NULL);
}

#ifdef __APPLE__

/* Returns the description of caches on Darwin.  */

static char *
detect_caches_darwin (void)
{
  unsigned l1_sizekb, l1_line, l1_assoc, l2_sizekb;
  size_t len = 4;
  static int l1_size_name[2] = { CTL_HW, HW_L1DCACHESIZE };
  static int l1_line_name[2] = { CTL_HW, HW_CACHELINE };
  static int l2_size_name[2] = { CTL_HW, HW_L2CACHESIZE };

  sysctl (l1_size_name, 2, &l1_sizekb, &len, NULL, 0);
  sysctl (l1_line_name, 2, &l1_line, &len, NULL, 0);
  sysctl (l2_size_name, 2, &l2_sizekb, &len, NULL, 0);
  l1_assoc = 0;

  return describe_cache (l1_sizekb / 1024, l1_line, l1_assoc,
			 l2_sizekb / 1024);
}

static const char *
detect_processor_darwin (void)
{
  unsigned int proc;
  size_t len = 4;

  sysctlbyname ("hw.cpusubtype", &proc, &len, NULL, 0);

  if (len > 0)
    switch (proc)
      {
      case 1:
	return "601";
      case 2:
	return "602";
      case 3:
	return "603";
      case 4:
      case 5:
	return "603e";
      case 6:
	return "604";
      case 7:
	return "604e";
      case 8:
	return "620";
      case 9:
	return "750";
      case 10:
	return "7400";
      case 11:
	return "7450";
      case 100:
	return "970";
      default:
	return "powerpc";
      }

  return "powerpc";
}

#endif /* __APPLE__ */

#ifdef __FreeBSD__

/* Returns the description of caches on FreeBSD PPC.  */

static char *
detect_caches_freebsd (void)
{
  unsigned l1_sizekb, l1_line, l1_assoc, l2_sizekb;
  size_t len = 4;

  /* Currently, as of FreeBSD-7.0, there is only the cacheline_size
     available via sysctl.  */
  sysctlbyname ("machdep.cacheline_size", &l1_line, &len, NULL, 0);

  l1_sizekb = 32;
  l1_assoc = 0;
  l2_sizekb = 512;

  return describe_cache (l1_sizekb, l1_line, l1_assoc, l2_sizekb);
}

/* Currently returns default powerpc.  */
static const char *
detect_processor_freebsd (void)
{
  return "powerpc";
}

#endif /* __FreeBSD__  */

#ifdef __linux__

/* Returns the canonical AT_PLATFORM if present, otherwise NULL.  */

static const char *
elf_platform (void)
{
  /* Used to cache the result we determine below.  */
  static const char *cpu = NULL;

  /* Use the cached AT_PLATFORM cpu name if we've already determined it.  */
  if (cpu != NULL)
    return cpu;

  int fd = open ("/proc/self/auxv", O_RDONLY);

  if (fd != -1)
    {
      char buf[1024];
      ElfW(auxv_t) *av;
      ssize_t n;

      n = read (fd, buf, sizeof (buf));
      close (fd);

      if (n > 0)
	{
	  for (av = (ElfW(auxv_t) *) buf; av->a_type != AT_NULL; ++av)
	    if (av->a_type == AT_PLATFORM)
	      {
		/* Cache the result.  */
		cpu = (const char *) av->a_un.a_val;
		break;
	      }
	}

      /* Verify that CPU is either a valid -mcpu=<cpu> option name, or is a
	 valid alternative name.  If it is a valid alternative name, then use
	 the canonical name.  */
      if (cpu != NULL)
	{
	  size_t i, j;
	  char *s;

	  /* Check if AT_PLATFORM is a GCC canonical cpu name.  */
	  for (i = 0; i < ARRAY_SIZE (rs6000_supported_cpu_names); i++)
	    if (!strcmp (cpu, rs6000_supported_cpu_names[i]))
	      return cpu;

	  /* Check if AT_PLATFORM can be translated to a canonical cpu name.  */
	  for (i = 0; linux_cpu_translation_table[i][0] != NULL; i++)
	    {
	      const char *canonical = linux_cpu_translation_table[i][0];
	      for (j = 1; linux_cpu_translation_table[i][j] != NULL; j++)
		if (!strcmp (cpu, linux_cpu_translation_table[i][j]))
		  {
		    /* Cache the result.  */
		    cpu = canonical;
		    return cpu;
		  }
	    }

	  /* The kernel returned an AT_PLATFORM name we do not support.  */
	  auto_vec <const char *> candidates;
	  for (i = 0; i < ARRAY_SIZE (rs6000_supported_cpu_names); i++)
	    candidates.safe_push (rs6000_supported_cpu_names[i]);
	  candidates_list_and_hint (cpu, s, candidates);
	  error ("unsupported cpu name returned from kernel "
		 "for %<-mcpu=native%>: %s", cpu);
	  fatal_error (input_location, "please use an explicit cpu name; "
		       "valid cpu names are: %s", s);
	}
    }
  return NULL;
}

/* Returns AT_DCACHEBSIZE if present, otherwise generic 32.  */

static int
elf_dcachebsize (void)
{
  int fd;

  fd = open ("/proc/self/auxv", O_RDONLY);

  if (fd != -1)
    {
      char buf[1024];
      ElfW(auxv_t) *av;
      ssize_t n;

      n = read (fd, buf, sizeof (buf));
      close (fd);

      if (n > 0)
	{
	  for (av = (ElfW(auxv_t) *) buf; av->a_type != AT_NULL; ++av)
	    switch (av->a_type)
	      {
	      case AT_DCACHEBSIZE:
		return av->a_un.a_val;

	      default:
		break;
	      }
	}
    }
  return 32;
}

/* Returns the description of caches on Linux.  */

static char *
detect_caches_linux (void)
{
  unsigned l1_sizekb, l1_line, l1_assoc, l2_sizekb;
  const char *platform;

  platform = elf_platform ();

  if (platform != NULL)
    {
      l1_line = 128;

      if (platform[5] == '6')
	/* POWER6 and POWER6x */
	l1_sizekb = 64;
      else
	l1_sizekb = 32;
    }
  else
    {
      l1_line = elf_dcachebsize ();
      l1_sizekb = 32;
    }

  l1_assoc = 0;
  l2_sizekb = 512;

  return describe_cache (l1_sizekb, l1_line, l1_assoc, l2_sizekb);
}

static const char *
detect_processor_linux (void)
{
  const char *platform;

  platform = elf_platform ();

  if (platform != NULL)
    return platform;
  else
    return "powerpc";
}

#endif /* __linux__ */

#ifdef _AIX
/* Returns the description of caches on AIX.  */

static char *
detect_caches_aix (void)
{
  unsigned l1_sizekb, l1_line, l1_assoc, l2_sizekb;

  l1_sizekb = _system_configuration.dcache_size / 1024;
  l1_line = _system_configuration.dcache_line;
  l1_assoc = _system_configuration.dcache_asc;
  l2_sizekb = _system_configuration.L2_cache_size / 1024;

  return describe_cache (l1_sizekb, l1_line, l1_assoc, l2_sizekb);
}


/* Returns the processor implementation on AIX.  */

static const char *
detect_processor_aix (void)
{
  switch (_system_configuration.implementation)
    {
    case 0x0008:
      return "601";

    case 0x0020:
      return "603";

    case 0x0010:
      return "604";

    case 0x0040:
      return "620";

    case 0x0080:
      return "630";

    case 0x0100:
    case 0x0200:
    case 0x0400:
      return "rs64";

    case 0x0800:
      return "power4";

    case 0x2000:
      if (_system_configuration.version == 0x0F0000)
	return "power5";
      else
	return "power5+";

    case 0x4000:
      return "power6";

    case 0x8000:
      return "power7";

    case 0x10000:
      return "power8";

    case 0x20000:
      return "power9";

    case 0x40000:
      return "power10";

    default:
      return "powerpc";
    }
}
#endif /* _AIX */


/*
 * Array to map -mcpu=native names to the switches passed to the assembler.
 * This list mirrors the specs in ASM_CPU_SPEC, and any changes made here
 * should be made there as well.
 */

struct asm_name {
  const char *cpu;
  const char *asm_sw;
};

static const struct asm_name asm_names[] = {
#if defined (_AIX)
  { "power3",	"-m620" },
  { "power4",	"-mpwr4" },
  { "power5",	"-mpwr5" },
  { "power5+",	"-mpwr5x" },
  { "power6",	"-mpwr6" },
  { "power6x",	"-mpwr6" },
  { "power7",	"-mpwr7" },
  { "power8",	"-mpwr8" },
  { "power9",	"-mpwr9" },
  { "power10",	"-mpwr10" },
  { "power11",	"-mpwr11" },
  { "powerpc",	"-mppc" },
  { "rs64",	"-mppc" },
  { "603",	"-m603" },
  { "603e",	"-m603" },
  { "604",	"-m604" },
  { "604e",	"-m604" },
  { "620",	"-m620" },
  { "630",	"-m620" },
  { "970",	"-m970" },
  { "G5",	"-m970" },
  { NULL,	"\
  %{mvsx: -mpwr6; \
    maltivec: -m970; \
    maix64|mpowerpc64: -mppc64; \
    : %(asm_default)}" },

#else
  { "cell",	"-mcell" },
  { "power3",	"-mppc64" },
  { "power4",	"-mpower4" },
  { "power5",	"-mpower5" },
  { "power5+",	"-mpower5" },
  { "power6",	"-mpower6 %{!mvsx:%{!maltivec:-maltivec}}" },
  { "power6x",	"-mpower6 %{!mvsx:%{!maltivec:-maltivec}}" },
  { "power7",	"-mpower7" },
  { "power8",	"-mpower8" },
  { "power9",	"-mpower9" },
  { "power10",	"-mpower10" },
  { "power11",	"-mpower11" },
  { "a2",	"-ma2" },
  { "powerpc",	"-mppc" },
  { "powerpc64", "-mppc64" },
  { "powerpc64le", "-mpower8" },
  { "rs64",	"-mppc64" },
  { "401",	"-mppc" },
  { "403",	"-m403" },
  { "405",	"-m405" },
  { "405fp",	"-m405" },
  { "440",	"-m440" },
  { "440fp",	"-m440" },
  { "464",	"-m440" },
  { "464fp",	"-m440" },
  { "476",	"-m476" },
  { "476fp",	"-m476" },
  { "505",	"-mppc" },
  { "601",	"-m601" },
  { "602",	"-mppc" },
  { "603",	"-mppc" },
  { "603e",	"-mppc" },
  { "ec603e",	"-mppc" },
  { "604",	"-mppc" },
  { "604e",	"-mppc" },
  { "620",	"-mppc64" },
  { "630",	"-mppc64" },
  { "740",	"-mppc" },
  { "750",	"-mppc" },
  { "G3",	"-mppc" },
  { "7400",	"-mppc %{!mvsx:%{!maltivec:-maltivec}}" },
  { "7450",	"-mppc %{!mvsx:%{!maltivec:-maltivec}}" },
  { "G4",	"-mppc %{!mvsx:%{!maltivec:-maltivec}}" },
  { "801",	"-mppc" },
  { "821",	"-mppc" },
  { "823",	"-mppc" },
  { "860",	"-mppc" },
  { "970",	"-mpower4 %{!mvsx:%{!maltivec:-maltivec}}" },
  { "G5",	"-mpower4 %{!mvsx:%{!maltivec:-maltivec}}" },
  { "8540",	"-me500" },
  { "8548",	"-me500" },
  { "e300c2",	"-me300" },
  { "e300c3",	"-me300" },
  { "e500mc",	"-me500mc" },
  { "e500mc64",	"-me500mc64" },
  { "e5500",	"-me5500" },
  { "e6500",	"-me6500" },
  { "titan",	"-mtitan" },
  { NULL,	"\
%{mcrypto|mdirect-move|mhtm: -mpower8; \
  mvsx: -mpower7; \
  mpowerpc64: -mppc64; \
  : %(asm_default)}" },
#endif
};

/* This will be called by the spec parser in gcc.cc when it sees
   a %:local_cpu_detect(args) construct.  Currently it will be called
   with either "arch" or "tune" as argument depending on if -march=native
   or -mtune=native is to be substituted.

   Additionally it will be called with "asm" to select the appropriate flags
   for the assembler.

   It returns a string containing new command line parameters to be
   put at the place of the above two options, depending on what CPU
   this is executed.

   ARGC and ARGV are set depending on the actual arguments given
   in the spec.  */
const char *
host_detect_local_cpu (int argc, const char **argv)
{
  const char *cpu = NULL;
  const char *cache = "";
  const char *options = "";
  bool arch;
  bool assembler;
  size_t i;

  if (argc < 1)
    return NULL;

  arch = strcmp (argv[0], "cpu") == 0;
  assembler = (!arch && strcmp (argv[0], "asm") == 0);
  if (!arch && !assembler && strcmp (argv[0], "tune"))
    return NULL;

  if (! assembler)
    {
#if defined (_AIX)
      cache = detect_caches_aix ();
#elif defined (__APPLE__)
      cache = detect_caches_darwin ();
#elif defined (__FreeBSD__)
      cache = detect_caches_freebsd ();
      /* FreeBSD PPC does not provide any cache information yet.  */
      cache = "";
#elif defined (__linux__)
      cache = detect_caches_linux ();
      /* PPC Linux does not provide any cache information yet.  */
      cache = "";
#else
      cache = "";
#endif
    }

#if defined (_AIX)
  cpu = detect_processor_aix ();
#elif defined (__APPLE__)
  cpu = detect_processor_darwin ();
#elif defined (__FreeBSD__)
  cpu = detect_processor_freebsd ();
#elif defined (__linux__)
  cpu = detect_processor_linux ();
#else
  cpu = "powerpc";
#endif

  if (assembler)
    {
      for (i = 0; i < ARRAY_SIZE (asm_names); i++)
	{
	  if (!asm_names[i].cpu || !strcmp (asm_names[i].cpu, cpu))
	    return asm_names[i].asm_sw;
	}

      return NULL;
    }

  return concat (cache, "-m", argv[0], "=", cpu, " ", options, NULL);
}

#else /* GCC_VERSION */

/* If we aren't compiling with GCC we just provide a minimal
   default value.  */
const char *
host_detect_local_cpu (int argc, const char **argv)
{
  const char *cpu;
  bool arch;

  if (argc < 1)
    return NULL;

  arch = strcmp (argv[0], "cpu") == 0;
  if (!arch && strcmp (argv[0], "tune"))
    return NULL;

  if (arch)
    cpu = "powerpc";

  return concat ("-m", argv[0], "=", cpu, NULL);
}

#endif /* GCC_VERSION */

