/* Native CPU detection for RISC-V.
   Copyright (C) 2026 Free Software Foundation, Inc.

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
#define INCLUDE_STRING
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "riscv-subset.h"

#ifdef __linux__
#include <sched.h>
#include "common/config/riscv/riscv-hwprobe.h"

/* Read for UNKNOWN_ID alone; the core table itself is built further down.  */
#include "riscv-cores.def"

#ifndef __NR_getcpu
#define __NR_getcpu 168
#endif

/* One entry per extension the kernel can report.  */

struct riscv_hwprobe_ext
{
  const char *name;
  int key;
  int bit;
  int xlen;
};

static const struct riscv_hwprobe_ext riscv_hwprobe_exts[] = {
#define RISCV_HWPROBE_EXT(NAME, UPPERCASE_NAME, KEY, BIT, XLEN)		\
  { NAME, KEY, BIT, XLEN },
#include "common/config/riscv/riscv-hwprobe.def"
};

/* What the mvendorid, marchid and mimpid CSRs read on one core.  */

struct riscv_cpuid
{
  constexpr riscv_cpuid ()
    : mvendorid (UNKNOWN_ID), marchid (UNKNOWN_ID), mimpid (UNKNOWN_ID)
  {
  }

  constexpr riscv_cpuid (riscv_core_id_t vendor, riscv_core_id_t arch,
			 riscv_core_id_t impl)
    : mvendorid (vendor), marchid (arch), mimpid (impl)
  {
  }

  bool valid_p () const;
  bool match (const struct riscv_cpuid &other) const;

  riscv_core_id_t mvendorid;
  riscv_core_id_t marchid;
  riscv_core_id_t mimpid;
};

/* True if all three registers have a value.  */

bool
riscv_cpuid::valid_p () const
{
  return (mvendorid != UNKNOWN_ID
	  && marchid != UNKNOWN_ID
	  && mimpid != UNKNOWN_ID);
}

/* True if OTHER is the same core.  All three registers have to agree, and
   both sides have to have all three: most of riscv-cores.def records none
   of them, and a detection that came back empty must not match those.  */

bool
riscv_cpuid::match (const struct riscv_cpuid &other) const
{
  return (valid_p ()
	  && other.valid_p ()
	  && mvendorid == other.mvendorid
	  && marchid == other.marchid
	  && mimpid == other.mimpid);
}

/* One entry per core, holding what it reports in its identification
   registers.  */

struct riscv_core_id
{
  const char *name;
  struct riscv_cpuid id;
};

static const struct riscv_core_id riscv_core_ids[] = {
#define RISCV_CORE(CORE_NAME, ARCH, MICRO_ARCH, MVENDORID, MARCHID, MIMPID) \
  { CORE_NAME, { (riscv_core_id_t) (MVENDORID),				\
		 (riscv_core_id_t) (MARCHID),				\
		 (riscv_core_id_t) (MIMPID) } },
#include "riscv-cores.def"
};

/* Return the name of the core whose identification registers read ID, or
   NULL if riscv-cores.def describes no such core.  */

static const char *
riscv_core_from_cpuid (const struct riscv_cpuid &id)
{
  for (size_t i = 0; i < ARRAY_SIZE (riscv_core_ids); i++)
    if (riscv_core_ids[i].id.match (id))
      return riscv_core_ids[i].name;

  return NULL;
}

/* Return the number of the CPU this process is currently running on, or -1
   if it cannot be determined.  */

static int
riscv_current_cpu (void)
{
  unsigned int cpu = 0;

  if (syscall_5_args (__NR_getcpu, (long) &cpu, 0, 0, 0, 0) != 0)
    return -1;

  return (int) cpu;
}

/* Ask hwprobe about the CPUs in CPUS.  */

static bool
riscv_hwprobe_cpus (struct riscv_hwprobe *pairs, size_t npairs,
		    const cpu_set_t *cpus)
{
  return syscall_5_args (__NR_riscv_hwprobe, (long) pairs, (long) npairs,
			 (long) sizeof (*cpus), (long) cpus, 0) == 0;
}

/* Ask hwprobe about the CPUs this process may run on rather than about the
   whole machine: on a machine whose cores are not all alike, a query that
   spans cores which disagree is answered with UNKNOWN_ID.

   TODO: Cores that differ but share an ISA could still be described, once
   we have -mcpu=A+B/-mtune=A+B.  */

static bool
riscv_hwprobe_affinity (struct riscv_hwprobe *pairs, size_t npairs)
{
  cpu_set_t cpus;

  if (sched_getaffinity (0, sizeof (cpus), &cpus) != 0)
    return false;

  return riscv_hwprobe_cpus (pairs, npairs, &cpus);
}

/* Ask hwprobe about the one CPU this process is running on, for when the
   CPUs it may run on do not agree and the wider query therefore answers
   UNKNOWN_ID.  That is the core /proc/cpuinfo describes as well.  */

static bool
riscv_hwprobe_this_core (struct riscv_hwprobe *pairs, size_t npairs)
{
  cpu_set_t cpus;
  int cpu = riscv_current_cpu ();

  if (cpu < 0 || cpu >= CPU_SETSIZE)
    return false;

  CPU_ZERO (&cpus);
  CPU_SET (cpu, &cpus);

  return riscv_hwprobe_cpus (pairs, npairs, &cpus);
}

/* Append the Zvl extension for this machine's vector register width,
   which only the vlenb CSR reports.  */

static void
riscv_add_vlen (std::string &isa)
{
  unsigned long vlenb;
  unsigned long vlen;
  char buf[32];

  __asm__ volatile ("csrr %0, 0xc22" : "=r" (vlenb));

  vlen = vlenb * 8;

  gcc_assert (vlen >= 32 && vlen <= 65536 && (vlen & (vlen - 1)) == 0);

  snprintf (buf, sizeof (buf), "_zvl%lub", vlen);
  isa += buf;
}

/* Build the ISA string from what hwprobe reports, and set *VECTOR_P if the
   machine can execute vector instructions.  Return false if hwprobe cannot
   describe the machine.  */

static bool
riscv_arch_from_hwprobe (std::string &isa, bool *vector_p)
{
  struct riscv_hwprobe pairs[] = {
    { RISCV_HWPROBE_KEY_IMA_EXT_0, 0 },
    { RISCV_HWPROBE_KEY_IMA_EXT_1, 0 }
  };
  size_t npairs = ARRAY_SIZE (pairs);
  unsigned long long ima0;

  if (riscv_hwprobe (pairs, npairs) != 0)
    return false;

  const int xlen = __riscv_xlen;

  isa = xlen == 32 ? "rv32ima" : "rv64ima";

  for (size_t i = 0; i < ARRAY_SIZE (riscv_hwprobe_exts); i++)
    {
      const struct riscv_hwprobe_ext *ext = &riscv_hwprobe_exts[i];
      unsigned long long value;

      if (ext->xlen != 0 && ext->xlen != xlen)
	continue;

      value = riscv_hwprobe_value (pairs, npairs, ext->key);
      if (value & (1ULL << ext->bit))
	{
	  isa += '_';
	  isa += ext->name;
	}
    }

  ima0 = riscv_hwprobe_value (pairs, npairs, RISCV_HWPROBE_KEY_IMA_EXT_0);

  if (ima0 & (RISCV_HWPROBE_EXT_V
	      | RISCV_HWPROBE_EXT_ZVE32X
	      | RISCV_HWPROBE_EXT_ZVE64X))
    *vector_p = true;

  return true;
}

/* What the /proc/cpuinfo fallback managed to read out of one processor
   block.  */

struct riscv_cpuinfo
{
  std::string isa;
  struct riscv_cpuid id;
};

/* GCC_CPUINFO names a file to read instead of /proc/cpuinfo, and
   GCC_CPUINFO_CPU the processor to look for in it.  Both are for
   testing.  */

static const char *
riscv_cpuinfo_file (void)
{
  const char *path = getenv ("GCC_CPUINFO");

  return path != NULL ? path : "/proc/cpuinfo";
}

static bool
riscv_cpuinfo_forced (void)
{
  return getenv ("GCC_CPUINFO") != NULL;
}

static int
riscv_cpuinfo_cpu (void)
{
  const char *forced = getenv ("GCC_CPUINFO_CPU");

  return forced != NULL ? atoi (forced) : riscv_current_cpu ();
}

/* Split LINE, which /proc/cpuinfo writes as "name\t: value", putting the
   name in NAME and returning the value.  Both are stripped of surrounding
   whitespace.  Return NULL if LINE is not of that shape.  */

static const char *
riscv_cpuinfo_field (char *line, std::string &name)
{
  char *colon = strchr (line, ':');
  char *end;

  if (colon == NULL)
    return NULL;

  end = colon;
  while (end > line && ISSPACE (end[-1]))
    end--;
  name.assign (line, end - line);

  colon++;
  while (ISSPACE (*colon))
    colon++;

  end = colon + strlen (colon);
  while (end > colon && ISSPACE (end[-1]))
    end--;
  *end = '\0';

  return colon;
}

/* Read the /proc/cpuinfo block describing processor WANT into OUT, or the
   first block in the file if WANT is negative.  The processor field holds
   the number sched_getcpu would return, not the hart id.  Return false if
   there is no such block, or if it says nothing about the ISA.  */

static bool
riscv_read_cpuinfo (int want, struct riscv_cpuinfo *out)
{
  FILE *f = fopen (riscv_cpuinfo_file (), "r");
  char line[1024];
  bool in_block = false;
  bool hart_isa_p = false;

  if (f == NULL)
    return false;

  out->isa.clear ();
  out->id = riscv_cpuid ();

  while (fgets (line, sizeof (line), f) != NULL)
    {
      std::string name;
      const char *value = riscv_cpuinfo_field (line, name);

      if (value == NULL)
	continue;

      if (name == "processor")
	{
	  if (in_block)
	    break;
	  in_block = want < 0 || atoi (value) == want;
	  continue;
	}

      if (!in_block)
	continue;

      /* "hart isa" describes this core alone and is the one to go by, but
	 the kernels this fallback exists for are older than that line and
	 offer only the machine-wide "isa".  */
      if (name == "hart isa")
	{
	  out->isa = value;
	  hart_isa_p = true;
	}
      else if (name == "isa" && !hart_isa_p)
	out->isa = value;
      else if (name == "mvendorid")
	out->id.mvendorid = strtoull (value, NULL, 0);
      else if (name == "marchid")
	out->id.marchid = strtoull (value, NULL, 0);
      else if (name == "mimpid")
	out->id.mimpid = strtoull (value, NULL, 0);
    }

  fclose (f);

  return !out->isa.empty ();
}

/* Read the block for the current CPU, or the first one if it has none.  */

static bool
riscv_cpuinfo (struct riscv_cpuinfo *out)
{
  return (riscv_read_cpuinfo (riscv_cpuinfo_cpu (), out)
	  || riscv_read_cpuinfo (-1, out));
}

/* Copy KERNEL_ISA into ISA, dropping every extension this compiler does
   not know: one unrecognised name would make -march= fail outright.  Set
   *VECTOR_P if what is left has vectors.  Return false if KERNEL_ISA is
   not an ISA string at all.  */

static bool
riscv_filter_isa (const char *kernel_isa, std::string &isa, bool *vector_p)
{
  const char *base = __riscv_xlen == 32 ? "rv32" : "rv64";
  const char *p = kernel_isa;

  if (strncmp (p, base, 4) != 0)
    return false;
  p += 4;

  if (*p != 'i' && *p != 'e' && *p != 'g')
    return false;

  isa = base;
  isa += *p++;

  /* The single-letter extensions run together up to the first underscore.  */
  for (; *p != '\0' && *p != '_'; p++)
    {
      const char name[2] = { *p, '\0' };

      if (!riscv_ext_is_known_p (name))
	continue;

      isa += '_';
      isa += *p;
      if (*p == 'v')
	*vector_p = true;
    }

  /* The multi-letter ones are separated by underscores.  */
  while (*p == '_')
    {
      const char *end = ++p;

      while (*end != '\0' && *end != '_')
	end++;

      std::string name (p, end - p);
      p = end;

      if (name.empty () || !riscv_ext_is_known_p (name.c_str ()))
	continue;

      isa += '_';
      isa += name;
      if (name.compare (0, 3, "zve") == 0)
	*vector_p = true;
    }

  return true;
}

/* Build the ISA string from /proc/cpuinfo, for kernels that are too old to
   have hwprobe.  */

static bool
riscv_arch_from_cpuinfo (std::string &isa, bool *vector_p)
{
  struct riscv_cpuinfo info;

  if (!riscv_cpuinfo (&info))
    return false;

  return riscv_filter_isa (info.isa.c_str (), isa, vector_p);
}

/* Return the ISA string describing this machine, or NULL if it cannot be
   determined.  */

static const char *
riscv_native_arch (void)
{
  std::string isa;
  bool vector_p = false;
  bool forced = riscv_cpuinfo_forced ();

  if ((forced || !riscv_arch_from_hwprobe (isa, &vector_p))
      && !riscv_arch_from_cpuinfo (isa, &vector_p))
    return NULL;

  if (vector_p && !forced)
    riscv_add_vlen (isa);

  return xstrdup (isa.c_str ());
}

/* Get mvendorid, marchid, mimpid out of what a hwprobe query wrote into
   PAIRS.  Return false unless every register came back with a value.  */

static bool
riscv_cpuid_from_pairs (const struct riscv_hwprobe *pairs, size_t npairs,
			struct riscv_cpuid &id)
{
  id = riscv_cpuid (riscv_hwprobe_value (pairs, npairs,
					 RISCV_HWPROBE_KEY_MVENDORID),
		    riscv_hwprobe_value (pairs, npairs,
					 RISCV_HWPROBE_KEY_MARCHID),
		    riscv_hwprobe_value (pairs, npairs,
					 RISCV_HWPROBE_KEY_MIMPID));

  return id.valid_p ();
}

/* Get mvendorid, marchid, mimpid by hwprobe.  Return false if the
   kernel cannot describe the core.  */

static bool
riscv_cpuid_from_hwprobe (struct riscv_cpuid &id)
{
  struct riscv_hwprobe pairs[] = {
    { RISCV_HWPROBE_KEY_MVENDORID, 0 },
    { RISCV_HWPROBE_KEY_MARCHID, 0 },
    { RISCV_HWPROBE_KEY_MIMPID, 0 }
  };
  size_t npairs = ARRAY_SIZE (pairs);

  if (riscv_hwprobe_affinity (pairs, npairs)
      && riscv_cpuid_from_pairs (pairs, npairs, id))
    return true;

  return (riscv_hwprobe_this_core (pairs, npairs)
	  && riscv_cpuid_from_pairs (pairs, npairs, id));
}

/* Get mvendorid, marchid, mimpid by /proc/cpuinfo.  Return false if
   it does not name all three.  */

static bool
riscv_cpuid_from_cpuinfo (struct riscv_cpuid &id)
{
  struct riscv_cpuinfo info;

  if (!riscv_cpuinfo (&info))
    return false;

  id = info.id;

  return id.valid_p ();
}

/* Return the name of the core this machine runs, or NULL if it is not one
   riscv-cores.def has identification registers for.  */

static const char *
riscv_native_tune (void)
{
  struct riscv_cpuid id;
  const char *core;

  if ((riscv_cpuinfo_forced () || !riscv_cpuid_from_hwprobe (id))
      && !riscv_cpuid_from_cpuinfo (id))
    return NULL;

  core = riscv_core_from_cpuid (id);

  return core != NULL ? xstrdup (core) : NULL;
}

#else /* !__linux__ */

static const char *
riscv_native_arch (void)
{
  return NULL;
}

static const char *
riscv_native_tune (void)
{
  return NULL;
}

#endif /* __linux__ */

/* Implement the local_cpu_detect spec function.  ARGV[0] selects what to
   report: "arch" for an -march= option, "tune" for an -mtune= one.  */

const char *
host_detect_local_cpu (int argc, const char **argv)
{
  if (argc < 1 || argv[0] == NULL)
    return NULL;

  if (strcmp (argv[0], "arch") == 0)
    {
      const char *isa = riscv_native_arch ();

      if (isa == NULL)
	return NULL;

      return concat ("-march=", isa, NULL);
    }

  if (strcmp (argv[0], "tune") == 0)
    {
      const char *core = riscv_native_tune ();

      if (core == NULL)
	return NULL;

      return concat ("-mtune=", core, NULL);
    }

  return NULL;
}
