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

#ifdef __linux__
#include "common/config/riscv/riscv-hwprobe.h"

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

  if (ima0 & (RISCV_HWPROBE_IMA_V
	      | RISCV_HWPROBE_EXT_ZVE32X
	      | RISCV_HWPROBE_EXT_ZVE64X))
    *vector_p = true;

  return true;
}

/* Return the ISA string describing this machine, or NULL if it cannot be
   determined.  */

static const char *
riscv_native_arch (void)
{
  std::string isa;
  bool vector_p = false;

  if (!riscv_arch_from_hwprobe (isa, &vector_p))
    return NULL;

  if (vector_p)
    riscv_add_vlen (isa);

  return xstrdup (isa.c_str ());
}

#else /* !__linux__ */

static const char *
riscv_native_arch (void)
{
  return NULL;
}

#endif /* __linux__ */

/* Implement the local_cpu_detect spec function.  ARGV[0] selects what to
   report: "arch" for an -march= option.  */

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

  return NULL;
}
