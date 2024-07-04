/* { dg-do run { target { powerpc*-*-linux* && { lp64 && p9vector_hw } } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx -mpower9-misc" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */

#include <amo.h>
#include <stdint.h>
#include <stdlib.h>

/* Test whether the ISA 3.0 amo (atomic memory operations) functions perform as
   expected.  */

/* 32-bit tests.  */
static uint32_t u32_ld[4] = {
  9,				/* add */
  7,				/* xor */
  6,				/* ior */
  7,				/* and */
};

static uint32_t u32_st[4] = {
  9,				/* add */
  7,				/* xor */
  6,				/* ior */
  7,				/* and */
};

static uint32_t u32_result[4];

static uint32_t u32_update[4] = {
  9 + 1,			/* add */
  7 ^ 1,			/* xor */
  6 | 1,			/* ior */
  7 & 1,			/* and */
};

static uint32_t u32_prev[4] = {
  9,				/* add */
  7,				/* xor */
  6,				/* ior */
  7,				/* and */
};

/* 64-bit tests.  */
static uint64_t u64_ld[4] = {
  9,				/* add */
  7,				/* xor */
  6,				/* ior */
  7,				/* and */
};

static uint64_t u64_st[4] = {
  9,				/* add */
  7,				/* xor */
  6,				/* ior */
  7,				/* and */
};

static uint64_t u64_result[4];

static uint64_t u64_update[4] = {
  9 + 1,			/* add */
  7 ^ 1,			/* xor */
  6 | 1,			/* ior */
  7 & 1,			/* and */
};

static uint64_t u64_prev[4] = {
  9,				/* add */
  7,				/* xor */
  6,				/* ior */
  7,				/* and */
};

int
main (void)
{
  size_t i;

  u32_result[0] = amo_lwat_add (&u32_ld[0], 1);
  u32_result[1] = amo_lwat_xor (&u32_ld[1], 1);
  u32_result[2] = amo_lwat_ior (&u32_ld[2], 1);
  u32_result[3] = amo_lwat_and (&u32_ld[3], 1);

  u64_result[0] = amo_ldat_add (&u64_ld[0], 1);
  u64_result[1] = amo_ldat_xor (&u64_ld[1], 1);
  u64_result[2] = amo_ldat_ior (&u64_ld[2], 1);
  u64_result[3] = amo_ldat_and (&u64_ld[3], 1);

  amo_stwat_add (&u32_st[0], 1);
  amo_stwat_xor (&u32_st[1], 1);
  amo_stwat_ior (&u32_st[2], 1);
  amo_stwat_and (&u32_st[3], 1);

  amo_stdat_add (&u64_st[0], 1);
  amo_stdat_xor (&u64_st[1], 1);
  amo_stdat_ior (&u64_st[2], 1);
  amo_stdat_and (&u64_st[3], 1);

  for (i = 0; i < 4; i++)
    {
      if (u32_result[i] != u32_prev[i])
	abort ();

      if (u32_ld[i] != u32_update[i])
	abort ();

      if (u32_st[i] != u32_update[i])
	abort ();

      if (u64_result[i] != u64_prev[i])
	abort ();

      if (u64_ld[i] != u64_update[i])
	abort ();

      if (u64_st[i] != u64_update[i])
	abort ();
    }

  return 0;
}
