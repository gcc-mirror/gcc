/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mpower9-vector -mpower9-misc -O2" } */

/* Verify P9 atomic memory operations.  */

#include <amo.h>
#include <stdint.h>

uint32_t
do_lw_add (uint32_t *mem, uint32_t value)
{
  return amo_lwat_add (mem, value);
}

int32_t
do_lw_sadd (int32_t *mem, int32_t value)
{
  return amo_lwat_sadd (mem, value);
}

uint32_t
do_lw_xor (uint32_t *mem, uint32_t value)
{
  return amo_lwat_xor (mem, value);
}

uint32_t
do_lw_ior (uint32_t *mem, uint32_t value)
{
  return amo_lwat_ior (mem, value);
}

uint32_t
do_lw_and (uint32_t *mem, uint32_t value)
{
  return amo_lwat_and (mem, value);
}

uint32_t
do_lw_umax (uint32_t *mem, uint32_t value)
{
  return amo_lwat_umax (mem, value);
}

int32_t
do_lw_smax (int32_t *mem, int32_t value)
{
  return amo_lwat_smax (mem, value);
}

uint32_t
do_lw_umin (uint32_t *mem, uint32_t value)
{
  return amo_lwat_umin (mem, value);
}

int32_t
do_lw_smin (int32_t *mem, int32_t value)
{
  return amo_lwat_smin (mem, value);
}

uint32_t
do_lw_swap (uint32_t *mem, uint32_t value)
{
  return amo_lwat_swap (mem, value);
}

int32_t
do_lw_sswap (int32_t *mem, int32_t value)
{
  return amo_lwat_sswap (mem, value);
}

uint64_t
do_ld_add (uint64_t *mem, uint64_t value)
{
  return amo_ldat_add (mem, value);
}

int64_t
do_ld_sadd (int64_t *mem, int64_t value)
{
  return amo_ldat_sadd (mem, value);
}

uint64_t
do_ld_xor (uint64_t *mem, uint64_t value)
{
  return amo_ldat_xor (mem, value);
}

uint64_t
do_ld_ior (uint64_t *mem, uint64_t value)
{
  return amo_ldat_ior (mem, value);
}

uint64_t
do_ld_and (uint64_t *mem, uint64_t value)
{
  return amo_ldat_and (mem, value);
}

uint64_t
do_ld_umax (uint64_t *mem, uint64_t value)
{
  return amo_ldat_umax (mem, value);
}

int64_t
do_ld_smax (int64_t *mem, int64_t value)
{
  return amo_ldat_smax (mem, value);
}

uint64_t
do_ld_umin (uint64_t *mem, uint64_t value)
{
  return amo_ldat_umin (mem, value);
}

int64_t
do_ld_smin (int64_t *mem, int64_t value)
{
  return amo_ldat_smin (mem, value);
}

uint64_t
do_ld_swap (uint64_t *mem, uint64_t value)
{
  return amo_ldat_swap (mem, value);
}

int64_t
do_ld_sswap (int64_t *mem, int64_t value)
{
  return amo_ldat_sswap (mem, value);
}

void
do_sw_add (uint32_t *mem, uint32_t value)
{
  amo_stwat_add (mem, value);
}

void
do_sw_sadd (int32_t *mem, int32_t value)
{
  amo_stwat_sadd (mem, value);
}

void
do_sw_xor (uint32_t *mem, uint32_t value)
{
  amo_stwat_xor (mem, value);
}

void
do_sw_ior (uint32_t *mem, uint32_t value)
{
  amo_stwat_ior (mem, value);
}

void
do_sw_and (uint32_t *mem, uint32_t value)
{
  amo_stwat_and (mem, value);
}

void
do_sw_umax (int32_t *mem, int32_t value)
{
  amo_stwat_umax (mem, value);
}

void
do_sw_smax (int32_t *mem, int32_t value)
{
  amo_stwat_smax (mem, value);
}

void
do_sw_umin (int32_t *mem, int32_t value)
{
  amo_stwat_umin (mem, value);
}

void
do_sw_smin (int32_t *mem, int32_t value)
{
  amo_stwat_smin (mem, value);
}

void
do_sd_add (uint64_t *mem, uint64_t value)
{
  amo_stdat_add (mem, value);
}

void
do_sd_sadd (int64_t *mem, int64_t value)
{
  amo_stdat_sadd (mem, value);
}

void
do_sd_xor (uint64_t *mem, uint64_t value)
{
  amo_stdat_xor (mem, value);
}

void
do_sd_ior (uint64_t *mem, uint64_t value)
{
  amo_stdat_ior (mem, value);
}

void
do_sd_and (uint64_t *mem, uint64_t value)
{
  amo_stdat_and (mem, value);
}

void
do_sd_umax (int64_t *mem, int64_t value)
{
  amo_stdat_umax (mem, value);
}

void
do_sd_smax (int64_t *mem, int64_t value)
{
  amo_stdat_smax (mem, value);
}

void
do_sd_umin (int64_t *mem, int64_t value)
{
  amo_stdat_umin (mem, value);
}

void
do_sd_smin (int64_t *mem, int64_t value)
{
  amo_stdat_smin (mem, value);
}

/* { dg-final { scan-assembler-times {\mldat\M}  11 } } */
/* { dg-final { scan-assembler-times {\mlwat\M}  11 } } */
/* { dg-final { scan-assembler-times {\mstdat\M}  9 } } */
/* { dg-final { scan-assembler-times {\mstwat\M}  9 } } */
