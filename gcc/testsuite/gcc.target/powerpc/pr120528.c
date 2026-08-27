/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

#include <stddef.h>

#ifndef TYPE
#define TYPE unsigned long long
#endif

void
mem_to_vsx (TYPE *p, __uint128_t *q)
{
  /* lxvrdx 0,0,3
     stxv 0,0(4)  */

  __uint128_t x = *p;
  __asm__ (" # %x0" : "+wa" (x));
  *q = x;
}

void
memx_to_vsx (TYPE *p, size_t n, __uint128_t *q)
{
  /* sldi 4,4,3
     lxvrdx 0,3,4
     stxv 0,0(4)  */

  __uint128_t x = p[n];
  __asm__ (" # %x0" : "+wa" (x));
  *q = x;
}

void
mem3_to_vsx (TYPE *p, __uint128_t *q)
{
  /* addi 2,3,24
     lxvrdx 0,0,2
     stxv 0,0(4)  */

  __uint128_t x = p[3];
  __asm__ (" # %x0" : "+wa" (x));
  *q = x;
}

void
mem_to_gpr (TYPE *p, __uint128_t *q)
{
  /* ld 2,0(3)
     li 3,0
     std 2,0(4)
     std 3,8(8)  */

  __uint128_t x = *p;
  __asm__ (" # %0" : "+r" (x));
  *q = x;
}

void
memx_to_gpr (TYPE *p, size_t n, __uint128_t *q)
{
  /* sldi 4,4,3
     ldx 2,3,4
     li 3,0
     std 2,0(4)
     std 3,8(8)  */

  __uint128_t x = p[n];
  __asm__ (" # %0" : "+r" (x));
  *q = x;
}

void
mem3_to_gpr (TYPE *p, __uint128_t *q)
{
  /* ld 2,24(3)
     li 3,0
     std 2,0(4)
     std 3,8(8)  */

  __uint128_t x = p[3];
  __asm__ (" # %0" : "+r" (x));
  *q = x;
}

/* { dg-final { scan-assembler-times {\maddi\M}   1 } } */
/* { dg-final { scan-assembler-times {\mli\M}     3 } } */
/* { dg-final { scan-assembler-times {\mlxvrdx\M} 3 } } */
/* { dg-final { scan-assembler-times {\mstxv\M}   3 } } */
