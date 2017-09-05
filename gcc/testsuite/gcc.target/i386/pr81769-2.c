/* { dg-do compile } */
/* { dg-options "-O -mavx -fno-omit-frame-pointer" } */

typedef unsigned long long int u64 __attribute__ ((aligned(64)));

void
#ifndef __x86_64__
__attribute__((regparm(3)))
#endif
foo (u64 *idx, unsigned int *out_start, unsigned int *out_end,
     unsigned int *regions)
{
  if (*idx < 20 ) {
    unsigned int base = regions[*idx];
    *out_start = base;
    *out_end = base;
  }
}

/* Verify no dynamic realignment is performed.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*sp" } } */
