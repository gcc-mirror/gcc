/* { dg-do compile } */
/* { dg-options "-O -mavx" } */

typedef int v8si __attribute__ ((vector_size (32)));
typedef unsigned long long int u64 __attribute__ ((aligned(64)));


void
#ifndef __x86_64__
__attribute__((regparm(3)))
#endif
foo (u64 *idx, v8si *out_start, v8si *regions)
{
  if (*idx < 20 ) {
    v8si base = regions[*idx];
    *out_start = base;
  }
}

/* Verify no dynamic realignment is performed.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*sp" } } */
