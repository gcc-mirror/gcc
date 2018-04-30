/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer -mavx" } */

typedef int v8si __attribute__ ((vector_size (32)));

void
#ifndef __x86_64__
__attribute__((regparm(3)))
#endif
foo (v8si *out_start, v8si *out_end, v8si *regions)
{
  v8si base = regions[3];
  *out_start = base;
  *out_end = base;
}

/* No need to use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
/* Verify no dynamic realignment is performed.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*sp" } } */
