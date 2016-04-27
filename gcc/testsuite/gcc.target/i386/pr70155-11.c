/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2 -mtune=core2 -dp" } */

extern __int128 a;

struct foo
{
  __int128 i;
}__attribute__ ((packed));

extern struct foo x;

void
foo (void)
{
  x.i = a;
}

/* { dg-final { scan-assembler-not "movv1ti_internal" } } */
