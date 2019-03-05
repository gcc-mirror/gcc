/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O3 -mdejagnu-cpu=power7 -funroll-loops -ffast-math -mupdate -fno-auto-inc-dec" } */

/* PR 77289: LRA ICEs due to invalid constraint checking.  */

void dummy0 (float *);
float bar0 (float);
void
foo0 (long a, long b)
{
  float c[0];
  b = 0;
  for (; b < a; b++)
    c[b] = bar0 (c[b]);
  dummy0 (c);
}

void dummy1 (long *);
long bar1 (long);
void
foo1 (long a, long b)
{
  long array[128];
  long *c = array;
  for (b=0; b < a; b++)
    c[b] = bar1 (c[b]);
  dummy1 (c);
}
