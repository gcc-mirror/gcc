/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-O2 -fpic -mexplicit-relocs -mcpu=sifive-p450" } */

static void *p;
extern void *a[];
void
baz (void)
{
  p = 0;
}

void bar (void);
void
foo (int i)
{
  bar ();
  a[i] = p;
}


double *d;
void
foobar (int i)
{
  for (; i; ++i)
    d[i] = 1;
}
