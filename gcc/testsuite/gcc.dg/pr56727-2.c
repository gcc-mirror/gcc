/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -fPIC" } */
/* { dg-require-alias "" } */
/* { dg-final { scan-assembler "@(PLT|plt)" { target i?86-*-* x86_64-*-* powerpc*-*-linux* } } } */

__attribute__((noinline, noclone))
void f (short b)
{
  f (0);
}

static void g (short) __attribute__ ((alias ("f")));

void h ()
{
  g (0);
}
