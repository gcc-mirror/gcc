// Test EH when V4SI SIMD registers are involved.
// Contributed by Aldy Hernandez (aldy@quesejoda.com).
// { dg-options "-O" }
// { dg-options "-O -w" { target i?86-*-* } }
// { dg-options "-O -maltivec" { target powerpc64-*-linux* } }
// { dg-do run }
// { dg-error "" "PR target/12916" { target sparc64-*-* sparcv9-*-* } 23 }
// { dg-error "" "PR target/12916" { target sparc-*-* } 25 }
#ifdef __powerpc64__
#include <signal.h>
extern void
exit (int);

void 
sig_ill_handler (int sig)
{
    exit(0);
}
#endif
typedef int __attribute__((mode(V4SI))) vecint;

vecint vecfunc (vecint beachbum)
{
  return beachbum;
}

void f3 (void)
{
  vecint foobar = (vecint) {0, 0};
  foobar = vecfunc (foobar);

  throw int();
}

void f2 (void)
{
  vecint foobar = (vecint) {0, 0};
  foobar = vecfunc (foobar);

  f3 ();
}

void f1 (void)
{
  int i;
  try
    {
      f2 ();
    }
  catch (int)
    {
      i = 9;
    }
}

int main ()
{
#ifdef __powerpc64__
  /* Exit on systems without altivec.  */
  signal (SIGILL, sig_ill_handler);
  asm volatile (".long 0x10000484");  
  signal (SIGILL, SIG_DFL);
#endif
  f1 ();
  return 0;
}
