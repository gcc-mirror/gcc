// Test EH when V4SI SIMD registers are involved.
// Contributed by Aldy Hernandez (aldy@quesejoda.com).
// { dg-options "-O" }
// { dg-options "-O -w" { target i?86-*-* } }
// { dg-options "-O -w -maltivec" { target powerpc64-*-linux* } }
// { dg-do run { xfail "powerpc64-*-linux*"}  }
// { dg-error "" "PR target/12916" { target sparc*-*-* } 0 }

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
#if defined(__powerpc64__) && defined(__linux__)
  // Don't run on ppc64-linux, since not always AltiVec regs available   
  return -1;  
#endif
  f1 ();
  return 0;
}
