// Test EH when V2SI SIMD registers are involved.
// Contributed by Aldy Hernandez (aldy@quesejoda.com).
// { dg-options "-O -Wno-abi" }
// { dg-options "-O -w" { target { { i?86-*-* x86_64-*-* } && ilp32 } } }
// { dg-do run }

typedef int __attribute__((vector_size (8))) vecint;

vecint vecfunc (vecint beachbum)
{
  return beachbum;
}

void f3 (void)
{
  /* Force a use of a V2SI register if available.  On the PPC/E500,
     this will cause the compiler to save the registers in this
     function in 64-bits.  */
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
  f1 ();
  return 0;
}
