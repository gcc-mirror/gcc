// Test EH when V4SI SIMD registers are involved.
// Contributed by Aldy Hernandez (aldy@quesejoda.com).
// { dg-options "-O" }
// { dg-do run }

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
  f1 ();
  return 0;
}
