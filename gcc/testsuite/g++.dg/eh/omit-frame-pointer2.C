// Reduced from PR c++/5246, PR c++/2447
// { dg-options "-O -fomit-frame-pointer" }
// { dg-do run }

void step (int)
{
  void *sp = __builtin_alloca (0);
}

void f2 (void)
{
  step (2);
  throw int();
}

void f1 (void)
{
  try
    {
      step (1);
      f2 ();
      step (-1);
    }
  catch (int)
    {
      step (3);
    }
}

int main ()
{
  f1 ();
  return 0;
}
