// Test that type punning using an anonymous union works with strict aliasing.
// { dg-do run }
// { dg-options "-O2 -fstrict-aliasing" }

extern "C" void abort ();

void f (long i)
{
  union
  {
    long ui;
    float uf[20];
  };

  ui = i;
  if (uf[0] != 42.0)
    abort ();
}

int main ()
{
  union U { long i; float f[20]; } u;
  u.f[0] = 42.0;
  f (u.i);
}
