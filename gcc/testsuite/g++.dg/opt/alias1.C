// Test that type punning using an anonymous union works with strict aliasing.
// { dg-do run { xfail *-*-* } }
// { dg-options "-O2 -fstrict-aliasing" }

extern "C" void abort ();

void f (int i)
{
  union
  {
    int ui;
    float uf[2];
  };

  ui = i;
  if (uf[0] != 42.0)
    abort ();
}

int main ()
{
  union U { int i; float f[2]; } u;
  u.f[0] = 42.0;
  f (u.i);
}
