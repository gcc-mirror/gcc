// { dg-do compile }
// { dg-additional-options "-O3" }
// { dg-additional-options "-march=skylake-avx512" { target x86_64-*-* i?86-*-* } }

int a, b, c, f;
void g(bool h, int d[][5])
{
  for (short i = f; i; i += 1)
    {
      a = h && d[0][i];
      for (int j = 0; j < 4; j += c)
	b = 0;
    }
}
