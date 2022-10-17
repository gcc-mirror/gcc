// { dg-do compile }
// { dg-additional-options "-O3" }
// { dg-additional-options "-march=skylake-avx512" { target x86_64-*-* i?86-*-* } }

int a;
extern bool b[][14];
char h;
void f(short g[][14])
{
  for (short d = h; d < 21; d += 1)
    for (unsigned char e = 0; e < 14; e += 1)
      {
	a = 0;
	b[d][e] = g[d][e];
      }
}
