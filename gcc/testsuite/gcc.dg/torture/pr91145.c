/* { dg-do compile } */
/* { dg-additional-options "-march=skylake-avx512" { target x86_64-*-* i?86-*-* } } */

int a, c;
unsigned b, e;
extern unsigned d[100];

void f()
{
  for (int g = 0; g < 70; g++)
    {

      b += d[g] - c;
      e -= g ^ a;
    }
}
