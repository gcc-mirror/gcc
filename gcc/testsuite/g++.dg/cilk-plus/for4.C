/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int *p;
extern int stuff();

template <int value>
void foobar(int a)
{
  int j = 123;
#pragma simd linear(j : value + 1)
  for (int i=0; i < a; ++i)
    {
      p[i] = value;
      j += stuff();
    }
}

void funky()
{
  foobar <69> (1000);
}
