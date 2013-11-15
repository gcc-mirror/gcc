/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int *p;
extern int stuff();

template <int value>
void foobar(int a)
{
#pragma simd
  for (int i=0; i < a; ++i)
    p[i] = value;
}

template <int value>
void foobar2(int a)
{
  int j = 123;
#pragma simd linear(j : value)
  for (int i=0; i < a; ++i)
    {
      p[i] = value;
      j += stuff();
    }
}

void funky()
{
  foobar <69> (1000);
  foobar2 <123> (2000);
}

void foobar3(int a)
{
  int j = 123;
#pragma simd linear(j : a + a) /* { dg-error "step size must be an integer" } */
  for (int i=0; i < a; ++i)
    {
    p[i] = 1234;
    extern int bar();
    j += bar();
    }
}
