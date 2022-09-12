#include <cassert>

struct S {
  int x[10];
};

S *
choose (S *a, S *b, int c)
{
  if (c < 5)
    return a;
  else
    return b; 
}

int main (int argc, char *argv[])
{
  S a, b;

  for (int i = 0; i < 10; i++)
    a.x[i] = b.x[i] = 0;

  for (int i = 0; i < 10; i++)
    {
#pragma omp target map(choose(&a, &b, i)->x[:10])
/* { dg-message {sorry, unimplemented: unsupported map expression 'choose\(\(& a\), \(& b\), i\)->S::x\[0\]'} "" { target *-*-* } .-1 } */
      for (int j = 0; j < 10; j++)
        choose (&a, &b, i)->x[j]++;
    }

  for (int i = 0; i < 10; i++)
    assert (a.x[i] == 5 && b.x[i] == 5);

  return 0;
}


