/* { dg-do run } */
/* { dg-options "-O2" } */

extern "C" void abort (void);

int j;

void foo (bool x, bool y)
{
  if (!x)
    j = 0;
  if (!x == y)
    j = 1;
}

int main (void)
{
  foo (1, 1);
  if (j)
    abort ();
  return 0;
}
