/* { dg-options "-O3" } */

unsigned int a, b;

void
foo (void)
{
  for (b = 0; b < 13; b += 2)
    a &= !!b;
}
