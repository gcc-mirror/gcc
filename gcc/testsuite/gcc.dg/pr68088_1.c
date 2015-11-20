/* { dg-do compile } */
/* { dg-options "-O2" } */

void bar (unsigned long);

void
foo (unsigned long aul, unsigned m, unsigned i)
{
  while (1)
    {
      aul += i;
      i = aul % m;
      bar (aul);
    }
}
