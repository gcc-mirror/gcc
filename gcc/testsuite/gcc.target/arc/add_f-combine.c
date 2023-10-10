/* { dg-do compile } */
/* { dg-options "-O1" } */

/* Check if combiner is matching add.f patterns.  */

int a1 (int a, int b)
{
  if (a + b)
    {
      return 1;
    }
  return a + 2;
}

/* { dg-final { scan-assembler "add.f\\s+0,r\\d+,r\\d+" } } */
