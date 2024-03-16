/* { dg-additional-options "-std=gnu89" } */

foo (a)
{
  int i;

  for (i = 1;  i < a; i++)
    ;
  {
    int b = (int) &foo;

    return (a & b) == 0;
  }
}
