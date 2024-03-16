/* { dg-additional-options "-std=gnu89" } */

ase (p)
     short *p;
{
  int a;

  a = *p;
  *p = a + 1;
}
