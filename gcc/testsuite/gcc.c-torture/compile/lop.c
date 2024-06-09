/* { dg-additional-options "-std=gnu89" } */

lop (a)
{
  do
    a--;
  while (a >= -1);
}
