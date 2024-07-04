/* { dg-additional-options "-std=gnu89" } */

typedef char type;

type
foo (b)
{
  type a;
  for (a = 10; a < b; a++)
    ;
}
