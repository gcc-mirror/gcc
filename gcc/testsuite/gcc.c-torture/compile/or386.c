/* { dg-additional-options "-std=gnu89" } */

typedef int xtype;

xtype
foo (a)
     xtype a;
{
  return a | 0x7f;
}

main ()
{
  printf ("%08x\n", foo (-1));
}
