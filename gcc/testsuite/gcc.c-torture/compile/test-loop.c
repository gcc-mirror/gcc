/* { dg-additional-options "-std=gnu89" } */

main ()
{
  int i;
  for (i = 100;  i >= -1; i--)
    foo ();

}
