/* { dg-additional-options "-std=gnu89" } */

double g ();

f (x)
     double x;
{
  x = .85;
  while (g () < x)
    ;
}
