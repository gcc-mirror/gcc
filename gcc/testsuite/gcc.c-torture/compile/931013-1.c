/* { dg-additional-options "-std=gnu89" } */

g ();

f ()
{
  long ldata[2];
  int seed;

  seed = (ldata[0]) + (ldata[1] << 16);
  g (seed);
}
