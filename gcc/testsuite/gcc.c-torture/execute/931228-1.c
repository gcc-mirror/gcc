/* { dg-additional-options "-std=gnu89" } */
f (x)
{
  x &= 010000;
  x &= 007777;
  x ^= 017777;
  x &= 017770;
  return x;
}

main ()
{
  if (f (-1) != 017770)
    abort ();
  exit (0);
}
