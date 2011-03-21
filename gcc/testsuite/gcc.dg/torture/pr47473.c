/* PR c/47473 */
/* { dg-do run } */
/* { dg-options "-std=c99" } */

int
main (void)
{
  long double _Complex w = 0.2L - 0.3iL;
  w = w * (0.3L - (0.0F + 1.0iF) * 0.9L);
  if (__builtin_fabsl (__real__ w + 0.21L) > 0.001L
      || __builtin_fabsl (__imag__ w + 0.27L) > 0.001L)
    __builtin_abort ();
  return 0;
}
