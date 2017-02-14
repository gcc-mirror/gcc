/* PR middle-end/78416 */
/* { dg-do run { target int128 } } */

int
main ()
{
  unsigned __int128 x;
  x = 0xFFFFFFFFFFFFFFFFULL;
  x /= ~0x7FFFFFFFFFFFFFFFLL;
  if (x != 0)
    __builtin_abort ();
  x = ~0x7FFFFFFFFFFFFFFELL;
  x /= ~0x7FFFFFFFFFFFFFFFLL;
  if (x != 1)
    __builtin_abort ();
  return 0;
}
