/* { dg-do run } */
/* { dg-additional-options "-Wno-pedantic" } */

__int128 one = 1;
__int128 min_one = -1;
__int128 zero = 0;

int
main (void)
{
  if (zero - one != min_one)
    __builtin_abort ();

  return 0;
}
