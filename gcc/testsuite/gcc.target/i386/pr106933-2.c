/* { dg-do compile { target int128 } } */
/* { dg-options "-msse4 -Os" } */

__int128 n;

__int128
empty (void)
{
}

int
foo (void)
{
  n = empty ();

  return n == 0;
}
