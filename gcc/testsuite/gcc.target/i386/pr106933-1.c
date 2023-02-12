/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

short int
bar (void);

__int128
empty (void)
{
}

__attribute__ ((simd)) int
foo (__int128 *p)
{
  int a = 0x80000000;

  *p = empty ();

  return *p == (a < bar ());
}

