/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -fpeel-loops" } */

unsigned __int128 m;
int n;

__attribute__ ((simd)) void
foo (int x)
{
  x = n ? n : (short int) x;
  if (x)
    m /= 2;
}

