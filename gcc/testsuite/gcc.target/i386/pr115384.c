/* { dg-do compile { target int128 } } */
/* { dg-options "-O" } */

typedef __attribute__((__vector_size__(sizeof(__int128)))) __int128 W;

W w;

void
foo()
{
  w = w >> 4 & 18446744073709551600llu;
}
