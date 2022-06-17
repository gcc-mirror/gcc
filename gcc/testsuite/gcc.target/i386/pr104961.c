/* { dg-do compile { target int128 } } */
/* { dg-options "-Og" } */

__int128 i;

void bar (int);

void
foo (int a, char b, _Complex unsigned char c)
{
  __int128 j = i * i;
  c -= 1;
  bar (j);
  bar (__imag__ c);
}
