/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2" } */

typedef __attribute__((__vector_size__(16))) __int128 V;

union {
  _Complex long c;
  V v;
} u;

__int128 j;
int i;

void
foo()
{
  u.v &= 4;
  i %= u.c ? 3 : j;
}
