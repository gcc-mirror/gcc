/* { dg-do compile { target bitint } } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-mavx10.1" { target x86_64-*-* i?86-*-* } } */

union {
  _Complex long a[3];
  _BitInt (64) b[5];
  unsigned _BitInt (512) c;
} u;
int g;

void
bar (int j)
{
  __builtin_add_overflow (1, j, &g);
  u.c /= g;
  int __z;
  __builtin_sub_overflow (0, u.b[4], &__z);
  u.a[2] *= (_Complex double) __z;
}

void
foo ()
{
  bar (1);
}
