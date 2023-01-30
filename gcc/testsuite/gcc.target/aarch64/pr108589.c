/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-Os -O2 -mtune=ampere1a -fno-split-wide-types" } */

int i;
__int128 j;
short s;

void
foo (void)
{
  j -= i;
  int l = i - __builtin_sub_overflow_p (0, 61680, s);
  j -= __builtin_mul_overflow_p (i, l, 0);
}
