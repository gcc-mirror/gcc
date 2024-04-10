/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -fno-dce -fno-forward-propagate -fno-split-wide-types -funroll-loops" } */
int i;
__attribute__((__vector_size__(64))) __int128 v;

void
foo(void)
{
  v <<= 127;
  __builtin_mul_overflow(0, i, &v[3]);
  v *= 6;
}
