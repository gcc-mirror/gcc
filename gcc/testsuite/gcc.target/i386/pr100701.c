/* PR target/100701 */
/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O0 -fschedule-insns2 -msse2" } */

typedef unsigned char __attribute__((__vector_size__ (8))) V;
typedef unsigned int __attribute__((__vector_size__ (8))) U;

U u;
unsigned x;
unsigned char y;

V
foo (V a, __int128 i)
{
  V b = a;
  a &= y;
  if (i == 0)
    __builtin_abort ();
  U c = (x != y / i) <= u;
  return (V) c + a + b;
}

int
main (void)
{
  (void)foo ((V) { }, 4);
  return 0;
}
