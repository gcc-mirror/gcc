/* { dg-do run } */
/* { dg-options "-O -mno-sse2" } */

typedef unsigned char __attribute__ ((__vector_size__ (8))) V;
typedef unsigned long long __attribute__ ((__vector_size__ (16))) W;

V c;
W d, e;

V
foo (W f)
{
  W g = (W) { 0, 209 } <7 <= (0 < f);
  W h = e + g + d;
  V j = (V) (h[0]) + (V) c;
  return j;
}

int
main (void)
{
  V x = foo ((W) { 3 });
  for (unsigned i = 0; i < sizeof (x); i++)
    if (x[i] != 0xff)
      __builtin_abort ();
  return 0;
}
