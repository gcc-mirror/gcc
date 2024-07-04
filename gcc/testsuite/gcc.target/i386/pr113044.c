/* PR target/113044 */
/* { dg-do run } */
/* { dg-options "-O" } */

typedef unsigned char __attribute__((__vector_size__ (2))) V;

V
foo (char c, V v)
{
  V x = v >> (v & 8);
  volatile char d = c;
  if (!d)
    __builtin_abort();
  return x;
}

int
main (void)
{
  V x = foo (10, (V){3});
  if (x[0] != 3 || x[1])
    __builtin_abort();
  return 0;
}
