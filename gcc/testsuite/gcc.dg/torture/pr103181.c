/* { dg-do run } */

typedef unsigned char __attribute__((__vector_size__ (2))) U;
typedef unsigned short S;
typedef unsigned int __attribute__((__vector_size__ (64))) V;

V v;
U a, b, c;

U
foo (S s)
{
  v += __builtin_bswap16 (s) || (S) (a / ((U){3, 0}));
  return b + c;
}

int
main (void)
{
  U x = foo (4);
  if (x[0] || x[1])
    __builtin_abort ();
  return 0;
}
