/* { dg-do run } */
/* PR target/101529 */

typedef unsigned char __attribute__((__vector_size__ (1))) W;
typedef unsigned char __attribute__((__vector_size__ (8))) V;
typedef unsigned short __attribute__((__vector_size__ (16))) U;

unsigned short us;

/* aarch64 used to miscompile foo to just return 0. */
W
foo (unsigned char uc)
{
  V v = __builtin_convertvector ((U){ } >= us, V);
  return __builtin_shufflevector ((W){ }, v, 4) & uc;
}

int
main (void)
{
  W x = foo (5);
  if (x[0] != 5)
    __builtin_abort();
  return 0;
}

