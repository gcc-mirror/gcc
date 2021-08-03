/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake" } */

typedef unsigned int __attribute__((__vector_size__ (32))) U;
typedef unsigned char __attribute__((__vector_size__ (64))) V;

V g;

U
foo (void)
{
  V v = __builtin_shufflevector (g, g,
				 0, 1, 2, 0, 5, 1, 0, 1, 3, 2, 3, 0, 4, 3, 1, 2,
				 2, 0, 4, 2, 3, 1, 1, 2, 3, 4, 1, 1, 0, 0, 5, 2,
				 0, 3, 3, 3, 3, 4, 5, 0, 1, 5, 2, 1, 0, 1, 1, 2,
				 3, 2, 0, 5, 4, 5, 1, 0, 1, 4, 4, 3, 4, 5, 2, 0);
  v ^= 255;
  V w = v + g;
  U u = ((union { V a; U b; }) w).b + ((union { V a; U b; }) w).b[1];
  return u;
}

/* { dg-final { scan-assembler-not "\.byte\[ \t\]+-1\n" } } */
