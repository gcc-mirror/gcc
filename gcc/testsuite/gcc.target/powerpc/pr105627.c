/* Specify -w to disable some warnings, such as: -Wpsabi.  */
/* { dg-options "-Og -fcompare-debug -mdejagnu-cpu=power8 -w" } */

typedef unsigned char __attribute__ ((__vector_size__ (8))) U;
typedef unsigned char __attribute__ ((__vector_size__ (64))) V;

U u;
char c;
V v;

V
foo (void)
{
  V w = c
	& __builtin_shufflevector (u, (V){0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
					  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
					  0, 0, 0, 0, 0, 0, 0, 5},
				   24, 24, 41, 45, 53, 60, 22, 35, 45, 12, 61,
				   9, 52, 15, 44, 46, 5, 5, 1, 0, 4, 9, 0, 8, 5,
				   7, 2, 5, 9, 2, 7, 7, 5, 6, 0, 2, 6, 1, 7, 7,
				   0, 4, 0, 1, 7, 2, 5, 3, 2, 3, 5, 6, 6, 6, 0,
				   6, 1, 9, 0, 5, 4, 3, 5, 4);
  w = w + v;
  return w;
}

