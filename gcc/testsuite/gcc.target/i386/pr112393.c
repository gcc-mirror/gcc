/* { dg-do compile } */
/* { dg-options "-mavx512f -Wuninitialized" } */
typedef _Float16 __attribute__((__vector_size__ (32))) V;

V v;

void
foo (void)
{
  (void) __builtin_shufflevector (v, __builtin_shufflevector (v, (V){},
                                                              3, 0, 2, 2,
							      5, 6, 3, 7, 5,
							      6, 0, 8, 6, 4,
							      3, 2, 8, 9, 5,
							      8, 8, 7, 5, 4,
							      8, 9, 1, 2, 4,
							      9, 9, 7),
                                  40, 33);
}

