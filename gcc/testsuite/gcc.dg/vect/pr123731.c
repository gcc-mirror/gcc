/* { dg-do run } */

typedef unsigned short A __attribute__ ((vector_size (4 * sizeof (short))));
typedef short B __attribute__ ((vector_size (8 * sizeof (short))));
typedef unsigned C __attribute__ ((vector_size (4 * sizeof (int))));
unsigned long long c;

__attribute__ ((noipa)) void
foo (A *a)
{
  C b[9] = {};
  unsigned d = __builtin_convertvector (
  __builtin_shufflevector ((A) {}, *a, 0, 5, 7, 3, 6, 2, 5, 4), B)[2];
  b[0] += (C) {d, d, d, d};
  c += b[0][0];
}

int
main ()
{
  A t = (A) {0, 0, 0, -6};
  foo (&t);
  if (sizeof (short) == 2
      && sizeof (int) == 4
      && __CHAR_BIT__ == 8
      && c != -6U)
    __builtin_abort ();
}
