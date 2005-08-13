/* { dg-do run } */
/* { dg-options "-O1 -fno-tree-vrp -fwrapv" } */

/* PR tree-optimization/22236

    Avoid conversion of (signed char) {(uchar)1, +, (uchar)1}_x when
    it is not possible to prove that the scev does not wrap.  

    In this PR, a sequence 1, 2, ..., 255 has to be converted to
    signed char, but this would wrap: 1, 2, ..., 127, -128, ...  The
    result should not be a linear scev {(schar)1, +, (schar)1}_x.
    The conversion should be kept: (schar) {(uchar)1, +, (uchar)1}_x.
 */

void abort(void);

static inline void
foo (signed char a)
{
  int b = a - 0x7F;
  if (b > 1)
    abort();
}

int main()
{
  unsigned char b;
  for(b = 0; b < 0xFF; b++)
    foo (b);

  return 0;
}

