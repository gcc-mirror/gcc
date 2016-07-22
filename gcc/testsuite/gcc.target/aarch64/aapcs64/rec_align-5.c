/* Test AAPCS layout (alignment) for callee.  */

/* { dg-do run { target aarch64*-*-* } } */

extern void abort (void);

typedef __attribute__ ((__aligned__ (8))) int alignedint;

alignedint a = 11;
alignedint b = 13;
alignedint c = 17;
alignedint d = 19;
alignedint e = 23;
alignedint f = 29;
alignedint g = 31;
alignedint h = 37;
alignedint i = 41;
alignedint j = 43;

void
test_passing_many_alignedint (alignedint x0, alignedint x1, alignedint x2,
			      alignedint x3, alignedint x4, alignedint x5,
			      alignedint x6, alignedint x7, alignedint stack,
			      alignedint stack8)
{
  if (x0 != a
      || x1 != b
      || x2 != c
      || x3 != d
      || x4 != e
      || x5 != f
      || x6 != g
      || x7 != h
      || stack != i
      || stack8 !=j)
    abort ();
}

int
main (int argc, char **argv)
{
  test_passing_many_alignedint (a, b, c, d, e, f, g, h, i, j);
  return 0;
}
