/* Test AAPCS layout (alignment) for callee.  */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O2 -fno-inline" } */

extern void abort (void);

typedef __attribute__((aligned (8))) int alignedint;

alignedint a = 11;
alignedint b = 13;
alignedint c = 17;
alignedint d = 19;
alignedint e = 23;
alignedint f = 29;

void
foo (alignedint r0, alignedint r1, alignedint r2, alignedint r3,
     alignedint stack, alignedint stack4)
{
  if (r0 != a
      || r1 != b
      || r2 != c
      || r3 != d
      || stack != e
      || stack4 !=f)
    abort ();
}

int
main (int argc, char **argv)
{
  foo (a, b, c, d, e, f);
  return 0;
}
