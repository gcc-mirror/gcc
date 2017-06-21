/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 --param case-values-threshold=2" } */
/* { dg-final { scan-assembler "mtctr" } } */
/* { dg-final { scan-assembler "bctr" } } */

/* Force using a dispatch table even though by default we would generate
   ifs.  */

extern long call (long);

long
test_switch (long a, long b)
{
  long c;

  switch (a)
    {
    case 0:  c = -b;	break;
    case 1:  c = ~b;	break;
    case 2:  c = b+1;	break;
    default: c = b & 9;	break;
    }

  return call (c) + 1;
}
