/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "--param case-values-threshold=3 -O2" } */
/* { dg-additional-options "-funwind-tables" { target powerpc*-*-darwin* } }

/* This is testing that a trailing local label is followed by a
   nop where required.  */
   
int foo (int x)
{
  switch (x)
    {
      case 0:
        return 10;
      case 3:
        return -1;
      case 5:
        return 29;
      default:
        __builtin_unreachable();
    }
}

/* { dg-final { scan-assembler "nop\\nLFE.*" { target  { *-*-darwin* } } } } */
