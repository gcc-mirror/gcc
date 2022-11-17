/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O2 -fdump-rtl-split1" } */

long int
test (void)
{
  return 0x1234567890abcdef;
}
/* { dg-final { scan-rtl-dump-times "scanning new insn with uid" 6 "split1" } } */

