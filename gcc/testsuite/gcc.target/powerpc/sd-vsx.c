/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* powerpc-ibm-aix* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-O2 -mcpu=power7 -mhard-dfp" } */
/* { dg-final { scan-assembler-times "lfiwzx" 2 } } */
/* { dg-final { scan-assembler-times "stfiwx" 1 } } */
/* { dg-final { scan-assembler-not   "lfd"      } } */
/* { dg-final { scan-assembler-not   "stfd"     } } */
/* { dg-final { scan-assembler-times "dctdp"  2 } } */
/* { dg-final { scan-assembler-times "dadd"   1 } } */
/* { dg-final { scan-assembler-times "drsp"   1 } } */

/* Test that power7 can directly load/store SDmode variables without using a
   bounce buffer.  */
_Decimal32 a;

void inc_dec32 (void)
{
  a += (_Decimal32) 1.0;
}
