/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* powerpc-ibm-aix* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-O2 -mdejagnu-cpu=power6 -mhard-dfp" } */
/* { dg-final { scan-assembler-not   "lfiwzx"   } } */
/* { dg-final { scan-assembler-times "lfd"    2 } } */
/* { dg-final { scan-assembler-times "dctdp"  2 } } */
/* { dg-final { scan-assembler-times "dadd"   1 } } */
/* { dg-final { scan-assembler-times "drsp"   1 } } */

/* Test that for power6 we need to use a bounce buffer on the stack to load
   SDmode variables because the power6 does not have a way to directly load
   32-bit values from memory.  */
_Decimal32 a;

void inc_dec32 (void)
{
  a += (_Decimal32) 1.0;
}
