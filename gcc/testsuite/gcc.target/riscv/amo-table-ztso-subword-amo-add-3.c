/* { dg-do compile } */
/* Verify that subword atomic op mappings match the Ztso suggested mapping.  */
/* { dg-options "-march=rv64id_ztso -mabi=lp64d" } */
/* { dg-final { scan-assembler-times "lr.w\t" 1 } } */
/* { dg-final { scan-assembler-times "sc.w\t" 1 } } */

void foo (short* bar, short* baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_RELEASE);
}
