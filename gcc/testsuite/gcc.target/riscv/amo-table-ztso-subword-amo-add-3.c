/* { dg-do compile } */
/* Verify that subword atomic op mappings match the Ztso suggested mapping.  */
/* { dg-add-options riscv_a } */
/* { dg-add-options riscv_ztso } */
/* { dg-final { scan-assembler-times "lr.w\t" 1 } } */
/* { dg-final { scan-assembler-times "sc.w\t" 1 } } */

void foo (short* bar, short* baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_RELEASE);
}
