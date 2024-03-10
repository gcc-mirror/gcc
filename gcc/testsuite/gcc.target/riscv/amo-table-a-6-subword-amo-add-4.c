/* { dg-do compile } */
/* Verify that subword atomic op mappings match Table A.6's recommended mapping.  */
/* { dg-final { scan-assembler-times "lr.w.aq\t" 1 } } */
/* { dg-final { scan-assembler-times "sc.w.rl\t" 1 } } */

void foo (short* bar, short* baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_ACQ_REL);
}
