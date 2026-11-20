/* { dg-do compile { target { rv64 } } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=rocket -O2 -fno-ipa-icf -fdump-rtl-sched_fusion" } */
/* { dg-final { scan-rtl-dump ";; Function fusion_default" "sched_fusion" } } */
/* { dg-final { scan-rtl-dump ";; Function fusion_forced" "sched_fusion" } } */

/* Rocket uses the common -O2 default even without memory-pair rules.  */
int
fusion_default (int a)
{
  return a + 1;
}

__attribute__ ((optimize ("schedule-fusion"))) int
fusion_forced (int a)
{
  return a + 2;
}
