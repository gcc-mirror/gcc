/* Check the common scheduling-fusion default at -Os and -O3 across
   functions with explicit scheduling-fusion options.  */
/* { dg-do compile { target { rv64 } } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -Os -mtune=xt-c9501fdvt -fno-ipa-icf -fdump-rtl-sched_fusion" } */
/* { dg-final { scan-rtl-dump-times ";; Function fusion_fast" 2 "sched_fusion" } } */
/* { dg-final { scan-rtl-dump ";; Function fusion_explicit_on" "sched_fusion" } } */
/* { dg-final { scan-rtl-dump-times ";; Function fusion_size" 3 "sched_fusion" } } */
/* { dg-final { scan-rtl-dump-not ";; Function fusion_explicit_off" "sched_fusion" } } */

int
fusion_size_before (int a)
{
  return a + 1;
}

__attribute__ ((optimize ("O3"))) int
fusion_fast (int a)
{
  return a + 2;
}

__attribute__ ((optimize ("Os"))) int
fusion_size_local (int a)
{
  return a + 3;
}

__attribute__ ((optimize ("O3", "no-schedule-fusion"))) int
fusion_explicit_off (int a)
{
  return a + 4;
}

__attribute__ ((optimize ("Os", "schedule-fusion"))) int
fusion_explicit_on (int a)
{
  return a + 5;
}

__attribute__ ((optimize ("O3"))) int
fusion_fast_again (int a)
{
  return a + 6;
}

int
fusion_size_after (int a)
{
  return a + 7;
}
