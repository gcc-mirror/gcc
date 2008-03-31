/* { dg-do compile } */
/* { dg-options "-Os -fdump-rtl-loop2_invariant" } */

const volatile int g_361 = 3L;
volatile int g_2 = 0L;
void func_1 (void)
{
  for (g_2 = 0; g_2 > 10; g_2++)
    {
      int l_357 = g_361;
    }
}

/* { dg-final { scan-rtl-dump-times "Decided to move invariant" 0 "loop2_invariant" } } */
/* { dg-final { cleanup-rtl-dump "loop2_invariant" } } */
