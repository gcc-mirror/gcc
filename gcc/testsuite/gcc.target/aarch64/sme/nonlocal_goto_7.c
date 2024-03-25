/* { dg-options "-O2 -fno-schedule-insns -fno-schedule-insns2" } */

void run(void (*)() __arm_inout("za"));
void callee () __arm_inout("za");

int
foo (int *ptr)
{
  __label__ failure;

  void bar () __arm_inout("za")
  {
    callee ();
    *ptr += 1;
    goto failure;
  }
  run (bar);
  return 1;

failure:
  return 0;
}

// { dg-final { scan-assembler-not {\tsmstart\t} } }
// { dg-final { scan-assembler-not {\tsmstop\t} } }
