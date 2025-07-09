/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc -mabi=ilp32 -fprofile-update=atomic -fcondition-coverage" { target { rv32 } } } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64 -fprofile-update=atomic -fcondition-coverage" { target { rv64 } } } */

/* rv32 only has 32-bit atomic instructions, so the 64-bit gcov decision
   counter update is split into up to two 32-bit atomic bitwise-or
   operations, one per 32-bit half.  Here each counter update is a small
   compile-time constant, so the always-zero high half is folded away and
   only one atomic bitwise-or per branch remains.  rv64 has 64-bit atomic
   instructions, so a single atomic bitwise-or suffices there too.  */

int a (int);
int b (int);

int
f (int i)
{
  if (i)
    return a (i);
  else
    return b (i);
}

/* { dg-final { scan-assembler-times "\tamoor\\.w\t" 2 { target { rv32 } } } } */
/* { dg-final { scan-assembler-not "\tamoor\\.d\t" { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "\tamoor\\.d\t" 2 { target { rv64 } } } } */
/* { dg-final { scan-assembler-not "\tamoor\\.w\t" { target { rv64 } } } } */
