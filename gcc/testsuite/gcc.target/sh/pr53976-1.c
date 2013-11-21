/* Check that the SH specific sh_optimize_sett_clrt RTL optimization pass
   works as expected.  */
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } } */
/* { dg-final { scan-assembler-times "clrt" 2 } } */
/* { dg-final { scan-assembler-times "sett" 1 } } */

long long
test_00 (long long a, long long b, long long c, int d)
{
  /* One of the blocks should have a clrt and the other one should not.  */
  if (d > 5)
    return a + b;
  else
    return a + c;
}

long long
test_01 (long long a, long long b)
{
  /* Must see a clrt because T bit is undefined at function entry.  */
  return a + b;
}

int
test_02 (const char* a)
{
  /* Must not see a sett after the inlined strlen.  */
  return __builtin_strlen (a);
}

int
test_03 (int a, int b, int c, int d)
{
  /* One of the blocks should have a sett and the other one should not.  */
  if (d > 4)
    return a + b + 1;
  else
    return a + c + 1;
}
