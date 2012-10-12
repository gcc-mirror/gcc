/* Check that the redundant test removal code in the *cbranch_t split works
   as expected on non-SH2A targets.  Because on SH2A the movrt instruction
   is used, this test is re-used and checked differently in pr51244-16.c.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" "-m2a*" } { "" } } */
/* { dg-final { scan-assembler-times "tst" 6 } } */
/* { dg-final { scan-assembler-times "movt" 6 } } */
/* { dg-final { scan-assembler-times "xor" 3 } } */
/* { dg-final { scan-assembler-not "extu|exts|negc" } } */

typedef char bool;

int
test_0 (int a, int b, int c, int* d)
{
  /* non SH2A: 1x tst, 1x movt, 1x xor
         SH2A: 1x tst, 1x movrt  */
  bool x = a == 0;
  d[2] = !x;
  return x ? b : c;
}

int
test_1 (int a, int b, int c, int* d)
{
  /* 1x tst, 1x movt  */
  bool x = a != 0;
  d[2] = !x;
  return x ? b : c;
}

int
test_2 (int a, int b, int c, char* d)
{
  /* Check that there is no sign/zero-extension before the store.
     non SH2A: 1x tst, 1x movt, 1x xor
         SH2A: 1x tst, 1x movrt  */
  bool x = a == 0;
  d[2] = !x;
  return x ? b : c;
}

int
test_3 (int a, int b, int c, char* d)
{
  /* Check that there is no sign/zero-extension before the store.
     1x tst, 1x movt  */
  bool x = a != 0;
  d[2] = !x;
  return x ? b : c;
}

int
test_4 (int a, int b, int c, char* d)
{
  /* 1x tst, 1x movt  */
  bool x = a != 0;
  d[2] = !x;
  return !x ? b : c;
}

int
test_5 (int a, int b, int c, char* d)
{
  /* non SH2A: 1x tst, 1x movt, 1x xor
         SH2A: 1x tst, 1x movrt  */
  bool x = a == 0;
  d[2] = !x;
  return !x ? b : c;
}
