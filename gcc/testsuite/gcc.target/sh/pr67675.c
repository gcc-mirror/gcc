/* Check that run time alignment tests are generated only for inputs of
   unknown alignment.  */
/* { dg-do compile }  */
/* { dg-options "-O2" }  */
/* { dg-final { scan-assembler-not "jmp|bsr|jsr" } }  */
/* { dg-final { scan-assembler-times {tst	#3,r0} 6 } }  */
/* { dg-final { scan-assembler-times {or	r[0-9],r[0-9]} 1 } }  */

int
test_00 (const char* x, const char* y)
{
  /* 1x or reg,reg, 1x tst #3,r0  */
  return __builtin_strcmp (x, y);
}

int
test_01 (const char* x, const char* y)
{
  /* 1x tst #3,r0  */
  return __builtin_strcmp (__builtin_assume_aligned (x, 4), y);
}

int
test_02 (const char* x, const char* y)
{
  /* 1x tst #3,r0  */
  return __builtin_strcmp (x, __builtin_assume_aligned (y, 4));
}

int
test_03 (const char* x, const char* y)
{
  return __builtin_strcmp (__builtin_assume_aligned (x, 4),
			   __builtin_assume_aligned (y, 4));
}

int
test_04 (const char* x, const char* y)
{
  /* 1x tst #3,r0  */
  return __builtin_strcmp (x, "1234567");
}

int
test_05 (const char* x, const char* y)
{
  /* 1x tst #3,r0  */
  return __builtin_strcmp ("1234567", y);
}

int
test_06 (const char* s1)
{
  /* 1x tst #3,r0  */
  return __builtin_strncmp (s1, "abcdabcd", 8);
}

int
test_07 (const char* s1)
{
  return __builtin_strncmp (__builtin_assume_aligned (s1, 4), "abcdabcd", 8);
}
