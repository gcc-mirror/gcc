/* Check that the __builtin_strncmp function is inlined
   when optimizing for speed.  */
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } } */
/* { dg-final { scan-assembler-not "jmp" } } */
/* { dg-final { scan-assembler-times "cmp/str" 1 } } */

/* Test that cmp/str is not used for small lengths.  */
test01(const char *s1)
{
  return __builtin_strncmp (s1, "abcde", 3);
}

/* Test that the cmp/str loop is used.  */
test02(const char *s1)
{
  return __builtin_strncmp (s1, "abcdefghi", 8);
}

/* Test that no call is generated  */
test03(const char *s1, int n)
{
  return __builtin_strncmp (s1, "abcde", n);
}



