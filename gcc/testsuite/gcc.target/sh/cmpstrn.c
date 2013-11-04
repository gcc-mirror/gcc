/* Check that the __builtin_strncmp function is inlined
   when optimizing for speed.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } } */
/* { dg-final { scan-assembler-not "jmp" } } */
/* { dg-final { scan-assembler-times "cmp/str" 1 } } */

/* Test that the cmp/str loop is optimized out.  */
test01(const char *s1, const char *s2, int n)
{
  return __builtin_strncmp (s1, "abcde", 3);
}

/* Test that the cmp/str loop is used.  */
test02(const char *s1, const char *s2, int n)
{
  return __builtin_strncmp (s1, "abcdefghi", 8);
}


