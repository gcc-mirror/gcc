/* Check that the __builtin_strlen function is inlined with cmp/str
   when optimizing for speed.  */
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "jmp" } } */
/* { dg-final { scan-assembler-times "cmp/str" 2 } } */
/* { dg-final { scan-assembler-times "tst\t#3" 1 } } */

int
test00 (const char *s1)
{
  return __builtin_strlen (s1);
}

/* Check that no test for alignment is needed.  */
int
test03 (const char *s1)
{
  return __builtin_strlen (__builtin_assume_aligned (s1, 4));
}
