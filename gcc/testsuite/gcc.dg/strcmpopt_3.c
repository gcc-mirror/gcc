/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

__attribute__ ((noinline)) int
f1 (void)
{
  char *s0= "abcd";
  char s[8];
  __builtin_strcpy (s, s0);
  return __builtin_strcmp (s, "abc") != 0;
}

__attribute__ ((noinline)) int
f2 (void)
{
  char *s0 = "ab";
  char s[8];
  __builtin_strcpy (s, s0);
  return __builtin_strcmp ("abc", s) != 0;
}

int main (void)
{
  if (f1 () != 1
      || f2 () != 1)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "strcmp" 0 "optimized" } } */
