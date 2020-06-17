/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

char s[100] = {'a','b','c','d'};
typedef struct { char s[8]; int x; } S;

__attribute__ ((noinline)) int
f1 (S *s)
{
  /* Member arrays not handled due to the fix for PR 92765.  */
  return 0; // __builtin_strcmp (s->s, "abc") != 0;
}

__attribute__ ((noinline)) int
f2 (void)
{
  return __builtin_strcmp (s, "abc") != 0;
}

__attribute__ ((noinline)) int
f3 (S *s)
{
  return 0; // __builtin_strcmp ("abc", s->s) != 0;
}

__attribute__ ((noinline)) int
f4 (void)
{
  return __builtin_strcmp ("abc", s) != 0;
}

__attribute__ ((noinline)) int
f5 (S *s)
{
  return 0; // __builtin_strncmp (s->s, "abc", 3) != 0;
}

__attribute__ ((noinline)) int
f6 (void)
{
  return __builtin_strncmp (s, "abc", 2) != 0;
}

__attribute__ ((noinline)) int
f7 (S *s)
{
  return 0; // __builtin_strncmp ("abc", s->s, 3) != 0;
}

__attribute__ ((noinline)) int
f8 (void)
{
  return __builtin_strncmp ("abc", s, 2) != 0;
}

int main (void)
{
  S ss = {{'a','b','c'}, 2};

  if (f1 (&ss) != 0 || f2 () != 1 || f3 (&ss) != 0 ||
      f4 () != 1 || f5 (&ss) != 0 || f6 () != 0 ||
      f7 (&ss) != 0 || f8 () != 0)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "cmp_eq \\(" 4 "strlen1" } } */
