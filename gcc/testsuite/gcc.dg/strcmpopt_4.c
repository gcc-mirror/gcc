/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

typedef struct { char s[8]; int x; } S;

extern int max_i;

int f_param (S s)
{
  int result = 0;
  for (int i = 0; i < max_i; i++)
    result += __builtin_strcmp (s.s, "abc") != 0 ? 2 : 1;
  return result;
}


S s;

int f_object (void)
{
  int result = 0;
  for (int i = 0; i < max_i; i++)
    result += __builtin_strcmp (s.s, "abc") != 0 ? 2 : 1;
  return result;
}

/* { dg-final { scan-tree-dump-times "cmp_eq \\(" 2 "strlen1" } } */
