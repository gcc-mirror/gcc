/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

typedef struct { char s[8]; int x; } S;
extern int max_i;

int
f1 (S * s)
{ 
  int result, i;
  for (i = 0; i < max_i; i++)
    result += __builtin_strcmp (s->s, "abc") != 0 ? 2 : 1;
  return result;
}

/* { dg-final { scan-tree-dump-times "cmp_eq \\(" 1 "strlen1" } } */
