/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

struct S { char a[3]; char b[5]; } s = { "abc", "defg" };

__SIZE_TYPE__
foo (struct S s, int a, int b)
{
  char *p = (char *) &s.a[0];
  if (a)
    p = (char *) &s.a;
  else if (b)
    p = (char *) &s;
  return __builtin_strlen (p);
}

__SIZE_TYPE__
bar (int a, int b)
{
  char *p = (char *) &s.a[0];
  if (a)
    p = (char *) &s.a;
  else if (b)
    p = (char *) &s;
  return __builtin_strlen (p);
}

/* { dg-final { scan-tree-dump-times "strlen \\\(&s" 2 "fre1" } } */
