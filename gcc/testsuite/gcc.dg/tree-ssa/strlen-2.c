/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

void f (unsigned);

void f3 (void)
{
  char s[] = "1234";

  f (__builtin_strlen (s));
  f (__builtin_strlen (s));
  f (__builtin_strlen (s));
}

/* { dg-final { scan-tree-dump-times "strlen" 0 "strlen" } } */
