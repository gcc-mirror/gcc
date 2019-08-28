/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

char a[4];

void f (void)
{
  __builtin_strcpy (a, "abc");

  if (__builtin_strlen (a) != 3)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-not "__builtin_strlen" "strlen1" } } */
