/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

unsigned g (void)
{
  char d[8];
  const char s[] = "0123";
  __builtin_memcpy (d, s, sizeof s);
  return __builtin_strlen (d);
}

/* { dg-final { scan-tree-dump-not "__builtin_strlen" "strlen1" } } */
