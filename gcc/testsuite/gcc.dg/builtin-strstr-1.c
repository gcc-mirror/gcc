/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "link_error" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_strstr" "optimized" } } */
/* { dg-final { scan-tree-dump-times "return p_\[0-9]*.D.;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_strchr" 1 "optimized" } } */

extern void link_error (void);

void
foo (void)
{
  const char *p = "abcdef";
  const char *q = "def";
  p++;
  q++;
  if (__builtin_strstr (p, q) != p + 3)
    link_error ();
}

char *
bar (const char *p)
{
  return __builtin_strstr (p, "");
}

char *
baz (const char *p)
{
  return __builtin_strstr (p, "d");
}
