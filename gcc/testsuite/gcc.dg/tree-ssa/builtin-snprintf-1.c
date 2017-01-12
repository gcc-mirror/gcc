/* { dg-do compile } */
/* { dg-options "-O2 -fprintf-return-value -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "__builtin_snprintf" "optimized"} } */

int
foo (void)
{
  int a = __builtin_snprintf (0, 0, "%s", "abcdefgh");
  return a;
}

void
bar (void)
{
  __builtin_snprintf (0, 0, "%s", "abcdefgh");
}
