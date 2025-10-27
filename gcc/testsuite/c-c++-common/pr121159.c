/* PR middle-end/121159 */
/* { dg-do compile { target musttail } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "foo \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } } */
		
[[noreturn, gnu::noipa]] void
foo (void)
{
  for (;;)
    ;
}

void
bar (void)
{
  [[gnu::musttail]] return foo ();
}
