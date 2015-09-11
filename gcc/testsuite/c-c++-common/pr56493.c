/* PR c++/56493 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-gimple" } */

unsigned long long bar (void);
int x;

void
foo (void)
{
  x += bar ();
}

/* Verify we narrow the addition from unsigned long long to unsigned int type.  */
/* { dg-final { scan-tree-dump "  (\[a-zA-Z._0-9]*) = \\(unsigned int\\) \[^;\n\r]*;.*  (\[a-zA-Z._0-9]*) = \\(unsigned int\\) \[^;\n\r]*;.* = \\1 \\+ \\2;" "gimple" { target { ilp32 || lp64 } } } } */
