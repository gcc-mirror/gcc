/* { dg-do compile } */
/* { dg-options "-march=loongarch64 -mabi=lp64d -fdump-tree-einline-details -O2" } */
/* { dg-final { scan-tree-dump {missed:   not inlinable: bar/\d+ -> foo/\d+, target specific option mismatch} "einline" } } */
/* { dg-final { scan-tree-dump-not {\(inlined\)} "einline" } } */

void
foo (void)
{}

void
__attribute__ ((target ("cmodel=extreme")))
bar (void)
{
  foo ();
}
