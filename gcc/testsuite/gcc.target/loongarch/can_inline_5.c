/* { dg-do compile } */
/* { dg-options "-march=loongarch64 -mabi=lp64d -fdump-tree-einline-details -O2" } */
/* { dg-final { scan-tree-dump {\(inlined\)} "einline" } } */

void
__attribute__ ((always_inline))
inline
foo (void)
{}

void
__attribute__ ((target ("cmodel=extreme")))
bar (void)
{
  foo ();
}
