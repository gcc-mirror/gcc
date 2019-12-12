/* PR rtl-optimization/86620 */
/* { dg-do compile } */
/* { dg-options "-O2 -flive-range-shrinkage --param=max-sched-ready-insns=0" } */
/* { dg-error "argument to '--param=max-sched-ready-insns=' is not between 1 and 65536" "" { target *-*-* } 0 } */

void
foo (void)
{
}
