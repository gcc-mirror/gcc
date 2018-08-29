/* PR rtl-optimization/86620 */
/* { dg-do compile } */
/* { dg-options "-O2 -flive-range-shrinkage --param=max-sched-ready-insns=0" } */
/* { dg-error "minimum value of parameter 'max-sched-ready-insns' is 1" "" { target *-*-* } 0 } */

void
foo (void)
{
}
