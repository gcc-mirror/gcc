/* PR rtl-optimization/98271 */
/* PR rtl-optimization/98276 */
/* PR tree-optimization/98279 */
/* { dg-do compile } */
/* { dg-options "-O --param=align-loop-iterations=1197120096074465280 --param=gcse-cost-distance-ratio=2147483648 --param=hot-bb-frequency-fraction=2147483648" } */
/* { dg-error "argument to .--param=align-loop-iterations=. is bigger than 2147483647" "" { target *-*-* } 0 } */
/* { dg-error "argument to .--param=gcse-cost-distance-ratio=. is bigger than 2147483647" "" { target *-*-* } 0 } */
/* { dg-error "argument to .--param=hot-bb-frequency-fraction=. is bigger than 2147483647" "" { target *-*-* } 0 } */

void
foo (void)
{
}
