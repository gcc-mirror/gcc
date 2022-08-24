/* PR target/64821 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* Check that we constant fold sqrt(4.0) into 2.0. */
/* { dg-final { scan-tree-dump-not " \\\.SQRT" "optimized" } } */
/* { dg-final { scan-tree-dump " 2\\\.0e\\\+0" "optimized" } } */
/* { dg-final { scan-assembler-not "fsqrt" } } */
/* We should produce a fmov to d0 with 2.0 but currently don't, see PR 103959. */
/* { dg-final { scan-assembler-times "\n\tfmov\td0, 2.0e.0" 1 { xfail *-*-* } } } */

#include <arm_neon.h>

float64x1_t f64(void)
{
   float64x1_t a = (float64x1_t){4.0};
   return vsqrt_f64 (a);
}
