/* PR debug/84252 */
/* { dg-do compile } */
/* { dg-options "-g -O2" } */

struct S { __Int32x4_t b[2]; };

void
foo (struct S x)
{
}
