/* PR target/93221 */
/* { dg-do compile } */
/* { dg-options "-O0 -mno-omit-leaf-frame-pointer" } */

struct S { __Int32x4_t b[2]; };

void
foo (struct S x)
{
}
