/* PR target/83488 */
/* { dg-do compile } */
/* { dg-options "-mno-avx -mavx512vnni" } */

typedef int __v16si __attribute__((vector_size (64)));

void
foo (__v16si *a, __v16si *b, __v16si *c, __v16si *d)
{
  *a = __builtin_ia32_vpdpbusd_v16si (*b, *c, *d);
}
