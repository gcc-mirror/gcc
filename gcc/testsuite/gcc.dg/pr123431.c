/* PR tree-optimization/123431 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

extern void foo (int);

extern inline __attribute__((always_inline)) void
bar (int x, ...)
{
  if (__builtin_constant_p (__builtin_va_arg_pack ()))
    foo (x);
}

void
baz (int x)
{
  bar (1, 2);
  bar (3, x);
}
