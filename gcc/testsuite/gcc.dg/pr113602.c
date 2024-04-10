/* { dg-do compile } */
/* { dg-require-effective-target bitint575 } */
/* { dg-options "-O2 -fno-tree-loop-optimize" } */

_BitInt(503)
f(void)
{
  register _BitInt(503) r asm(""); /* { dg-error "invalid" } */
  return r;
}
