/* PR tree-optimization/112673 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-mavx2" { target i?86-*-* x86_64-*-* } } */

int
foo (_BitInt(256) x)
{
  return __builtin_ctzg ((unsigned _BitInt(512)) x);
}
