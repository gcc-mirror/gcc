/* PR tree-optimization/115544 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O -fno-tree-fre -fno-tree-ccp -fno-tree-forwprop" } */

#if __BITINT_MAXWIDTH__ >= 129
typedef _BitInt(129) B;
#else
typedef _BitInt(63) B;
#endif
B a, b;

int
foo (void)
{
  return __builtin_mul_overflow (a, 1, &b);
}
