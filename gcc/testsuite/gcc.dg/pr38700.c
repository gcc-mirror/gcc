/* PR c/38700 */
/* { dg-do compile } */
/* { dg-options "-O0" } */
/* { dg-require-effective-target label_values } */

int
foo ()
{
  __UINTPTR_TYPE__ s = __builtin_expect ((__UINTPTR_TYPE__)&&L, 0);
L:
  return 0;
}
