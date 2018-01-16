/* { dg-do compile } */
/* { dg-options "-pedantic-errors -Wno-vla" } */
/* { dg-require-effective-target alloca } */
/* { dg-require-effective-target alloca } */

void func (int i)
{
  int array[i];
}
