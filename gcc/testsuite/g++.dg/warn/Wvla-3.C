/* { dg-do compile } */
/* { dg-options "-pedantic-errors -Wno-vla" } */

void func (int i)
{
  int array[i];
}
