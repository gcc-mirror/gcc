/* { dg-do compile { target { ! c++14 } } } */
/* { dg-options "-pedantic-errors -Wvla" } */

void func (int i)
{
  int array[i]; /* { dg-error "ISO C.* forbids variable.* array 'array'" } */
}
