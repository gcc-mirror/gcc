/* { dg-do compile { target { ! c++1y } } } */
/* { dg-options "-pedantic-errors -Wvla" } */

void func (int i)
{
  int array[i]; /* { dg-error "ISO C.* forbids variable.* array 'array'" } */
}
