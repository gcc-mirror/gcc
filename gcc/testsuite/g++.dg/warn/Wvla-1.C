/* { dg-do compile } */
/* { dg-options "-Wvla" } */

void func (int i)
{
  int array[i]; /* { dg-warning "variable length array 'array' is used" } */
}
