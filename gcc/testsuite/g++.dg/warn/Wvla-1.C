/* { dg-do compile } */
/* { dg-options "-Wvla" } */

void func (int i)
{
  int array[i]; /* { dg-warning "7:variable length array 'array' is used" } */
}
