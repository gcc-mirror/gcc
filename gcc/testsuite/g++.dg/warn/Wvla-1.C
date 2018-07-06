/* { dg-do compile } */
/* { dg-options "-Wvla" } */
/* { dg-require-effective-target alloca } */

void func (int i)
{
  int array[i]; /* { dg-warning "variable length array 'array' is used" } */
}
