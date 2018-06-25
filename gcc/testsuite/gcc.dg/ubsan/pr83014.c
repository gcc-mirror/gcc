/* PR sanitizer/83014 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

int
foo (void)
{
  int data[5];
  data[0] = 0;
  data[5] = 0;
  return data[0];
}
