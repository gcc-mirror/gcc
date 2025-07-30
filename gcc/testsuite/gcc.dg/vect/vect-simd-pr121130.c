/* { dg-do compile } */

int n2;

__attribute__((simd)) char
w7(void)
{
  short int xb = n2;
    xb = w7() < 1;
  return xb;
}
