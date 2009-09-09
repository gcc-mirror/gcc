/* { dg-do compile } */
/* { dg-options "-w" } */

int test (char v1)
{
  v1 >>= 0xdebecced;
  return v1;
}
