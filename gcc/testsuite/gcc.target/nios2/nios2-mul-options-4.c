/* { dg-do compile } */
/* { dg-options "-mno-hw-mul" } */
/* { dg-final { scan-assembler "__mulsi3" } } */

int x, y, z;

void test()
{
  x = y * z;
}
  
