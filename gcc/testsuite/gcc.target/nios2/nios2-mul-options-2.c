/* { dg-do compile } */
/* { dg-options "-mhw-mulx" } */
/* { dg-final { scan-assembler-not "__muldi3" } } */

long long x, y, z;

void test()
{
  x = y * z;
}
  
