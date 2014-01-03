/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-final { scan-assembler "__muldi3" } } */

long long x, y, z;

void test()
{
  x = y * z;
}
  
