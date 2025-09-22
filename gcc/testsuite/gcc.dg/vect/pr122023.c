/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */

unsigned int gvar1;
int gvar2;
void f ()
{
  unsigned int temp1;
  while (gvar1--)
    {
      temp1 = gvar2;
      gvar2 >>= 1;
      gvar2 &= 2147483647;
      temp1 <<= 31;
      gvar2 |= temp1;
    }
}
