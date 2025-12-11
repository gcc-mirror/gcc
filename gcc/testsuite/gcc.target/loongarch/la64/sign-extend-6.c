/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O2" } */
/* { dg-final { scan-assembler-times "addi.w\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,1\t" 1 } } */

extern unsigned short d;
int
test (int a, unsigned short b)
{
  if (a > 0)
    {
      d = 1;
      if (b > d)
       return 10;
    }

  return 50;
}
