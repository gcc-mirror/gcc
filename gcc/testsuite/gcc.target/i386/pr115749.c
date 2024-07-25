/* PR target/115749 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic" } */
/* { dg-final { scan-assembler-times "imul" 2 } } */
/* { dg-final { scan-assembler-not "sal" } } */

unsigned long func(unsigned long x)
{
    return x % 240;
}

unsigned short func2(unsigned short x)
{
  return x * 240;
}

