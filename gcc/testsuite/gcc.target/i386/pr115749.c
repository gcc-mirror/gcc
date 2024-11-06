/* PR target/115749 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic" } */
/* { dg-final { scan-assembler-times "imul" 2 } } */
/* { dg-final { scan-assembler-not "sal" } } */

typedef unsigned int uword __attribute__ ((mode (word)));

uword func(uword x)
{
    return x % 240;
}

unsigned short func2(unsigned short x)
{
  return x * 240;
}

