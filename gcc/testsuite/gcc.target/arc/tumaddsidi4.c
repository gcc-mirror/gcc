/* { dg-do compile } */
/* { dg-options "-mcpu=archs -O2 -mmpy-option=plus_dmpy -w" } */

/* Check how we generate umaddsidi4 patterns.  */
long a;
long long b;
unsigned c, d;

void fn1(void)
{
  b = d * (long long)c + a;
}

/* { dg-final { scan-assembler "macu" } } */
