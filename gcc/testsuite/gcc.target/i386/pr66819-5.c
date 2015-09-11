/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mregparm=3" } */
/* { dg-final { scan-assembler "call" } } */

void (*bar)(int, int, int);

void foo(int i, int j, int k)
{
  bar(i, j, k);
}
