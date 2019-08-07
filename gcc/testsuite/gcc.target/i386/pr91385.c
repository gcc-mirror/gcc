/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -dp" } */
/* { dg-final { scan-assembler-not "zero_extendsidi" } } */

unsigned long long
foo (unsigned int a)
{
  return -a;
}
