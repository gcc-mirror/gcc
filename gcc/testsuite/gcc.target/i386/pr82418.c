/* PR target/82418 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "imul\[^\n\r]*1374389535" } } */

unsigned
f1(unsigned x)
{
  return x / 100;
}
