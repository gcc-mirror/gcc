/* PR target/101716 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -dp" } */
/* { dg-final { scan-assembler-not "zero_extendsidi" } } */

void sample1 (unsigned long long x, unsigned long long *r)
{
  unsigned int t = -1;
  *r = (x << 1) & t;
}
