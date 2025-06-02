/* PR target/120360 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-stack-protector -masm=att" } */
/* { dg-additional-options "-fno-pic" { target { ! *-*-darwin* } } } */
/* { dg-final { scan-assembler-times "\tjn*s\t" 3 } } */
/* { dg-final { scan-assembler-times "\tcmp\[lq]\t%" 1 } } */
/* { dg-final { scan-assembler-times "\tcmp\[lq]\t\\\$-1234," 1 } } */
/* { dg-final { scan-assembler-times "\tcmp\[lq]\t\\\$2345," 1 } } */
/* { dg-final { scan-assembler-not "\tadd\[lq]\t" { target { ! *-*-darwin* } } } } */
/* { dg-final { scan-assembler-not "\tsub\[lq]\t" { target { ! *-*-darwin* } } } } */

void qux (unsigned long);

void
foo (unsigned long x, unsigned long y)
{
  unsigned long z = x - y;
  if ((long) z < 0)
    qux (x);
}

void
bar (unsigned long x)
{
  unsigned long z = x + 1234;
  if ((long) z < 0)
    qux (x);
}

void
baz (unsigned long x)
{
  unsigned long z = x - 2345;
  if ((long) z < 0)
    qux (x);
}
