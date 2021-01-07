/* PR target/98567 */
/* { dg-do compile } */
/* { dg-options "-O2 -mbmi -fno-stack-protector" } */
/* { dg-final { scan-assembler-times "\tblsi" 4 } } */
/* { dg-final { scan-assembler-times "\tsetne\t" 2 } } */
/* { dg-final { scan-assembler-times "\tsete\t" 2 } } */
/* { dg-final { scan-assembler-not "\ttest\[ld]" } } */

int
foo (unsigned long x)
{
  return (-x & x) == 0;
}

int
bar (unsigned int x)
{
  return (-x & x) == 0;
}

int
baz (unsigned long x)
{
  return (x & -x) != 0;
}

int
qux (unsigned int x)
{
  return 0 != (x & -x);
}
