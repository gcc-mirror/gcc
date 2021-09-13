/* PR target/98567 */
/* { dg-do compile } */
/* { dg-options "-O2 -mbmi -fno-stack-protector" } */
/* { dg-final { scan-assembler-times "\tblsi" 4 } } */
/* { dg-final { scan-assembler-times "\tsetle\t" 2 } } */
/* { dg-final { scan-assembler-times "\tsetg\t" 2 } } */
/* { dg-final { scan-assembler-not "\ttest\[ld]" } } */

int
foo (unsigned long x)
{
  return 0 >= (int) (-x & x);
}

int
bar (unsigned int x)
{
  return (int) (-x & x) <= 0;
}

int
baz (unsigned long x)
{
  return (int) (x & -x) > 0;
}

int
qux (unsigned int x)
{
  return 0 < (int) (x & -x);
}
