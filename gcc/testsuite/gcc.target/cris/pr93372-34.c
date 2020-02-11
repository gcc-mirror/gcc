/* Check that btst/btstq other than a field starting at bit 0, is used. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tand" } } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest" } } */
/* { dg-final { scan-assembler-times "\tbtstq" 3 } } */
/* { dg-final { scan-assembler-times "\tbtst " 3 } } */

void foo(void);

void f(int *a)
{
  if ((*a & 32) != 0)
    foo();
}

void g(short int *a)
{
  if ((*a & 128) == 0)
    foo();
}

void h(char *a)
{
  if ((*a & 64) != 0)
    foo();
}

void i(int *a, unsigned int n)
{
  if ((*a & (1 << n)) != 0)
    foo();
}

void j(short int *a, unsigned int n)
{
  if ((*a & (1 << n)) == 0)
    foo();
}

void k(char *a, unsigned int n)
{
  if ((*a & (1 << n)) != 0)
    foo();
}
