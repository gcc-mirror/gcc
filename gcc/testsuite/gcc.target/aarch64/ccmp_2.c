/* { dg-do compile } */
/* { dg-options "-O2" } */

int g(void);
int h(int a, _Bool c)
{
  if (a != 0 && c)
    return g();
  return 1;
}

/* { dg-final { scan-assembler "\tccmp\t" } } */
