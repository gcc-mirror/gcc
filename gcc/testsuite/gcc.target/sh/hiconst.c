/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1" } */

char a;
int b;

foo(char *pt, int *pti)
{
  a = 0;
  b = 0;
  *pt = 0;
  *pti = 0;
}

rab(char *pt, int *pti)
{
  pt[2] = 0;
  pti[3] = 0;
}

/* { dg-final { scan-assembler-times "mov\t#0" 2 } } */

