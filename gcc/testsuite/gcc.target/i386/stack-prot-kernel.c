/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target nonpic } */
/* { dg-options "-O2 -fstack-protector-all -mcmodel=kernel" } */

void test1 (int x)
{
  char p[40];
  int i;
  for (i=0; i<40; i++)
	p[i] = x;
}

/* { dg-final { scan-assembler-not "%fs" } } */
