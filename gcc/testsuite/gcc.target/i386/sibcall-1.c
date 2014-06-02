/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2" } */

extern int (*foo)(int);

int boo (int a)
{
  return (*foo) (a);
}

/* { dg-final { scan-assembler-not "mov" } } */
