/* { dg-do compile { target { { ! x32 }  && nonpic } } } */
/* { dg-options "-O2" } */

extern int (*foo)(int);

int boo (int a)
{
  return (*foo) (a);
}

/* { dg-final { scan-assembler-not "mov" } } */
