/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O1 -mapx-features=ppx -fno-omit-frame-pointer" } */

/* { dg-final { scan-assembler "pushp" } } */
/* { dg-final { scan-assembler "popp" } } */
/* { dg-final { scan-assembler-not "leave" } } */

extern int bar (int a);
extern int *q;

void foo (int *a)
{
  q[2] = bar (q[1]);
}
