/* { dg-do compile } */
/* { dg-additional-options "-O0" } */

/* Local variable 'x' is saved on the stack.  */

extern int use (int *x);
extern int bar (int *x);
extern int baz (int *x);

int a[10];

int foo (int n)
{
  int x = use (a);
  if (x)
    return bar (a);
  else
    return baz (a);
}

/* { dg-final { scan-assembler-times {\tirg\t} 1 } } */
/* { dg-final { scan-assembler-times {\tsubg\t...?} 1 } } */
