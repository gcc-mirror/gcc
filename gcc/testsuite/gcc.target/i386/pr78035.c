/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */

extern int a;
extern int b;
extern int c;

int foo(int choose_a)
{
  int *p;
  if (choose_a)
    p = &a;
  else
    p = &b;
  return p != &c;
}

int bar ()
{
  return &a != &c;
}

/* We should not optimize away either comparison.  */
/* { dg-final { scan-assembler-times "cmp" 2 } } */
