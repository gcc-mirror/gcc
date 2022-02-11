/* { dg-do compile } */
/* { dg-options "-Oz" } */

int foo()
{
  return 25;
}

/* { dg-final { scan-assembler "push" } } */
/* { dg-final { scan-assembler "pop" } } */
