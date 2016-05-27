/* { dg-do compile } */
/* { dg-options "-O2 --save-temps" } */

int
f (int a, ...)
{
  /* { dg-final { scan-assembler-not "str" } } */
  return a;
}

/* { dg-final { cleanup-saved-temps } } */
