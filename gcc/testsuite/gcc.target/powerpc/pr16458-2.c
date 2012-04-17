/* Test cse'ing of unsigned compares.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-not "cmpw" } } */
/* { dg-final { scan-assembler-times "cmplw" 1 } } */

unsigned int *a, *b;

int
foo (void)
{
  if (*a == *b) return 1;
  if (*a > *b)  return 2;
  if (*a < *b)  return 3;
  if (*a != *b) return 4;
  return 0;
}
