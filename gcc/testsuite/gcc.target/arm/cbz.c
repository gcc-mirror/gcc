/* { dg-do compile {target { arm_thumb2 || arm_thumb1_cbz_ok } } } */
/* { dg-options "-O2" } */

int
foo (int a, int *b)
{
  if (a)
    *b = 1;
  return 0;
}

/* { dg-final { scan-assembler-times "cbz\\tr\\d" 1 } } */
