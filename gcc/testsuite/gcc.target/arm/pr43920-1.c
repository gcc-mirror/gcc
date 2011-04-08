/* { dg-do compile } */
/* { dg-options "-mthumb -Os" }  */
/* { dg-require-effective-target arm_thumb2_ok } */

int
f (int start, int end, int *start_)
{
  if (start == -1 || end == -1)
    return -1;

  if (end - start)
    return -1;

  *start_ = start;

  return 0;
}

/* { dg-final { scan-assembler-times "\torr" 0 } } */
/* { dg-final { scan-assembler-times "\tit\t" 0 } } */
/* { dg-final { scan-assembler "\tbeq" } } */
