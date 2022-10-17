/* { dg-do compile } */
/* { dg-options "-O1" } */

int one_cmpl_abs(int a)
{
  return a < 0 ? ~a : a;
}

/* { dg-final { scan-assembler-not "bgez" } } */
