/* { dg-do compile } */
/* { dg-options "-ffinite-math-only -fno-signed-zeros -dp" } */

float
smaxf (float x, float y)
{
  return x >= y ? x : y;
}

/* { dg-final { scan-assembler-not "\ttail\tfmaxf\t" } } */
/* { dg-final { scan-assembler-not "\tfge\\.s\t" } } */
/* { dg-final { scan-assembler "\tfmax\\.s\t\[^\n\]* smaxsf3\n" } } */
