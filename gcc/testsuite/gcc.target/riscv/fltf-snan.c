/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-fno-finite-math-only -ftrapping-math -fsignaling-nans" } */

long
fltf (float x, float y)
{
  return __builtin_isless (x, y);
}

/* { dg-final { scan-assembler "\tfrflags\t(\[^\n\]*)\n\tflt\\.s\t\[^,\]*,(\[^,\]*),(\[^,\]*)\n\tfsflags\t\\1\n\tfeq\\.s\tzero,\\2,\\3\n" } } */
/* { dg-final { scan-assembler-not "snez" } } */
