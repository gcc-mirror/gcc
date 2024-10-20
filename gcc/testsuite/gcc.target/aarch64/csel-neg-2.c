/* { dg-do compile } */
/* Disable phiopt as that will optimize away the `?:`,
   want to test simplify-rtx */
/* { dg-options "-O2 -ffast-math -fno-ssa-phiopt" } */

/* PR rtl-optimization/58195 */

float a2(float input)
{
  if (input == 0)
    return 0;
  return -input;
}

/* There should be no comparison against 0 here,  */
/* { dg-final { scan-assembler-not "\tfcmp\t" } } */
/* { dg-final { scan-assembler-not "\tfcsel\t" } } */
/* { dg-final { scan-assembler "\tfneg\t" } } */

