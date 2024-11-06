/* { dg-do compile } */
/* Disable phiopt as that will optimize away the `?:`,
   want to test simplify-rtx */
/* { dg-options "-O2 -fno-ssa-phiopt" } */

/* PR rtl-optimization/58195 */

int a2(int input)
{
  if (input == 0)
    return 0;
  return -input;
}
int a1(int input)
{
    int t = -input;
    return input == 0 ? 0 : t;
}

int a(int input)
{
    int value = 0;
    for(int n = input; n != 0; ++n)
        ++value;
    return value;
}

/* There should be no comparison against 0 here,  */
/* { dg-final { scan-assembler-not "cmp\t" } } */
/* { dg-final { scan-assembler "\tneg\t" } } */

