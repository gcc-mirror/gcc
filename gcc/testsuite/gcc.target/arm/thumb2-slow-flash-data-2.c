/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v7em_hard_ok } */
/* { dg-skip-if "-mslow-flash-data and -mword-relocations incompatible" { *-*-* } { "-mword-relocations" } } */
/* { dg-options "-O2 -mslow-flash-data" } */
/* { dg-add-options arm_arch_v7em_hard } */

float f (float);

const float max = 0.01f;

int
g (float in)
{
  if (f (in) + f (in) < max)
    return 0;
  return 1;
}

double foo (void)
{
  return 0xF1EC7A5239123AF;
}

double bar (void)
{
  return 0.0f;
}
