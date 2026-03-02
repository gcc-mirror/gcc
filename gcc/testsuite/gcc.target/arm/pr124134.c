/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v7a_fp_hard_ok } */
/* { dg-add-options arm_arch_v7a_fp_hard } */
/* { dg-additional-options "-O2 -mrestrict-it" } */
double max___a, max___b;
double max() {
  if (max___a < max___b)
    return max___b;
  return max___a;
}
