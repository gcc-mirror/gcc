/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_vfp_ok } */
/* { dg-require-effective-target arm_arch_v8a_ok } */
/* { dg-options "-O2 -ffast-math" } */
/* { dg-add-options arm_v8_vfp } */
/* { dg-add-options arm_arch_v8a } */

int global;

void
lceil_float (float x, int b)
{
  if (b) global = __builtin_lceilf (x);
}

void
lceil_double (double x, int b)
{
  if (b) global = __builtin_lceil (x);
}

void
lfloor_float (float x, int b)
{
  if (b) global =  __builtin_lfloorf (x);
}

void
lfloor_double (double x, int b)
{
  if (b) global = __builtin_lfloor (x);
}

void
lround_float (float x, int b)
{
  if (b) global = __builtin_lroundf (x);
}

void
lround_double (double x, int b)
{
  if (b) global = __builtin_lround (x);
}
