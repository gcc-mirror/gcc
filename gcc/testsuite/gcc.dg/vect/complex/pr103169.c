/* { dg-do compile { target { vect_double } } } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-additional-options "-O2 -fvect-cost-model=unlimited" } */

_Complex double b_0, c_0;

void
mul270snd (void)
{
  c_0 = b_0 * 1.0iF * 1.0iF;
}

