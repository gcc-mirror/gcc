/* { dg-options "-Ofast -mdejagnu-cpu=power9 " } */

int
compare_exponents_unordered (double exponent1, double exponent2)
{
  return __builtin_vec_scalar_cmp_exp_unordered (exponent1, exponent2);
}
