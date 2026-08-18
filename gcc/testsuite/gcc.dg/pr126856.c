/* { dg-do run } */
/* { dg-options "-O2" } */
__attribute__((noipa)) static int
mpfr_scale2_bad (int exp)
{
    if (-1073 > exp || exp > 1025)
      __builtin_unreachable();
    if (exp < -1021)
      __builtin_abort();
    return exp;
}
int main ()
{
    if (mpfr_scale2_bad(499) != 499)
    __builtin_abort();
}
