/* { dg-do compile { target { lp64 && { ! { sparc*-*-* hppa*-*-* } } } } } */
/* Excluding sparc since there we do not end up with a comparison of memory and
   a constant which means that the optimization is not applicable.  */
/* { dg-options "-O2 -fdump-rtl-combine-details" } */
/* { dg-final { scan-rtl-dump "narrow comparison from mode .I to HI" "combine" } } */

typedef __UINT64_TYPE__ uint64_t;

int
ge_2bytes_a (uint64_t *x)
{
  return *x > 0x400cffffffffffff;
}

int
ge_2bytes_b (uint64_t *x)
{
  return *x >= 0x400d000000000000;
}
