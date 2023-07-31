/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O1 -fdump-rtl-combine-details" } */
/* { dg-final { scan-rtl-dump "narrow comparison from mode DI to HI" "combine" } } */

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
