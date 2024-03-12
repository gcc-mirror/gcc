/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O2 -fdump-rtl-combine-details" } */
/* { dg-final { scan-rtl-dump "narrow comparison from mode .I to QI" "combine" } } */

typedef __UINT64_TYPE__ uint64_t;

int
ge_1byte_a (uint64_t *x)
{
  return *x > 0x3fffffffffffffff;
}

int
ge_1byte_b (uint64_t *x)
{
  return *x >= 0x4000000000000000;
}
