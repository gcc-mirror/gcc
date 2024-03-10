/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O1 -fdump-rtl-combine-details" } */
/* { dg-final { scan-rtl-dump "narrow comparison from mode .I to HI" "combine" } } */

typedef __UINT64_TYPE__ uint64_t;

int
le_2bytes_a (uint64_t *x)
{
  return *x <= 0x3ffdffffffffffff;
}

int
le_2bytes_b (uint64_t *x)
{
  return *x < 0x3ffe000000000000;
}
