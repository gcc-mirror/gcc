/* { dg-do compile { target { lp64 } && ! target { sparc*-*-* } } } */
/* { dg-options "-O1 -fdump-rtl-combine-details" } */
/* { dg-final { scan-rtl-dump "narrow comparison from mode .I to SI" "combine" } } */

typedef __UINT64_TYPE__ uint64_t;

int
le_4bytes_a (uint64_t *x)
{
  return *x <= 0x3ffffdffffffffff;
}

int
le_4bytes_b (uint64_t *x)
{
  return *x < 0x3ffffe0000000000;
}
