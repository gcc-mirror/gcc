/* { dg-do compile { target { lp64 && { ! { sparc*-*-* hppa*-*-* } } } } } */
/* Excluding sparc since there a prior optimization already reduced the
   constant, i.e., nothing left for us.  */
/* { dg-options "-O2 -fdump-rtl-combine-details" } */
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
