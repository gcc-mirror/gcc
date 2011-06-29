/* { dg-do compile } */
/* { dg-options -Wtraditional-conversion } */

int
test_s (signed int x)
{
  return __builtin_abs (x)	/* { dg-bogus "as unsigned due to prototype" } */
    + __builtin_clz (x)		/* { dg-warning "as unsigned due to prototype" } */
    + __builtin_ctz (x)		/* { dg-warning "as unsigned due to prototype" } */
    + __builtin_clrsb (x)	/* { dg-bogus "as unsigned due to prototype" } */
    + __builtin_ffs (x)		/* { dg-bogus "as unsigned due to prototype" } */
    + __builtin_parity (x)	/* { dg-warning "as unsigned due to prototype" } */
    + __builtin_popcount (x);	/* { dg-warning "as unsigned due to prototype" } */
}

int
test_u (unsigned int x)
{
  return __builtin_abs (x)	/* { dg-warning "as signed due to prototype" } */
    + __builtin_clz (x)		/* { dg-bogus "as signed due to prototype" } */
    + __builtin_ctz (x)		/* { dg-bogus "as signed due to prototype" } */
    + __builtin_clrsb (x)	/* { dg-warning "as signed due to prototype" } */
    + __builtin_ffs (x)		/* { dg-warning "as signed due to prototype" } */
    + __builtin_parity (x)	/* { dg-bogus "as signed due to prototype" } */
    + __builtin_popcount (x);	/* { dg-bogus "as signed due to prototype" } */
}

int
test_sl (signed long x)
{
  return __builtin_labs (x)	/* { dg-bogus "as unsigned due to prototype" } */
    + __builtin_clzl (x)	/* { dg-warning "as unsigned due to prototype" } */
    + __builtin_ctzl (x)	/* { dg-warning "as unsigned due to prototype" } */
    + __builtin_clrsbl (x)	/* { dg-bogus "as unsigned due to prototype" } */
    + __builtin_ffsl (x)	/* { dg-bogus "as unsigned due to prototype" } */
    + __builtin_parityl (x)	/* { dg-warning "as unsigned due to prototype" } */
    + __builtin_popcountl (x);	/* { dg-warning "as unsigned due to prototype" } */
}

int
test_ul (unsigned long x)
{
  return __builtin_labs (x)	/* { dg-warning "as signed due to prototype" } */
    + __builtin_clzl (x)	/* { dg-bogus "as signed due to prototype" } */
    + __builtin_ctzl (x)	/* { dg-bogus "as signed due to prototype" } */
    + __builtin_clrsbl (x)	/* { dg-warning "as signed due to prototype" } */
    + __builtin_ffsl (x)	/* { dg-warning "as signed due to prototype" } */
    + __builtin_parityl (x)	/* { dg-bogus "as signed due to prototype" } */
    + __builtin_popcountl (x);	/* { dg-bogus "as signed due to prototype" } */
}

int
test_sll (signed long long x)
{
  return __builtin_llabs (x)	/* { dg-bogus "as unsigned due to prototype" } */
    + __builtin_clzll (x)	/* { dg-warning "as unsigned due to prototype" } */
    + __builtin_ctzll (x)	/* { dg-warning "as unsigned due to prototype" } */
    + __builtin_clrsbll (x)	/* { dg-bogus "as unsigned due to prototype" } */
    + __builtin_ffsll (x)	/* { dg-bogus "as unsigned due to prototype" } */
    + __builtin_parityll (x)	/* { dg-warning "as unsigned due to prototype" } */
    + __builtin_popcountll (x);	/* { dg-warning "as unsigned due to prototype" } */
}

int
test_ull (unsigned long long x)
{
  return __builtin_llabs (x)	/* { dg-warning "as signed due to prototype" } */
    + __builtin_clzll (x)	/* { dg-bogus "as signed due to prototype" } */
    + __builtin_ctzll (x)	/* { dg-bogus "as signed due to prototype" } */
    + __builtin_clrsbll (x)	/* { dg-warning "as signed due to prototype" } */
    + __builtin_ffsll (x)	/* { dg-warning "as signed due to prototype" } */
    + __builtin_parityll (x)	/* { dg-bogus "as signed due to prototype" } */
    + __builtin_popcountll (x);	/* { dg-bogus "as signed due to prototype" } */
}
