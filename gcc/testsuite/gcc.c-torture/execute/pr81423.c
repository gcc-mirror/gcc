/* PR rtl-optimization/81423 */

extern void abort (void);

unsigned long long int ll = 0;
unsigned long long int ull1 = 1ULL;
unsigned long long int ull2 = 12008284144813806346ULL;
unsigned long long int ull3;

unsigned long long int __attribute__ ((noinline))
foo (void)
{
  ll = -5597998501375493990LL;

  ll = (unsigned int) (5677365550390624949LL - ll) - (ull1 > 0);
  unsigned long long int ull3;
  ull3 = (unsigned int)
    (2067854353LL <<
     (((ll + -2129105131LL) ^ 10280750144413668236ULL) -
      10280750143997242009ULL)) >> ((2873442921854271231ULL | ull2)
				    - 12098357307243495419ULL);

  return ull3;
}

int
main (void)
{
  /* We need a long long of exactly 64 bits and int of exactly 32 bits
     for this test.  */
  if (__SIZEOF_LONG_LONG__ * __CHAR_BIT__ != 64
      || __SIZEOF_INT__ * __CHAR_BIT__ != 32)
    return 0;

  ull3 = foo ();
  if (ull3 != 3998784)
    abort ();
  return 0;
}
