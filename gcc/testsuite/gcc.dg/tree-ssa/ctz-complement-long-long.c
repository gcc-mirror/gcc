/* { dg-do run } */
/* { dg-require-effective-target ctzll } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#define PREC (__CHAR_BIT__ * __SIZEOF_LONG_LONG__)

int
__attribute__ ((noinline, noclone))
foo (unsigned long long b) {
    int c = 0;

    while (b) {
	b <<= 1;
	c++;
    }

    return c;
}

int main()
{
  if (foo(0) != 0)
    __builtin_abort ();
  if (foo(96) != PREC - 5)
    __builtin_abort ();
  if (foo(35) != PREC)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "__builtin_ctz|\\.CTZ" 1 "optimized" } } */
