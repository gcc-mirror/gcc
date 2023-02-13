/* { dg-do run } */
/* { dg-require-effective-target clzl } */
/* { dg-options "-O2 -fno-tree-ch -fdump-tree-optimized" } */

#define PREC (__CHAR_BIT__)

int
__attribute__ ((noinline, noclone))
foo (unsigned char b) {
    int c = 0;

    if (b == 0)
      return PREC;

    while (!(b & (1 << (PREC - 1)))) {
	b <<= 1;
	c++;
    }

    return c;
}

int main()
{
  if (foo(0) != PREC)
    __builtin_abort ();
  if (foo(1 << (PREC - 1)) != 0)
    __builtin_abort ();
  if (foo(35) != PREC - 6)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "__builtin_clz|\\.CLZ" 1 "optimized" } } */
