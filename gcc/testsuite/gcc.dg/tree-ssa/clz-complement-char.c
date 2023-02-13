/* { dg-do run } */
/* { dg-require-effective-target clz } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#define PREC (__CHAR_BIT__)

int
__attribute__ ((noinline, noclone))
foo (unsigned char b) {
    int c = 0;

    while (b) {
	b >>= 1;
	c++;
    }

    return c;
}

int main()
{
  if (foo(0) != 0)
    __builtin_abort ();
  if (foo(5) != 3)
    __builtin_abort ();
  if (foo(255) != 8)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "__builtin_clz|\\.CLZ" 1 "optimized" } } */
