/* { dg-require-weak "" } */
/* { dg-require-alias "" } */
#define weak_alias(func, aliasname) \
	extern __typeof (func) aliasname __attribute__ ((weak, alias (#func)));

#define add3(d, m, c) ((d) + (m) + (c))

int
__add3(int d, int m, int c)
{
  return d + m + c;
}

weak_alias (__add3, add3)
