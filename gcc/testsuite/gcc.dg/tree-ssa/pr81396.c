/* PR tree-optimization/81396 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef unsigned long long uint64_t;

uint64_t
foo (uint64_t word)
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__ && __SIZEOF_LONG_LONG__ == 8
  const unsigned char *const ptr = (const unsigned char *) &word;
  return ((uint64_t) ptr[0]
	  | ((uint64_t) ptr[1] << 8)
	  | ((uint64_t) ptr[2] << (8 * 2))
	  | ((uint64_t) ptr[3] << (8 * 3))
	  | ((uint64_t) ptr[4] << (8 * 4))
	  | ((uint64_t) ptr[5] << (8 * 5))
	  | ((uint64_t) ptr[6] << (8 * 6))
	  | ((uint64_t) ptr[7] << (8 * 7)));
#else
  return word;
#endif
}

/* { dg-final { scan-tree-dump "return word_\[0-9]*\\(D\\);" "optimized" } } */
