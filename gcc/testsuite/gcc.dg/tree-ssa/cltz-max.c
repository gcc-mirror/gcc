/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-loop-optimize -fdump-tree-optimized" } */

#define PREC (__CHAR_BIT__)

#if __SIZEOF_INT__ < 4
#define int __INT32_TYPE__
#endif

int clz_count1 (unsigned char b) {
    int c = 0;

    if (b == 0)
      return 0;

    while (!(b & (1 << (PREC - 1)))) {
	b <<= 1;
	c++;
    }
    if (c <= PREC - 1)
      return 0;
    else
      return 34567;
}

int clz_count2 (unsigned char b) {
    int c = 0;

    if (b == 0)
      return 0;

    while (!(b & (1 << PREC - 1))) {
	b <<= 1;
	c++;
    }
    if (c <= PREC - 2)
      return 0;
    else
      return 76543;
}

int ctz_count1 (unsigned char b) {
    int c = 0;

    if (b == 0)
      return 0;

    while (!(b & 1)) {
	b >>= 1;
	c++;
    }
    if (c <= PREC - 1)
      return 0;
    else
      return 23456;
}

int ctz_count2 (unsigned char b) {
    int c = 0;

    if (b == 0)
      return 0;

    while (!(b & 1)) {
	b >>= 1;
	c++;
    }
    if (c <= PREC - 2)
      return 0;
    else
      return 65432;
}
/* { dg-final { scan-tree-dump-times "34567" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "76543" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "23456" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "65432" 1 "optimized" } } */
