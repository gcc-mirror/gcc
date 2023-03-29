/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-loop-optimize -fdump-tree-optimized" } */

#define PREC (__CHAR_BIT__)

int clz_complement_count1 (unsigned char b) {
    int c = 0;

    while (b) {
	b >>= 1;
	c++;
    }
    if (c <= PREC)
      return 0;
    else
      return 34567;
}

int clz_complement_count2 (unsigned char b) {
    int c = 0;

    while (b) {
	b >>= 1;
	c++;
    }
    if (c <= PREC - 1)
      return 0;
    else
      return 76543;
}

int ctz_complement_count1 (unsigned char b) {
    int c = 0;

    while (b) {
	b <<= 1;
	c++;
    }
    if (c <= PREC)
      return 0;
    else
      return 23456;
}

int ctz_complement_count2 (unsigned char b) {
    int c = 0;

    while (b) {
	b <<= 1;
	c++;
    }
    if (c <= PREC - 1)
      return 0;
    else
      return 65432;
}
/* { dg-final { scan-tree-dump-times "34567" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "76543" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "23456" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "65432" 1 "optimized" } } */
