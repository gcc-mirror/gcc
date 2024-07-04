/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-loop-optimize -fdump-tree-optimized" } */

#define PREC (__CHAR_BIT__)

__INT32_TYPE__ count1 (unsigned char b) {
    int c = 0;

    while (b) {
	b &= b - 1;
	c++;
    }
    if (c <= PREC)
      return 0;
    else
      return 34567;
}

__INT32_TYPE__ count2 (unsigned char b) {
    int c = 0;

    while (b) {
	b &= b - 1;
	c++;
    }
    if (c <= PREC - 1)
      return 0;
    else
      return 76543;
}

/* { dg-final { scan-tree-dump-times "34567" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "76543" 1 "optimized" } } */
