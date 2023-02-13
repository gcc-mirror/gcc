/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ch" } */

/* This test case is based on gcc.dg/tree-ssa/clz-char.c. */

#define PREC (sizeof(short) * 8)

int
__attribute__ ((noinline, noclone))
foo (unsigned short b) {
    int c = 0;

    if (b == 0)
      return PREC;

    while (!(b & (1 << (PREC - 1)))) {
	b <<= 1;
	c++;
    }

    return c;
}

/* { dg-final { scan-assembler "lmbd\\tr\[012\]\[0-9\]?.w\[0-2\], r\[012\]\[0-9\]?.w\[0-2\], 1" } } */
