/* PR tree-optimization/114760 */
/* { dg-do compile } */
/* { dg-require-effective-target clz } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

// Check that for signed type, there's no CLZ.
#define PREC (__CHAR_BIT__ * __SIZEOF_INT__)
int
no_nlz32 (int b) {
    int c = PREC;

    while (b != 0) {
	b /= 2;
	c --;
    }

    return c;
}

/* { dg-final { scan-tree-dump-not "__builtin_ctz|\\.CLZ" "optimized" } } */
