/* { dg-do compile } */
/* { dg-require-effective-target popcountl } */
/* { dg-options "-O3 -fdump-tree-optimized -fno-tree-ch" } */

extern int foo (int);

int PopCount (long b) {
    int c = 0;
    b++;

    while (b) {
	b &= b - 1;
	c++;
    }
    return c;
}
int PopCount2 (long b) {
    int c = 0;

    while (b) {
	b &= b - 1;
	c++;
    }
    foo (c);
    return foo (c);
}

void PopCount3 (long b1) {

    for (long i = 0; i < b1; ++i)
      {
	long b = i;
	int c = 0;
	while (b) {
	    b &= b - 1;
	    c++;
	}
	foo (c);
      }
}

/* { dg-final { scan-tree-dump-times "__builtin_popcount|\\.POPCOUNT" 3 "optimized" } } */
