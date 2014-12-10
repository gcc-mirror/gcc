/* PR rtl-optimization/52714

   Check that combine manages to remove the "stack == 0" test.
   Without ICEing.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */

int __re_compile_fastmap(unsigned char *p)
{
    unsigned char **stack;
    unsigned size;
    unsigned avail;

    stack = __builtin_alloca(5 * sizeof(unsigned char*));
    if (stack == 0)
	return -2;
    size = 5;
    avail = 0;

    for (;;) {
	switch (*p++) {
	case 0:
	    if (avail == size)
		return -2;
	    stack[avail++] = p;
	}
    }

    return 0;
}

/* { dg-final { scan-assembler-not {\mtst\.l %sp\M} } } */
