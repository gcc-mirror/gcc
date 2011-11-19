/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fdump-tree-tmmark" } */

#include <stdlib.h>

char *z;

void foobar(void)
{
    char *p, *q;
    __transaction_atomic {
	p = (char *)malloc(123);
	q = (char *)calloc(555,1);
	free(q);
	free(p);
    }
    z = (char *)malloc (666);
}

/* { dg-final { scan-tree-dump-times " malloc .666" 1 "tmmark" } } */
/* { dg-final { scan-tree-dump-times "__builtin__ITM_malloc" 1 "tmmark" } } */
/* { dg-final { scan-tree-dump-times "__builtin__ITM_calloc" 1 "tmmark" } } */
/* { dg-final { scan-tree-dump-times "__builtin__ITM_free" 2 "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmmark" } } */
