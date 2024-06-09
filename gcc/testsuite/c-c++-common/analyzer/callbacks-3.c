#include "analyzer-decls.h"

typedef __SIZE_TYPE__ size_t;
typedef int (*__compar_fn_t)(const void *, const void *);
extern void qsort(void *__base, size_t __nmemb, size_t __size,
		  __compar_fn_t __compar)
  __attribute__((__nonnull__(1, 4)));

static int
test_1_callback (const void *p1, const void *p2)
{
  __analyzer_dump_path (); /* { dg-message "here" } */
  return 0;
}

void test_1_caller (int *arr, size_t n)
{
  qsort (arr, n, sizeof (int), test_1_callback);
}
