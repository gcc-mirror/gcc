/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-tailr1" } */

typedef long unsigned int size_t;
typedef int (*compare_t)(const void *, const void *);

int partition (void *base, size_t nmemb, size_t size, compare_t cmp);

void
my_qsort (void *base, size_t nmemb, size_t size, compare_t cmp)
{
  int pt;
  if (nmemb > 1)
    {
      pt = partition (base, nmemb, size, cmp);
      my_qsort (base, pt + 1, size, cmp);
      my_qsort ((void*)((char*) base + (pt + 1) * size),
		nmemb - pt - 1, size, cmp);
    }
}

/* { dg-final { scan-tree-dump-not "cmp\[^\r\n\]*PHI" "tailr1" } } */
