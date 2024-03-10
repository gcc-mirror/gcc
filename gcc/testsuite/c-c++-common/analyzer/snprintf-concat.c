typedef __SIZE_TYPE__ size_t;
#include "../../gcc.dg/analyzer/analyzer-decls.h"

extern size_t
strlen(const char* __s) __attribute__((__nothrow__, __leaf__))
__attribute__((__pure__)) __attribute__((__nonnull__(1)));

extern void*
malloc(size_t __size) __attribute__((__nothrow__, __leaf__))
__attribute__((__malloc__)) __attribute__((__alloc_size__(1)));

extern int
snprintf(char* __restrict __s, size_t size, const char* __restrict, ...)
  __attribute__((__nothrow__));

char *
test_1 (const char *a, const char *b)
{
  size_t sz = strlen (a) + strlen (b) + 2;
  char *p = (char *) malloc (sz);
  if (!p)
    return NULL;
  snprintf (p, sz, "%s/%s", a, b);
  return p;
}

void
test_2 (const char *a, const char *b)
{
  size_t sz = strlen (a) + strlen (b) + 2;
  char *p = (char *) malloc (sz); /* { dg-message "allocated here" "PR 107017" { xfail *-*-* } } */
  if (!p)
    return;
  snprintf (p, sz, "%s/%s", a, b); /* { dg-warning "leak of 'p'" "PR 107017" { xfail *-*-* } } */
}
