#include "analyzer-decls.h"

typedef int __attribute__((__vector_size__ (32))) V;

typedef __SIZE_TYPE__ size_t;

static size_t __attribute__((noinline))
call_strlen (const char *p)
{
  return __builtin_strlen (p);
}

void
foo (void *out)
{
  V v = (V) { };
  __analyzer_eval (call_strlen ((const char *)&v) == 0); /* { dg-warning "TRUE" } */
}
