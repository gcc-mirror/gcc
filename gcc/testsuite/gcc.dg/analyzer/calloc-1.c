#include "analyzer-decls.h"

typedef __SIZE_TYPE__ size_t;

#define NULL ((void *)0)

extern void *calloc (size_t __nmemb, size_t __size)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__malloc__))
  __attribute__ ((__alloc_size__ (1, 2))) ;

char *test_1 (size_t sz)
{
  char *p;

  p = calloc (1, 3);
  if (!p)
    return NULL;

  __analyzer_dump_capacity (p); /* { dg-warning "capacity: '\\(\[^\n\r\]*\\)3'" } */

  __analyzer_eval (p[0] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[1] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[2] == 0); /* { dg-warning "TRUE" } */

  return p;
}
