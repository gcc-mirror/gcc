/* { dg-additional-options "-fno-analyzer-suppress-followups" } */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

typedef __SIZE_TYPE__ size_t;


extern void *malloc (size_t __size)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__malloc__))
  __attribute__ ((__alloc_size__ (1)));
extern void *realloc (void *__ptr, size_t __size)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__warn_unused_result__))
  __attribute__ ((__alloc_size__ (2)));
extern void free (void *__ptr)
  __attribute__ ((__nothrow__ , __leaf__));
extern void *memset (void *__ptr, int __value, size_t __size);

/* realloc where the region shrinks on success_with_move.  */

void test_1 ()
{
  char *p = (char *) malloc (16);
  if (!p)
    return;
  memset (p, 1, 16);

  char *q = (char *) realloc (p, 8);
  if (!q)
    {
      free (p);
      return;
    }
  else if (p != q)
    {
      __analyzer_dump_capacity (q); /* { dg-warning "capacity: '\\(\[^\n\r\]*\\)8'" } */
      __analyzer_eval (q[8] == 1); /* { dg-line eval } */
    
      /* { dg-warning "UNKNOWN" "warning" { target *-*-* } eval } */
      /* { dg-warning "heap-based buffer over-read" "warning" { target *-*-* } eval } */
    }

  free (q);
}
