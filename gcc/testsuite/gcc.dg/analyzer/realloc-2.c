/* { dg-additional-options "-fno-analyzer-suppress-followups" } */

#include "analyzer-decls.h"

typedef __SIZE_TYPE__ size_t;

#define NULL ((void *)0)

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

char *test_8 (size_t sz)
{
  char *p, *q;

  p = malloc (3);
  if (!p)
    return NULL;

  __analyzer_dump_capacity (p); /* { dg-warning "capacity: '\\(size_t\\)3'" } */

  p[0] = 'a';
  p[1] = 'b';
  p[2] = 'c';

  __analyzer_eval (p[0] == 'a'); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[1] == 'b'); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[2] == 'c'); /* { dg-warning "TRUE" } */

  q = realloc (p, 6);

  /* We should have 3 nodes, corresponding to "failure",
     "success without moving", and "success with moving".  */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "3 processed enodes" } */
  
  if (q)
    {
      __analyzer_dump_capacity (q); /* { dg-warning "capacity: '\\(size_t\\)6'" } */
      q[3] = 'd';
      q[4] = 'e';
      q[5] = 'f';
      if (q == p)
	{
	  /* "realloc" success, growing the buffer in-place.  */
	  __analyzer_eval (p[0] == 'a'); /* { dg-warning "TRUE" } */
	  __analyzer_eval (p[1] == 'b'); /* { dg-warning "TRUE" } */
	  __analyzer_eval (p[2] == 'c'); /* { dg-warning "TRUE" } */
	  // TODO
	}
      else
	{
	  /* "realloc" success, moving the buffer (and thus freeing "p").  */
	  __analyzer_eval (q[0] == 'a'); /* { dg-warning "TRUE" } */
	  __analyzer_eval (q[1] == 'b'); /* { dg-warning "TRUE" } */
	  __analyzer_eval (q[2] == 'c'); /* { dg-warning "TRUE" } */
	  __analyzer_eval (p[0] == 'a'); /* { dg-warning "UNKNOWN" "unknown" } */
	  /* { dg-warning "use after 'free' of 'p'" "use after free" { target *-*-* } .-1 } */
	}
      __analyzer_eval (q[3] == 'd'); /* { dg-warning "TRUE" } */
      __analyzer_eval (q[4] == 'e'); /* { dg-warning "TRUE" } */
      __analyzer_eval (q[5] == 'f'); /* { dg-warning "TRUE" } */
    }
  else
    {
      /* "realloc" failure.  p should be unchanged.  */
      __analyzer_dump_capacity (p); /* { dg-warning "capacity: '\\(size_t\\)3'" } */
      __analyzer_eval (p[0] == 'a'); /* { dg-warning "TRUE" } */
      __analyzer_eval (p[1] == 'b'); /* { dg-warning "TRUE" } */
      __analyzer_eval (p[2] == 'c'); /* { dg-warning "TRUE" } */
      return p;
    }

  return q;
}
