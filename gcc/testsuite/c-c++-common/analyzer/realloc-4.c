#include "../../gcc.dg/analyzer/analyzer-decls.h"

typedef __SIZE_TYPE__ size_t;


extern void *calloc (size_t __nmemb, size_t __size)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__malloc__))
  __attribute__ ((__alloc_size__ (1, 2))) ;
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

/* realloc where we don't know the original size of the region.  */

char *test_8 (char *p, size_t sz)
{
  char *q;

  __analyzer_dump_capacity (p); /* { dg-warning "capacity: 'UNKNOWN\\(\[^\n\r\]*\\)'" } */

  p[0] = 'a';
  p[1] = 'b';
  p[2] = 'c';
  __analyzer_eval (p[0] == 'a'); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[1] == 'b'); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[2] == 'c'); /* { dg-warning "TRUE" } */

  q = (char *) realloc (p, 6);

  /* We should have 3 nodes, corresponding to "failure",
     "success without moving", and "success with moving".  */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "3 processed enodes" } */
  
  if (q)
    {
      q[3] = 'd';
      q[4] = 'e';
      q[5] = 'f';
      if (q == p)
	{
	  /* "realloc" success, growing the buffer in-place.  */
	  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
	  __analyzer_dump_capacity (q); /* { dg-warning "capacity: '\\(\[^\n\r\]*\\)6'" } */
	  __analyzer_eval (q[0] == 'a'); /* { dg-warning "TRUE" } */
	  __analyzer_eval (q[1] == 'b'); /* { dg-warning "TRUE" } */
	  __analyzer_eval (q[2] == 'c'); /* { dg-warning "TRUE" } */
	}
      else
	{
	  /* "realloc" success, moving the buffer (and thus freeing "p").  */
	  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
	  __analyzer_dump_capacity (q); /* { dg-warning "capacity: '\\(\[^\n\r\]*\\)6'" } */
	  /* We don't know how much of the buffer is copied.  */
	  __analyzer_eval (q[0] == 'a'); /* { dg-warning "UNKNOWN" } */
	  __analyzer_eval (q[1] == 'b'); /* { dg-warning "UNKNOWN" } */
	  __analyzer_eval (q[2] == 'c'); /* { dg-warning "UNKNOWN" } */
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
      __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
      __analyzer_dump_capacity (q); /* { dg-warning "capacity: 'UNKNOWN\\(\[^\n\r\]*\\)'" } */
      __analyzer_eval (p[0] == 'a'); /* { dg-warning "TRUE" } */
      __analyzer_eval (p[1] == 'b'); /* { dg-warning "TRUE" } */
      __analyzer_eval (p[2] == 'c'); /* { dg-warning "TRUE" } */
      return p;
    }

  return q;
}
