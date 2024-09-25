/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>
#include "analyzer-decls.h"

extern int foo (int);

static int __attribute__((noinline))
do_stuff_2 (int *p, int n)
{
  return 0;
}

/* As malloc-vs-local-2.c, but with a memory leak for the "on the heap case"
   by not attempting to free at the end.  */

int test_1 (int n)
{
  int buf[10];
  int *ptr;
  int result;

  if (n > 10)
    ptr = (int *)malloc (sizeof (int) * n);
  else
    ptr = buf;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  {
    int *p = ptr;
    int sum = 0;
    int i;
    for (i = 0; i < n; i++)
      p[i] = i; /* { dg-warning "dereference of possibly-NULL" } */
    for (i = 0; i < n; i++)
      sum += foo (p[i]); /* { dg-bogus "uninitialized" } */
    result = sum;
  }

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  return result; /* { dg-message "leak of 'p'|leak of 'ptr'" } */
}

/* A simpler version of the above.  */

int test_2 (int n)
{
  int buf[10];
  int *ptr;
  int result;

  if (n > 10)
    ptr = (int *)malloc (sizeof (int) * n);
  else
    ptr = buf;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  result = do_stuff_2 (ptr, n);

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  return result; /* { dg-message "leak of 'ptr'" } */
}
