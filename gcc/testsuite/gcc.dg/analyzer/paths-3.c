/* { dg-additional-options "-fanalyzer-transitivity" } */

#include <stdlib.h>
#include "analyzer-decls.h"

int test_1 (int a, int b)
{
  void *p;

  if (a > 5)
    if (b)
      p = malloc (16);
    else
      p = malloc (32);

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "4 exploded nodes" } */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 exploded nodes" } */

  if (a > 5)
    {
      free (p);
      __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 exploded node" } */
    }

  return 0; /* { dg-bogus "leak" } */
}

int test_2 (int a, int b)
{
  void *p;

  if (a > 5)
    if (b)
      p = malloc (16);
    else
      p = malloc (32);

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "4 exploded nodes" } */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 exploded nodes" } */

  if (a > 6) /* different condition */
    {
      free (p);
      __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 exploded node" } */
    }

  return 0; /* { dg-warning "leak of 'p'" } */
  /* leaks when a == 5.  */
}
