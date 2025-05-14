/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */
/* { dg-additional-options "-fno-exceptions" } */

#include <stdlib.h>
#include "analyzer-decls.h"

extern int foo (int);

int test (int flag, void *ptr, int *p, int n)
{
  int result;
  int sum = 0;
  int i;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  if (flag)
    free (ptr);

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  for (i = 0; i < n; i++)
    p[i] = i;
  for (i = 0; i < n; i++)
    sum += foo (p[i]); /* { dg-bogus "uninitialized" } */
  result = sum;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  if (flag)
    free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
  return result;
}

int test_2 (int flag, int *p, int n)
{
  int result;
  int sum = 0;
  int i;

  void *ptr = malloc (16);

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  if (flag)
    free (ptr);

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  for (i = 0; i < n; i++)
    p[i] = i;
  for (i = 0; i < n; i++)
    sum += foo (p[i]); /* { dg-bogus "uninitialized" } */
  result = sum;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
  return result;
}
