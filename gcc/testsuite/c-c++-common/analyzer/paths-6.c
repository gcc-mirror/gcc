/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>
#include "analyzer-decls.h"

/* Verify that ordering of writes doesn't matter when merging states.  */

/* Test with locals.  */

void test_1 (int flag)
{
  int a, b;
  if (flag)
    {
      a = 3;
      b = 4;
    }
  else
    {
      b = 4;
      a = 3;
    }

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
  __analyzer_eval (a == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (b == 4); /* { dg-warning "TRUE" } */
}

/* Test with globals.  */

int f, g, h;
void test_2 (int flag)
{
  if (flag)
    {
      f = 3;
      g = 4;
    }
  else
    {
      g = 4;
      f = 3;
    }

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
  __analyzer_eval (f == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (g == 4); /* { dg-warning "TRUE" } */
}

/* All 6 orderings of writes to 3 globals.  */

void test_3 (int i)
{
  switch (i)
    {
    default:
    case 0:
      f = 3;
      g = 4;
      h = 5;
      break;

    case 1:
      f = 3;
      h = 5;
      g = 4;
      break;

    case 2:
      g = 4;
      f = 3;
      h = 5;
      break;

    case 3:
      g = 4;
      h = 5;
      f = 3;
      break;

    case 4:
      h = 5;
      f = 3;
      g = 4;
      break;

    case 5:
      h = 5;
      g = 4;
      f = 3;
      break;
    }

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
  __analyzer_eval (f == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (g == 4); /* { dg-warning "TRUE" } */
  __analyzer_eval (h == 5); /* { dg-warning "TRUE" } */
}

void test_4 (int flag)
{
  void *p, *q;
  if (flag)
    {
      p = malloc (256);
      q = malloc (256);
    }
  else
    {
      q = malloc (256);
      p = malloc (256);
    }
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enode" } */
  free (p);
  free (q);
}
