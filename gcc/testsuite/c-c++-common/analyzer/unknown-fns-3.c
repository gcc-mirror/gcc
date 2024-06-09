/* Tests for handling constraints on results of unknown fns.  */

#include <stddef.h>
#include "analyzer-decls.h"

void unknown_fn (void *);

void test_1 (void)
{
  int i;
  unknown_fn (&i);
  if (i)
    __analyzer_eval (i); /* { dg-warning "TRUE" } */
  else
    __analyzer_eval (i); /* { dg-warning "FALSE" } */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}

struct foo
{
  int i;
  int j;
};

void test_2 (void)
{
  struct foo f;
  unknown_fn (&f);
  if (f.j)
    __analyzer_eval (f.j); /* { dg-warning "TRUE" } */
  else
    __analyzer_eval (f.j); /* { dg-warning "FALSE" } */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}

void test_3 (int flag)
{
  int i;
  unknown_fn (&i);
  if (i)
    {
      __analyzer_eval (i); /* { dg-warning "TRUE" } */
      if (flag)
	__analyzer_eval (flag); /* { dg-warning "TRUE" } */
      else
	__analyzer_eval (flag); /* { dg-warning "FALSE" } */
    }
  else
    __analyzer_eval (i); /* { dg-warning "FALSE" } */
  if (flag)
    __analyzer_eval (flag); /* { dg-warning "TRUE" } */
  else
    __analyzer_eval (flag); /* { dg-warning "FALSE" } */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}

void test_4 (int y)
{
  int x;
  unknown_fn (&x);
  if (x)
    {
      __analyzer_eval (x); /* { dg-warning "TRUE" } */
      x = 0;
    }
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}
