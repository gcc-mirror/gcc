/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

#include "../analyzer-decls.h"

#define Z_NULL 0

static void __attribute__((noinline))
test_1_callee (void *p, void *q)
{
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  __analyzer_eval (p == Z_NULL); /* { dg-warning "FALSE" } */
  __analyzer_eval (p != Z_NULL); /* { dg-warning "TRUE" } */

  __analyzer_eval (q == Z_NULL); /* { dg-warning "FALSE" } */ 
  __analyzer_eval (q != Z_NULL); /* { dg-warning "TRUE" } */ 
}

void test_1 (void *p, void *q)
{
  if (p == Z_NULL || q == Z_NULL)
    return;

  test_1_callee (p, q);
}

static void __attribute__((noinline))
test_2_callee (void *p, void *q)
{
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  __analyzer_eval (p == Z_NULL); /* { dg-warning "FALSE" } */
  __analyzer_eval (p != Z_NULL); /* { dg-warning "TRUE" } */

  __analyzer_eval (q == Z_NULL); /* { dg-warning "FALSE" } */ 
  __analyzer_eval (q != Z_NULL); /* { dg-warning "TRUE" } */ 
}

void test_2 (void *p, void *q)
{
  if (p != Z_NULL && q != Z_NULL)
    test_2_callee (p, q);
}
