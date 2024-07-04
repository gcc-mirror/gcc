/* { dg-additional-options "-fanalyzer-checker=pattern-test" } */

#include <stdlib.h>

extern void foo(void *);
extern void bar(void *);

void test1(void *ptr)
{
  if (ptr) { /* { dg-warning "pattern match on 'ptr != 0'" "ptr != 0" } */
  /* { dg-warning "pattern match on 'ptr == 0'" "ptr == 0" { target *-*-* } .-1 } */
    foo(ptr);
  } else {
    bar(ptr);
  }
}

void test_2 (void *p, void *q)
{
  if (p == NULL || q == NULL) /* { dg-line cond_2 }  */
    return;
  foo(p);

  /* { dg-warning "pattern match on 'p == 0'" "p == 0" { target *-*-* } cond_2 } */
  /* { dg-warning "pattern match on 'q == 0'" "q == 0" { target *-*-* } cond_2 } */
  /* { dg-warning "pattern match on 'p != 0'" "p != 0" { target *-*-* } cond_2 } */
  /* { dg-warning "pattern match on 'q != 0'" "q != 0" { target *-*-* } cond_2 } */
}
