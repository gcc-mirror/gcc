/* { dg-additional-options "-fanalyzer-checker=pattern-test -O2" } */
// TODO: run this at every optimization level

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
  _Bool tmp1 = p == 0;
  _Bool tmp2 = q == 0;
  _Bool tmp = tmp1 | tmp2;

  /* Verify that we can detect the implied conditions on p and q here.  */
  if (tmp) /* { dg-line cond_2 }  */
    return;
  foo(p);

  /* { dg-warning "pattern match on 'tmp1 == 0'" "tmp1 == 0" { target *-*-* } cond_2 } */
  /* { dg-warning "pattern match on 'tmp2 == 0'" "tmp2 == 0" { target *-*-* } cond_2 } */
  /* { dg-warning "pattern match on '<unknown> == 0'" "<unknown> == 0" { target *-*-* } cond_2 } */
  /* { dg-warning "pattern match on '<unknown> != 0'" "<unknown> != 0" { target *-*-* } cond_2 } */
  /* { dg-warning "pattern match on 'p != 0'" "p != 0" { target *-*-* } cond_2 } */
  /* { dg-warning "pattern match on 'q != 0'" "q != 0" { target *-*-* } cond_2 } */
}

void test_3 (void *p, void *q)
{
  _Bool tmp1 = p == 0;
  _Bool tmp2 = q == 0;
  _Bool tmp = tmp1 & tmp2;

  /* Verify that we can detect the implied conditions on p and q here.  */
  if (tmp) /* { dg-line cond_3 }  */
    return;
  foo(p);

  /* { dg-warning "pattern match on 'tmp1 != 0'" "tmp1 != 0" { target *-*-* } cond_3 } */
  /* { dg-warning "pattern match on 'tmp2 != 0'" "tmp2 != 0" { target *-*-* } cond_3 } */
  /* { dg-warning "pattern match on '<unknown> == 0'" "<unknown> == 0" { target *-*-* } cond_3 } */
  /* { dg-warning "pattern match on '<unknown> != 0'" "<unknown> != 0" { target *-*-* } cond_3 } */
  /* { dg-warning "pattern match on 'p == 0'" "p == 0" { target *-*-* } cond_3 } */
  /* { dg-warning "pattern match on 'q == 0'" "q == 0" { target *-*-* } cond_3 } */
}
