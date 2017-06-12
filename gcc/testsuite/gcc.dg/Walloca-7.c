/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-Walloca -O0" } */

extern void f(void *);

void foo(void)
{
  // Test that strict -Walloca works even without optimization.
  f (__builtin_alloca(500)); // { dg-warning "use of 'alloca'" }
}

void bar(void)
{
  // Test that we warn on alloca() calls, not just __builtin_alloca calls.
  extern void *alloca(__SIZE_TYPE__);
  f (alloca (123)); // { dg-warning "use of 'alloca'" }
}
