/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-Walloca-larger-than=2000 -O2" } */

// Test when the conditionals are incorrectly reversed.

void f (void *);
void foo (__SIZE_TYPE__ len)
{
  void *p;
  if (len < 500)
    p = __builtin_malloc (len);
  else
    p = __builtin_alloca (len); // { dg-warning "argument to .alloca. may be too large" }
  f (p);
}

void bar (__SIZE_TYPE__ len)
{
  void *p;
  if (len > 500)
    p = __builtin_alloca (len); // { dg-warning "argument to .alloca. may be too large" }
  else
    p = __builtin_malloc (len);
  f (p);
}
