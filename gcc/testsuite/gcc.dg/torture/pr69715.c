/* { dg-do compile } */

struct __attribute__((may_alias)) S { long long low; int high; };
struct S foo (void);
long double
bar (void)
{
  long double a;
  *(struct S *)&a = foo ();
  return a;
}
