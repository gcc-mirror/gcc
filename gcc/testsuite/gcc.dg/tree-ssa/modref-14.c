/* { dg-do compile } */
/* { dg-options "-O2 -Wsuggest-attribute=const" } */
struct a {int a,b,c;};
__attribute__ ((noinline))
int init (struct a *a)
{
  a->a=1;
  a->b=2;
  a->c=3;
}
int const_fn () /* { dg-warning "function might be candidate for attribute 'const" } */
{
  struct a a;
  init (&a);
  return a.a + a.b + a.c;
}
