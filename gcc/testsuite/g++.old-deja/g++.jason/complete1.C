// PRMS Id: 4695
// Bug: g++ wrongly requires A to be complete here.
// Build don't link:

struct A;

void foo(const A &);

void bar(A *p)
{
  foo(*p);
}
