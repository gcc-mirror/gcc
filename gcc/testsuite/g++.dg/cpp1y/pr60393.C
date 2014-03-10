// PR c++/60393
// { dg-options -std=c++1y }

void (*f)(auto) + 0; // { dg-error "expected" }

struct A
{
  int i;
};
