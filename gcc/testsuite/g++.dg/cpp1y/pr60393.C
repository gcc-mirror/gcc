// PR c++/60393
// { dg-do compile { target c++1y } }
// { dg-options "" }

void (*f)(auto) + 0; // { dg-error "expected" }

struct A
{
  int i;
};
