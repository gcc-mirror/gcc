// PR c++/60393
// { dg-do compile { target c++14 } }

void (*f)(auto) + 0; // { dg-error "auto|expected" }

struct A
{
  int i;
};
