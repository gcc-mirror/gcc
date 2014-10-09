// PR c++/59638
// { dg-do compile { target c++14 } }
// { dg-options "" }

void (*a)(auto);         // { dg-error "" }

void (*b)(auto) = 0;     // { dg-error "" }

typedef void (*f)(auto); // { dg-error "" }

struct A
{
  int i;
};
