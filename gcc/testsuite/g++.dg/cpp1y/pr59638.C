// PR c++/59638
// { dg-do compile { target c++1y } }
// { dg-options "" }
// { dg-excess-errors "sorry" }

void (*a)(auto);         // { dg-error "" "" { xfail *-*-* } }

void (*b)(auto) = 0;     // { dg-error "" "" { xfail *-*-* } }

typedef void (*f)(auto); // { dg-error "template declaration" }

struct A
{
  int i;
};
