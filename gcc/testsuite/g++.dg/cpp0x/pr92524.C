// PR c++/92524
// { dg-do compile { target c++11 } }

struct A { char a = '*'; };
struct B { A b[64]; };

void
foo ()
{
  A a;
  B{a};
}
