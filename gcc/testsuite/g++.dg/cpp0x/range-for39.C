// PR c++/119073
// { dg-do compile { target c++11 } }

struct A { ~A (); };
struct B { B (const A &a = A ()); int *begin (); int *end (); ~B (); };

void
foo (bool x)
{
  for (auto i : (x ? B{} : B{}))
    ;
}
