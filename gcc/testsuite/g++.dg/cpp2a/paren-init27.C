// PR c++/92812
// P1975R0
// { dg-do run { target c++20 } }
// { dg-options "-Wall -Wextra" }

struct Aggr { int x; int y; };
struct Base { int i; Base(int i_) : i{i_} { } };
struct BaseAggr : Base { };
struct X { };
struct AggrSDM { static X x; int i; int j; };

int
main ()
{
  Aggr a = static_cast<Aggr>(42); // { dg-warning "missing initializer" }
  if (a.x != 42 || a.y != 0)
    __builtin_abort ();
  BaseAggr b = static_cast<BaseAggr>(42);
  if (b.i != 42)
    __builtin_abort ();
  AggrSDM s = static_cast<AggrSDM>(42); // { dg-warning "missing initializer" }
  if (s.i != 42 || s.j != 0)
    __builtin_abort ();
}
