// PR c++/84449
// { dg-do compile { target c++11 } }

struct A
{
  constexpr A (int) {}
  ~A () = delete;
};

struct B
{
  A a;
  constexpr B () : a (0) {}	// { dg-error "use of deleted function" }
};
