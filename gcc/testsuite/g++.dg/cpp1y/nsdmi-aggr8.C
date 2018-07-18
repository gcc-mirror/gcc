// PR c++/80095
// { dg-do compile { target c++14 } }

struct A
{
  void* p = this;
};

void
foo ()
{
  const A& a = A{};
  A&& a2 = A{};
  const A& a3{};
  A&& a4{};
}
