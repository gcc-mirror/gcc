// PR c++/43790
// { dg-do compile { target c++11 } }

struct A
{
  int f();
};

int main()
{
  A a;
  auto l = [] () { return a.f(); }; // { dg-error "not captured|return" }
}
