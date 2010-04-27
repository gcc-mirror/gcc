// PR c++/43790
// { dg-options "-std=c++0x" }

struct A
{
  int f();
};

int main()
{
  A a;
  auto l = [] () { return a.f(); }; // { dg-error "not captured|return" }
}
