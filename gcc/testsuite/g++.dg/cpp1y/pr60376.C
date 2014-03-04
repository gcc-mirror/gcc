// PR c++/60376
// { dg-options -std=c++1y }

struct A
{
  int foo();
};

template<typename> void bar()
{
  using (A().foo);  // { dg-error "expected" }
}
