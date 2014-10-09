// PR c++/60376
// { dg-do compile { target c++14 } }

struct A
{
  int foo();
};

template<typename> void bar()
{
  using (A().foo);  // { dg-error "expected" }
}
