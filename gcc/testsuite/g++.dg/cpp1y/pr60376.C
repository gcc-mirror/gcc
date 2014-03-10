// PR c++/60376
// { dg-do compile { target c++1y } }

struct A
{
  int foo();
};

template<typename> void bar()
{
  using (A().foo);  // { dg-error "expected" }
}
