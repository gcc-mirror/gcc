// PR c++/40381
// { dg-do compile { target c++11 } }

struct A
{
  template<typename T> void foo(T) = delete; // { dg-error "previously|declared" }
};

template<typename T> void A::foo(T) {} // { dg-error "redefinition" }

void bar()
{
  A().foo(0);			// { dg-error "use" }
}
