// PR c++/39054

struct A {};

template<typename> void foo()
{ 
  A().~int();			// { dg-error "expected" }
}
