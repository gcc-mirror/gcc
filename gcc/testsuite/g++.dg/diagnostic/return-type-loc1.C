// { dg-do compile { target c++11 } }
// { dg-additional-options -Waggregate-return }

struct B { int i,j; };

template <class T>
struct A
{
  template <class U>
  U				// { dg-warning "aggregate" }
  f() { return {}; }
};

int main()
{
  A<int>().f<B>();		// { dg-warning "aggregate" }
}

B				// { dg-warning "aggregate" }
g() { return {}; }
