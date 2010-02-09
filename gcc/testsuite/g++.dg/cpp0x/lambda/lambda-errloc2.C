// PR c++/42399
// { dg-options "-std=c++0x" }

struct A {
  A();
  A(const A&) = delete;		// { dg-error "deleted function" }
};

template <class T>
void f()
{
  T t;
  [t] { return 0; };		// { dg-error "used here" }
}

int main()
{
  f<A>();			// { dg-message "instantiated" }
}
