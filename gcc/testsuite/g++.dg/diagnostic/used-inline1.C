template <class T> struct A {
  friend void foo(A*) { }	// { dg-bogus "never defined" }
  void bar() {
    baz(this);			// { dg-error "baz" }
    foo(this);
  }
};

int main()
{
  A<int>().bar();
}
