// Test that the error message mentions the template arguments.

struct A
{
  template <class T> void f(T);
  void f(int);
};

int main()
{
  A().f<1>(0);			// { dg-error "f<1>" }
  // { dg-error "type/value mismatch at argument 1" "" { target *-*-* } .-1 }
  // { dg-message "expected a type, got .1." "" { target *-*-* } .-2 }
}
