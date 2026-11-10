// { dg-do compile { target c++20 } }
// { dg-additional-options -fdiagnostics-all-candidates }

template <class T>
struct A {
  void f();					// { dg-message "discards qualifiers" }
  void f() const requires (__is_same (T, int)); // { dg-message "'char' is not the same as 'int'" }
};

int main()
{
  const A<char> a;
  a.f();			// { dg-error "no match" }
}
