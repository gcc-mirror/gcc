// PR c++/69277 - [6 Regression] ICE mangling a flexible array member
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-error=pedantic" }

struct A {
  int n;
  char a[];   // { dg-warning "forbids flexible array member" }
};

// Declare but do not define function templates.
template <class T>
void foo ();

template <typename T>
void fooref (T&);

// Rvalue references are a C++ 11 feature.
template <typename T>
void foorefref (T&&);

void bar (A a)
{
  // Decltype is also a C++ 11 feature.
  // Verify that decltype gets the right type and that foo is
  // mangled correctly.
  foo<decltype (a.a)>();

  // Verify that function templates taking a reference and an rvalue
  // references (as in PR c++/69277) are also mangled correctly.
  fooref (a.a);
  foorefref (a.a);
}

// In G++ versions prior to 6, flexible array members were incorrectly
// mangled as arrays of zero elements.  Verify that flexible array
// members are mangled correctly as arrays of an unspecified number
// of elements.

// void foo<char []>():
// { dg-final { scan-assembler _Z3fooIA_cEvv } }

// The following is derived from PR c++/69277:
// void fooref<char []>(char (&) [])
// { dg-final { scan-assembler _Z6foorefIA_cEvRT_ } }

// void foorefref<char (&) []>(char (&) [])
// { dg-final { scan-assembler _Z9foorefrefIRA_cEvOT_ } }
