// PR c++/69277 - [6 Regression] ICE mangling a flexible array member
// { dg-do compile }

struct A {
  int n;
  char a [];
};

// Declare but do not define function templates.
template <typename T>
void fooref (T&);

void bar (A a)
{
  fooref (a.a);
}

// In G++ versions prior to 6, flexible array members were incorrectly
// mangled as arrays of zero elements.  Verify that flexible array
// members are mangled correctly as arrays of an unspecified number
// of elements.

// void fooref<char []>(char (&) [])
// { dg-final { scan-assembler _Z6foorefIA_cEvRT_ } }
