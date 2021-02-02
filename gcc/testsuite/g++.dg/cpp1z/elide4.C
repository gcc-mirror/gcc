// { dg-do compile { target c++11 } }

// Check that there's a call to some base constructor of B: either the default
// constructor, if the copy is elided, or the copy constructor.

// { dg-final { scan-assembler {call[ \t]*_?_ZN1BC2} { target { i?86-*-* x86_64-*-* } } } }

int count;
struct A { int i = count++; };
struct B: virtual A {
  B() { }
  B(const B& b);
};
bool x;
struct C: B
{
  C() : B(x ? (0,B()) : B()) { }
};

int main()
{
  C c;
  return count;
}
