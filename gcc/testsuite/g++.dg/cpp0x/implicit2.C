// Test that the synthesized C copy constructor calls the A template
// constructor and has the appropriate exception specification.
// { dg-do run { target c++11 } }

int r = 1;

struct A
{
  A() {}
  A(const A&) throw () { }
  template <class T>
  A(T& t) { r = 0; }
};

struct B
{
  B() {}
  B(B&) throw () { }
};

struct C: A, B { };

#define SA(E) static_assert(E, #E)

C c;
SA (!noexcept(C(c)));

int main()
{
  (C(c));
  return r;
}
