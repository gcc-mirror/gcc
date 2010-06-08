// PR c++/44366
// While printing the operand of decltype We were trying to print f as the
// scope of t, causing infinite recursion.
// { dg-options "-std=c++0x" }

template <typename T>
void f(T t, decltype(*t))
{
  struct A { void g() {
    foo;			// { dg-error "foo" }
  } };
}
