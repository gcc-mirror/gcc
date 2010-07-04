// PR c++/44366
// While printing the operand of sizeof We were trying to print f as the
// scope of t, causing infinite recursion.

template <typename T>
void f(T t, int(*)[sizeof(t)])
{
  struct A { void g() {
    foo;			// { dg-error "foo" }
  } };
}
