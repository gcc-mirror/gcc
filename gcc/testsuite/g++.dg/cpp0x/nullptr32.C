// PR c++/63942
// A mangling alias for the first constructor was conflicting with the second.
// { dg-do compile { target c++11 } }
// { dg-options "-fno-inline" }

int i;
template <class T> struct A
{
  A(const T&) { i = 42; }
  A(const A&) { i = 36; }
};

typedef A<decltype(nullptr)> An;

int main()
{
  An a (nullptr);
  if (i != 42) __builtin_abort();
  An a2 (a);
  if (i != 36) __builtin_abort();
}
