// { dg-do assemble  }
// Bug: g++ fails to recognize that the template matches the target type.

template <class T> void foo (T *, int);

struct A;
void bar ()
{
  void (*p)(A *, int) = &foo;
}
