// Bug: g++ fails to recognize that the template matches the target type.
// Build don't link:

template <class T> void foo (T *, int);

struct A;
void bar ()
{
  void (*p)(A *, int) = &foo;
}
