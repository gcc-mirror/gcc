// Build don't link:
// GROUPS passed templates
template <class T>
void foo(T t);

template <class T>
struct S {};

int main()
{
  S<int> si;

  foo<S<int> >(si);
}
