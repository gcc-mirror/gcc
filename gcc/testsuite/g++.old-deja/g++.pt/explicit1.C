// Build don't link:
// GROUPS passed templates
template <class T>
void foo(T t) {}

void bar()
{
  (void (*)(double)) &foo<double>;
}
