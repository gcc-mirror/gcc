// Build don't link:
// GROUPS passed templates
template <class T, class U>
void foo(T t, U u) {}

void bar()
{
  (void (*)(double, int)) &foo<double>;
}
