// Build don't link:
// GROUPS passed templates
template <class T>
void foo(T t);

int main()
{
  foo<int>(3.0);
}
