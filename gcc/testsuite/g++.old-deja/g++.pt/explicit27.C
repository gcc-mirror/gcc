// Build don't run:
// GROUPS passed templates
template <class T>
void foo(T t);

template <>
void foo(int i) {}

int main()
{
  &foo<int>;
}
