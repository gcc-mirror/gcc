// { dg-do link  }
// GROUPS passed templates
template <class T>
void foo(T t);

template <>
void foo(int i) {}

int main()
{
  (void (*)(int)) &foo<int>;
}
