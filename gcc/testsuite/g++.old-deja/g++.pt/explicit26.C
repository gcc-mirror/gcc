// { dg-do link  }
// GROUPS passed templates
template <class T>
int foo(T t) { return 1; }

template <>
int foo(int i) { return 0; }

int main()
{
  (int (*)(int)) &foo<int>;
}
