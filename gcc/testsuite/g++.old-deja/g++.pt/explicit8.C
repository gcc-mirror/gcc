// { dg-do assemble  }
// GROUPS passed templates
template <class T, class U>
void foo(T t, U u);

template <class U>
void foo(double, U) {}

void baz()
{
  foo<char*>(3.0, "abc");
  foo<char*, double>("abc", 3.0);
}
