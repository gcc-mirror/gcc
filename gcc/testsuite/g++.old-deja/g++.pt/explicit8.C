// { dg-do assemble  }
// GROUPS passed templates
template <class T, class U>
void foo(T t, U u);

template <class U>
void foo(double, U) {}

void baz()
{
  foo<const char*>(3.0, "abc");
  foo<const char*, double>("abc", 3.0);
}
