// { dg-do assemble  }
// GROUPS passed templates
template <int I>
class S {};

template <int I, class T>
void foo(T t, S<I>);

void bar()
{
  S<3> s3;
  foo<3>("abc", s3);
}
