// { dg-do assemble  }

template <int I>
struct S {};

template <int J>
void foo(S<J - 1>);

template <class T>
void baz(S<sizeof(T)>);

template <int J>
void fun(S<J>, S<J * 2>);

void bar()
{
  foo<5>(S<4>()); // OK - 4 is 5 - 1.
  baz<int>(S<sizeof(int)>()); // OK
  fun(S<4>(), S<8>()); // OK - deduce J from first argument.
}
