// { dg-do assemble  }

template <int I>
struct S {};

template <int J>
void foo(S<J + 2>);

void bar()
{
  foo(S<3>()); // { dg-error "" } no way to deduce J from this.
}
