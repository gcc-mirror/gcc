// Build don't link:

template <int I>
struct S {};

template <int J>
void foo(S<J + 2>);

void bar()
{
  foo(S<3>()); // ERROR - no way to deduce J from this.
}
