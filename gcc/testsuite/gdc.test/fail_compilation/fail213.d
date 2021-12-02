/*
TEST_OUTPUT:
---
fail_compilation/fail213.d(18): Error: template instance `Foo!int` does not match template declaration `Foo(T : immutable(T))`
fail_compilation/fail213.d(25): Error: template instance `Foo!(const(int))` does not match template declaration `Foo(T : immutable(T))`
---
*/

template Foo(T:immutable(T))
{
    alias T Foo;
}

void main()
{
  {
    int x;
    alias Foo!(typeof(x)) f;
    //printf("%s\n", typeid(f).toString().ptr);
    assert(is(typeof(x) == int));
    assert(is(f == int));
  }
  {
    const int x;
    alias Foo!(typeof(x)) f;
  }
}
