// ICE(template.c) in DMD0.080
/*
TEST_OUTPUT:
---
fail_compilation/fail16.d(19): Error: function declaration without return type. (Note that constructors are always named `this`)
fail_compilation/fail16.d(19): Error: variable name expected after type `bar!(typeof(X))(X)`, not `;`
---
*/

int i;

template bar(T)
{
  void bar(int x) {}
}

template foo(alias X)
{
  bar!(typeof(X))(X);
}


void main()
{
  foo!(i);
}
