namespace A {
  void f();
}
int g()
{
  struct f { };
  using A::f;
}
