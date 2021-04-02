/*
TEST_OUTPUT:
---
fail_compilation/fail316.d(17): Error: mixin `fail316.foo.BadImpl!(uint, Mix1)` cannot resolve forward reference
---
*/
template BadImpl(T, alias thename)
{
  void a_bad_idea(T t)
  {
    thename.a_bad_idea(t);
  }
}

class foo
{
  mixin BadImpl!(uint,Mix1) Mix1;
}

int main()
{
  return 0;
}
