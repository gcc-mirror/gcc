/*
TEST_OUTPUT:
---
fail_compilation/fail44.d(18): Error: expression `bar[i]` is `void` and has no value
---
*/

void Foo()
{
  void[] bar;
  void[] foo;

  bar.length = 50;
  foo.length = 50;

  for(size_t i=0; i<50; i++)
  {
    foo[i] = bar[i];
  }
}

