// PR c++/104669

void bar()
{
  int foo(void);
  int foo(void) __attribute__((target("sse")));
  int foo(void) __attribute__((target("default")));
  int (*p)(void) = &foo;
  return;
}
