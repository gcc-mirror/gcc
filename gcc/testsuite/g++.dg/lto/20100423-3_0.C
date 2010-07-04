// { dg-lto-do assemble }
// { dg-lto-options {{-flto -g}} }

inline int foo()
{
  static union { int i; };
  return i;
}

void bar()
{
  foo();
}

