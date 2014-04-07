// { dg-options "-fno-gnu-unique" }

// A static variable in an inline function uses STB_GNU_UNIQUE normally.
inline int foo() { static int i; return ++i; }

extern "C" int fn()
{
  return foo();
}
