// PR c++/107755
// { dg-do compile }
// { dg-options "-Wlogical-op" }

struct Foo
{
  operator bool() const { return false; }
};

bool a;
Foo b;

template <typename ignored>
static bool Bar()
{
  return (true && (false ? a : b));
  return (false || (false ? a : b));
}

bool Baz()
{
  return Bar<void>();
}
