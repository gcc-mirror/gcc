// Build don't run:
// Special g++ Options: -O3
// Origin: Mark Mitchell <mark@codesourcery.com>

typedef int (*fp)();
 
struct S
{
  fp f;
};

struct T
{
  static int f() {}
};

static const S s = { &T::f };

int main()
{
  return (*s.f)();
}
