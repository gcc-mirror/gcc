// { dg-do link  }
// { dg-options "-O3" }
// Origin: Mark Mitchell <mark@codesourcery.com>

typedef int (*fp)();
 
struct S
{
  fp f;
};

struct T
{
  static int f() { return 0; }
};

static const S s = { &T::f };

int main()
{
  return (*s.f)();
}
