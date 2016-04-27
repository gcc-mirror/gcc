// { dg-do compile { target c++11 } }

constexpr int Foo (int i)
{
  return (i ? Foo (i - 1): 0) + i;
}

static int a = Foo (0);
static int b = Foo (1);
static int d = Foo (3);
static int c = Foo (2);
static int e = Foo (4);
static int g = Foo (6);
static int f = Foo (5);
