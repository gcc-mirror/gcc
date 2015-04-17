// { dg-do run { target c++11 } }

static int g;

struct A {
  A() { g = 1; }
};

struct accessor {
  A a;
  int x;
};

int
main (void)
{
  (void) accessor{};

  if (g != 1)
    __builtin_abort ();
}
