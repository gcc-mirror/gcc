// PR c++/81073
// { dg-options "" }
// { dg-do compile { target c++11 } }
// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.

struct test { const int *addr; };

const test* setup()
{
  static constexpr test atest =
    {
      ({ static const int inner = (throw 1, 1); &inner; }) // { dg-error "static" "" }
    };

  return &atest;
}

int main(){}
