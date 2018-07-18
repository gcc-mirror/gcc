// PR c++/81073
// { dg-options "" }
// { dg-do compile { target c++11 } }

struct test { const int *addr; };

const test* setup()
{
  static constexpr test atest =
    {
      ({ static const int inner = 123; &inner; }) // { dg-error "static" }
    };

  return &atest;
}

int main(){}
