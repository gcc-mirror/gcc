// { dg-do compile { target c++14 } }

typedef unsigned uint32_t __attribute__((mode (__SI__)));

struct Foo
{
  uint32_t i: 32;
};

int
main()
{
  Foo f{};
  return f.i;
}
