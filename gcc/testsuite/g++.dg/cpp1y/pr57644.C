// { dg-do compile { target c++14 } }

struct Foo
{
  unsigned i: 32;
};

int
main()
{
  Foo f{};
  return f.i;
}
