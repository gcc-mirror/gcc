// { dg-do compile { target c++1y } }

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
