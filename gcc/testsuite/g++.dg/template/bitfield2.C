// PR c++/25503

template<int N>
struct Test
{
  Test()
  {
    typedef struct StaticAssert {unsigned condition : (N); } XXX; // { dg-error "zero width" }
  }
};

int
main()
{
  Test<0> T;
}
