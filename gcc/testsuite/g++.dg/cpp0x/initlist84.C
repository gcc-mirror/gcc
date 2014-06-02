// PR c++/61242
// { dg-do compile { target c++11 } }

struct Foo
{
  struct A
  {
    const int &container;
    const int &args;
  };
  static void Create (const A &);
};

int main ()
{
  Foo::Create ({{}, {}});
}
