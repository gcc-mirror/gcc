// An explicit specialization doesn't get the tag from its template unless
// it is specified there, too.

// { dg-final { scan-assembler "_ZN3FooB5cxx11IcE1fEv" } }
template<typename T>
struct __attribute ((abi_tag("cxx11"))) Foo
{
  int f();
};

// { dg-final { scan-assembler "_ZN3FooB5cxx11IiE1fEv" } }
template<>
struct
__attribute ((abi_tag("cxx11")))
Foo<int>
{
  int f();
};

// { dg-final { scan-assembler "_ZN3FooIdE1fEv" } }
template<>
struct
Foo<double>
{
  int f();
};

int main()
{
  Foo<int> f;
  f.f();
  Foo<char> f1;
  f1.f();
  Foo<double> f2;
  f2.f();
}
