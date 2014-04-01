// An explicit specialization gets the tag from its template.

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
Foo<int>			// { dg-warning "attribute" }
{
  int f();
};

// { dg-final { scan-assembler "_ZN3FooB5cxx11IdE1fEv" } }
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
