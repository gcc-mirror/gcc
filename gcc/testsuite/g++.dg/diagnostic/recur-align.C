// ICE with diagnostic recursion
// { dg-do compile { target { c++11_only || c++14_only } } }
// { dg-options -Waligned-new }

struct __attribute__ ((aligned(256))) Aligned
{
  int b;
};

template<typename T>
auto Foo (const T* x) -> decltype (new T (*x))
{
  return new T (*x); // { dg-warning "with extended alignment" }
}

void Bar () {
  Aligned y;
  Foo (&y);
}
