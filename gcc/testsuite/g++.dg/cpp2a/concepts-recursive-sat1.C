// { dg-do compile { target c++20 } }

template<int N, typename T>
concept Foo = requires(T t) { foo<N + 1>(t); }; // { dg-error "template instantiation depth" }

namespace ns
{
  struct S { };

  template<int N, typename T>
    requires Foo<N, T>
  int foo(T t)
  {
    return foo<N + 1>(t);
  }
}

int main(int, char**)
{
  return foo<1>(ns::S{});
}

// { dg-prune-output "compilation terminated" }
