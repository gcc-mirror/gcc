// { dg-do compile { target c++20 } }

template<int N, typename T>
concept Foo = requires(T t) { foo<N + 1>(t); }; // { dg-error "template instantiation depth" }

template<int N = 1, typename T = int>
  requires Foo<N, T>
int foo(T t)
{
  return foo<N + 1>(t);
}

int main(int, char**)
{
  return foo<1>(1);
}

// { dg-prune-output "compilation terminated" }
