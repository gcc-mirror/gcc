// { dg-do compile { target c++2a } }

template<typename T>
concept Fooable = requires(T t) { foo(t); }; // { dg-error "template instantiation depth" }

template<Fooable T>
void foo(T t) { }

void test()
{
  struct S {} s;
  foo(s);
}

// { dg-prune-output "compilation terminated" }
