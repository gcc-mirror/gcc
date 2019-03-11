// PR c++/85227
// { dg-do compile { target c++11 } }
// { dg-options "" }

extern struct A a;

template<int> void foo()
{
  auto[i] = a;  // { dg-warning "incomplete" }
}  // { dg-warning "structured bindings only available with '-std=c..17' or '-std=gnu..17'" "" { target c++14_down } .-1 }
