// PR c++/60627
// { dg-do compile { target c++14 } }
// { dg-options "" }

template<typename T> void foo(T) {}

template void foo(auto);  // { dg-error "auto|does not match" }

void bar()
{
  foo(0);
}
