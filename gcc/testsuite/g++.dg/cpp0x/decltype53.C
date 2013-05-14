// PR c++/57092
// { dg-do compile { target c++11 } }

template <void (*F)(int)>
class B {
  decltype(F) v;
};

void foo(int) {}

B<foo> o;
