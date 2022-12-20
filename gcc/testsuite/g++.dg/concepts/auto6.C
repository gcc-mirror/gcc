// PR c++/101886
// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template<typename, typename> struct A { };

template<class T>
void f() {
  A<int, int> a;
  A<auto, auto> b1 = a;
  A<auto, auto> b2 = a;
}

template void f<int>();
