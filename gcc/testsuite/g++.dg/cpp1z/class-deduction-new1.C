// PR c++/101988
// { dg-do compile { target c++17 } }

template<typename T>
struct A {
  A(T);
  A();
};
auto p1 = new A[]{1}; // { dg-error "creating array of template placeholder type" }
auto p2 = new A[1]{1}; // { dg-error "invalid use of placeholder" }
auto p3 = new A<int>[]{1};
auto p4 = new A<int>[1]{1};
auto p5 = new A[]{1, 2}; // { dg-error "creating array of template placeholder type" }
auto p6 = new A<int>[]{1, 2};
auto p7 = new A<int>[]{A(1), A(1)};
auto p8 = new A<int>[2]{A(1), A(1)};
