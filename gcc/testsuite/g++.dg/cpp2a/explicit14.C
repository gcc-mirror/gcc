// PR c++/89420
// { dg-do compile { target c++20 } }

template<typename>
struct S {
  explicit(int(1)) S(int);
  explicit(int{1}) S(int, int);
};

S<int> s(1);
S<int> s2(1, 2);
