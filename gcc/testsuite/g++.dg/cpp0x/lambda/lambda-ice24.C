// PR c++/82293
// { dg-do compile { target c++11 } }
// { dg-options "-Wshadow" }

template <typename>
struct S {
  int f{[this](){return 42;}()};
};

int main(){
  return S<int>{}.f;
}
