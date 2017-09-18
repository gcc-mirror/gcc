// PR c++/61362
// { dg-do compile { target c++11 } }

template <typename>
struct S {
  int f{[this](){return 42;}()};
};

int main(){
  return S<int>{}.f; // should be 42
}
