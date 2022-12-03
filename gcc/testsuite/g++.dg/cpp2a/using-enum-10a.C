// A version of using-enum-10.C where Hog is a template.
// PR c++/103081
// { dg-do compile { target c++20 } }

enum class Pig { OINK };

template<int>
struct Hog {
  using enum Pig;
  Hog(Pig) { OINK; }
};

template<int N>
void pen() {
  Hog<1>(Hog<1>::OINK);
  Hog<N>(Hog<N>::OINK);
}

template void pen<0>();
