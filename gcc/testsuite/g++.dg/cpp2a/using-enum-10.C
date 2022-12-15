// PR c++/103081
// { dg-do compile { target c++20 } }

enum class Pig { OINK };

struct Hog {
  using enum Pig;
  Hog(Pig) { }
};

template<int>
void pen() {
  Hog(Hog::OINK);
}

template void pen<0>();
