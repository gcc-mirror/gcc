// PR c++/66921
// { dg-do compile { target c++11 } }

template<typename T>
struct Holder {
  constexpr static const int array[] = { 1, 2, 3 };
  enum {F = array[0]};
};
class HI: public Holder<int> {};
