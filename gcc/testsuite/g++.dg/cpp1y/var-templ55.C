// { dg-do compile { target c++14 } }

template <class T> struct A {
  template <class U> static const U x = 1;
  static const int y = 2;
};

int main() {
  A<int> a;
  int y = a.y;         // OK
  int x = a.x<int>;     // ???
}
