// PR c++/64462
// { dg-do compile { target c++11 } }

int x = 0;
int z;

int main() {
  constexpr int& y = x;
  // OK, 'y' is not ODR used
  [] { z = y; }();
}
