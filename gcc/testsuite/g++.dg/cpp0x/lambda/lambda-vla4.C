// PR c++/60855
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-vla" }

int main() {
  unsigned count = 5;
  bool array[count];
  [&array] () {
    array[0] = sizeof(array) > 5;
  }();
  return 0;
}
