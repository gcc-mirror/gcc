/* { dg-do run } */
/* { dg-options "-std=c++17 -O2" } */

template <int Size> struct Vector {
  int m_data[Size];
  Vector(int, int, int) {}
};
enum class E { POINTS, LINES, TRIANGLES };

__attribute__((noipa))
void getName(E type) {
  static E check = E::POINTS;
  if (type == check)
    check = (E)((int)check + 1);
  else
    __builtin_abort ();
}

int main() {
  int arr[]{0, 1, 2};
  for (auto dim : arr) {
    Vector<3> localInvs(1, 1, 1);
    localInvs.m_data[dim] = 8;
  }
  E types[] = {E::POINTS, E::LINES, E::TRIANGLES};
  for (auto primType : types)
    getName(primType);
  return 0;
}
