// PR c++/115583
// { dg-do compile { target c++23 } }

consteval int f(int i) {
  return i;
}
const bool b = 0;
constexpr int g(int i) {
  if consteval {
    return f(i);
  } else {
    return i;
  }
}
int main() {
  return g(1);
}
