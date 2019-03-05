// a.h:
template <typename T>
int counter() {
  static int cnt = 0;
  return ++cnt;
}
inline int f() {
  return counter<decltype([] {})>();
}
