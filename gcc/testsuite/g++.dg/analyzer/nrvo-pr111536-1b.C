// { dg-do compile { target c++11 } }

struct Guard {
  int i;
  ~Guard() {}
};
Guard lock() {
  return Guard(); // { dg-bogus "uninitialized" }
}
void bar() {
  auto foo = lock();
}
