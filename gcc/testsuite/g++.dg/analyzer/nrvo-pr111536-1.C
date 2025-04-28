struct Guard {
  int i;
  ~Guard() {}
};
Guard lock() {
  return Guard(); // { dg-bogus "uninitialized" }
}
void bar() {
  Guard foo = lock();
}

