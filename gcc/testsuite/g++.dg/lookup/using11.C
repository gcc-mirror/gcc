namespace N1 {
  enum e { a };
  void e(char);
}

void f() {
  using N1::e;
  enum e x;
}
