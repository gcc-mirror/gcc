// PR c++/90393

struct S {
  S();
  S(const S&) {}
};

S f() {
  const S m;
  return true ? m : throw 0;
}

int main() {}
