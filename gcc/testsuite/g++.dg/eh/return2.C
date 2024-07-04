// PR c++/113088

#if __cplusplus >= 201103L
#define THROWS noexcept(false)
#else
#define THROWS
#endif

struct X {
  ~X() {}
};

struct Y {
  ~Y() THROWS {}
};

X foo() {
  try {
    return X();
  } catch (...) {
    Y();
    return X();
  }

  try { } catch (...) { }
}
