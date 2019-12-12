// { dg-do compile { target c++14 } }
struct Z { // { dg-bogus "default constructor" }
  int y;   // { dg-bogus "initialize" }
};

template <class T>
constexpr Z f(const T *data) {
  Z z;
  __builtin_memcpy(&z, data, sizeof(z));
  return z;
}

constexpr Z g(const char *data) { return f(data); }
