// PR c++/64356
// { dg-do compile { target c++14 } }

typedef unsigned long size_t;

template<size_t N>
constexpr size_t f(const char (&x)[N]) {
  size_t s = 0;
  for(size_t c : x)
    s += c;
  return s;
}

template<size_t N>
constexpr size_t g(const char (&x)[N]) {
  char y[N] = {0};
  for(size_t i = 0; i < N; ++i)
    y[i] = x[i];
  return f(y);
}

constexpr auto x = g(__DATE__);
