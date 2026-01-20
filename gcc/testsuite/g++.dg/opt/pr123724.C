// PR target/123724
// { dg-do compile { target c++17 } }
// { dg-options "-O2 -g" }

namespace std {
template <typename T> struct initializer_list {
  const T *_M_array;
  decltype (sizeof 0) _M_len;
  auto size () const { return _M_len; }
  const T *begin () const { return _M_array; }
  const T *end () const { return _M_array + _M_len; }
};
}

typedef std::initializer_list <int> A;
struct B {
  int b, c;
  short d[4][3];
  void foo (int);
  void bar (A x) { for (auto i : x) d[b][c] |= i; }
};

void
B::foo (int x)
{
  for (int i = 0; i < x; i++)
    {
      A x = { 1, 2 };
      bar (x);
    }
}
