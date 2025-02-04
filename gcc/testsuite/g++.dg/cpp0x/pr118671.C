// PR c++/118671
// { dg-do run { target c++11 } }
// { dg-options "-O2" }

namespace std {
template <typename T>
struct initializer_list {
private:
  T *_M_array;
  decltype (sizeof 0) _M_len;
public:
  constexpr decltype (sizeof 0)
  size () const noexcept { return _M_len; }
  constexpr const T *
  begin () const noexcept { return _M_array; }
  constexpr const T *
  end () const noexcept { return begin () + size (); }
};
}

struct A {} a;

struct B {
  constexpr B (int x) : B (a, x) {}
  template <typename... T>
  constexpr B (A, T... x) : b(x...) {}
  int b;
};

struct C {
  C (std::initializer_list<B> x)
  {
    if (x.size () != 130)
      __builtin_abort ();
    unsigned int i = 1;
    for (auto a = x.begin (); a < x.end (); ++a)
      if (a->b != i)
	__builtin_abort ();
      else
	i = (i & 15) + 1;
    c = true;
  }
  bool c;
};

C c { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      1, 2 };

int
main ()
{
  if (!c.c)
    __builtin_abort ();
}
