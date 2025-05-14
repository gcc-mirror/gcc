// PR c++/119563
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
    if (x.size () != 130 + 1024 + 130)
      __builtin_abort ();
    unsigned int i = 1, j = 0;
    for (auto a = x.begin (); a < x.end (); ++a)
      if (a->b != i)
	__builtin_abort ();
      else
	{
	  if (j == 129 || j == 129 + 1024)
	    i = 0;
	  i = (i & 15) + 1;
	  ++j;
	}
    c = true;
  }
  bool c;
};

#define D 1 + 0, 2 + 0, 3 + 0, 4 + 0, 5 + 0, 6 + 0, 7 + 0, 8 + 0, \
	  9 + 0, 10 + 0, 11 + 0, 12 + 0, 13 + 0, 14 + 0, 15 + 0, 16 + 0
#define E D, D, D, D, D, D, D, D

C c { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      1, 2, E, E, E, E, E, E, E, E,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
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
