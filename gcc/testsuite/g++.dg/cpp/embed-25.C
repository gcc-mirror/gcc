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
    unsigned char buf[] = {
#embed __FILE__
    };
    if (x.size () != sizeof (buf))
      __builtin_abort ();
    unsigned int i = 0;
    for (auto a = x.begin (); a < x.end (); ++a, ++i)
      if (a->b != buf[i])
	__builtin_abort ();
    c = true;
  }
  bool c;
};

C c {
#embed __FILE__
};

int
main ()
{
  if (!c.c)
    __builtin_abort ();
}
