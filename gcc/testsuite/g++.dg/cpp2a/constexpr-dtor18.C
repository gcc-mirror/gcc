// PR c++/121068
// { dg-do compile { target c++20 } }

template <class T>
constexpr void
destroy_at (T* p) { p->~T(); }

template <class T>
struct V {
  union {
    unsigned char buf[sizeof (T)];
    const T ct;
  };
  bool active;
  constexpr V(): active (false) {}
  constexpr V(T t): ct (t), active (true) { }
  constexpr ~V() { if (active) destroy_at (&ct); }
};

constexpr char f()
{
  const V<int> vi {42};
  return vi.ct;
}

static_assert (f() == 42);
