// PR c++/87921
// { dg-do compile { target c++17 } }

template <class H>
struct X
{
  static inline long x[] = { 1L };
  long foo () { return x[0]; }
};

void
bar ()
{
  class L {};
  X<L> v {};
}
