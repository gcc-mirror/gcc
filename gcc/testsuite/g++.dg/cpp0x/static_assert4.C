// { dg-options "-std=c++11 --param ggc-min-heapsize=0 --param ggc-min-expand=0 " }
// PR C++/30033
// Make sure that the static assert does not crash the GC.

template <class T>
struct default_delete
{
  void
  operator() (T * ptr) const
  {
    static_assert (sizeof (T) > 0, "Can't delete pointer to incomplete type");
  }
};


