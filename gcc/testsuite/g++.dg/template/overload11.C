// PR c++/39413
// We don't need to instantiate Wrapper<int> to check the
// foo(const Thingy&) overload.

template <class T> struct Incomplete;

template <typename T> class Wrapper
{
  Incomplete<T> i;
};

template <typename T> struct Thingy
{
  Thingy();
  Thingy(const Wrapper<T>& v);

  template <typename X> void foo(const Thingy<X>&);
  void foo(const Thingy&);
};

int main()
{
    Thingy<int> ap1;
    Thingy<float> bp1;

    ap1.foo(bp1);
}
