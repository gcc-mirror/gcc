template <class T>
struct S {
  static int f ()
  {
    static int i;
    return ++i;
  }
  S () {};
  ~S () {};
};

typedef S<int> a;

int g ()
{
  return a::f();
}
