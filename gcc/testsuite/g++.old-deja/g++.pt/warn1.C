// Build don't link:
// Special g++ Options: -Wunused

template <class T>
struct S
{
  struct R 
  {
    R();
    ~R();
  };

  void foo()
  {
    R r;			// no warning
    int i;			// WARNING - unused
  }

  S();
  ~S();
};

void f()
{
  S<int> si;
  si.foo();
}
