// { dg-do assemble  }
// { dg-options "-Wunused" }

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
    int i;			// { dg-warning "" } unused
  }

  S();
  ~S();
};

void f()
{
  S<int> si;
  si.foo();
}
