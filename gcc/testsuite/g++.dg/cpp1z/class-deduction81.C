// PR c++/97034
// { dg-do compile { target c++17 } }

template <typename Z>
struct E {
  template <typename T>
  struct G {
    T t;
    G(T) { }
  };

  void fn() { G{1}; }
};

void
g ()
{
  E<int> e;
  e.fn ();
}
