// PR c++/97034
// { dg-do compile { target c++20 } }

template<typename>
struct E {
  template <typename T>
  struct G {
    T t;
  };

  void fn() { G{1}; }
};

void
g () {
  E<int> e;
  e.fn ();
}
