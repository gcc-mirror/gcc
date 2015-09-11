// PR c++/60067

template <class> struct A;
template <class> struct B { enum { v = 1 };  };

template <class T = void (A<int>)>
struct C {
  void f () {
    void g (int [B<T>::v]);
  }
};

void foo (void) {
    C<int>().f ();
}
