// { dg-do compile }

// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/9453
// Access checking when template friend is defined in class.

template <typename> class X {
  private:
    struct Inner;

    template <typename R>
    friend typename X<R>::Inner * foo (X<R>*) { return 0; }
};
template class X<void>;
X<void>* p;

struct U {
  void bar () { foo (p); }
};
