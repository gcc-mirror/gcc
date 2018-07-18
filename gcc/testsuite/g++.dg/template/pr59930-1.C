// PR c++/59930

namespace N {
  template<typename T> class A {
    // The injected name is N::B, because we don;t look outside of N
    template<typename U> friend struct B;
  private:
    int n; // { dg-message "declared private here" }
  public:
    A (int);
  };
}

template<typename T> struct B {
  int f(N::A<int> ai) { return ai.n; } // { dg-error "is private" }
};

int k = B<int>().f(0);
