// PR c++/71227
// { dg-options "" }

class A {
  public:
    template<typename T>
      friend int f(int x, T v) { // { dg-message "declaration" }
        return x + v;
      }
};


template<>
int f(int x, int v) {		// { dg-warning "friend" }
  return x + v;
}
