// { dg-do compile }

template <class>
struct S {
  S();
  friend int f(S x) { return x.i + x.j; }
  template <class T>
    friend int g(S x, T) { return x.i + x.j; }
private:
  int i;
protected:
  int j;
};
