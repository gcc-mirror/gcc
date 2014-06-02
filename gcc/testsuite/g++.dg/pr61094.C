/* { dg-do compile } */
/* { dg-options "-O3" }  */

template <typename> struct A {
  unsigned _width, _height, _depth, _spectrum;
  template <typename t> A(t p1) {
    int a = p1.size();
    if (a) {
      _width = p1._width;
      _depth = _height = _spectrum = p1._spectrum;
    }
  }
  long size() { return (long)_width * _height * _depth * _spectrum; }
};

int d;
void fn1(void *);
A<int> *fn2();
void fn3() {
  int b;
  for (;;) {
    A<char> c(*fn2());
    fn1(&c);
    if (d || !b)
      throw;
  }
}




