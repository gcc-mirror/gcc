// PR middle-end/87574
// Testcase by David Binderman <dcb314@hotmail.com>

// { dg-do compile }
// { dg-options "-O2 -g -Wno-return-type" }

class a {
public:
  virtual ~a();
};
class c {
public:
  enum j {};
  virtual j d() = 0;
};
class e : a, c {
  j d();
};
class f;
class g {
public:
  static g *h();
  f *i();
};
class f {
public:
  template <class b> b *l(int);
};
c::j e::d() {}
void m() {
  for (int k;;)
    g::h()->i()->l<c>(k)->d();
}
