// PR c++/88830

struct a {
  ~a();
};
class b {
  virtual void c(int &);
};
class C : b {
  void c(int &);
  virtual int d() = 0;
  a e;
};
void C::c(int &) {}
