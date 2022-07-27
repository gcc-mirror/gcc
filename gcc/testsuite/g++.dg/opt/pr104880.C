// { dg-do compile }
// { dg-options "-O2 -Wno-pmf-conversions -fno-checking" }

class c {
  long b;
};
class B {
public:
  typedef void *d;
};
class aa {
public:
  aa(B::d);
};
class e : public B {
public:
  e();
};
unsigned int f;
struct g {
  struct h : c {
    h(unsigned int &i) : c(reinterpret_cast<c &>(i)) {}
    unsigned int ad();
  };
};
class n : g {
public:
  n(int);
  void j() {
    unsigned int a;
    h k(a);
    __atomic_compare_exchange_n(&f, &a, k.ad(), true, 3, 0);
  }
};
int l;
class m : e {
  void ar() {
    n b(l);
    b.j();
  }
  virtual void bd() { aa(d(&m::ar)); }
};
void o() { new m; }
