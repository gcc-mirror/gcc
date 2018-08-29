/* { dg-do compile } */
/* { dg-options "-O3 -fno-early-inlining -fno-ipa-sra -fdump-ipa-cp"  } */
class A {};
class B {
public:
  A &operator[](int);
};
class C : B {
public:
  virtual int m_fn1() { return 0; }
  A &operator[](int p1) {
    int a;
    a = m_fn1();
    static_cast<void>(__builtin_expect(a, 0) ?: 0);
    return B::operator[](p1);
  }
};

C b;
int *e;
static void sort(C &p1, C &p2) {
  for (int i=0;; i++) {
    A c, d = p2[0];
    p1[0] = c;
    p2[0] = d;
  }
}

void lookupSourceDone() { b[0]; }

void update_sources() {
  if (e) {
    C f;
    sort(f, b);
  }
}
/* Note that we miss one devirtualization because we are not able to track the
   vtbl store in destructor.  
   Previously we devirtualized to C::m_fn1 instead of B::m_fn1.  */
/* { dg-final { scan-ipa-dump-times "Discovered a virtual call to a known target" 2 "cp"  } } */
