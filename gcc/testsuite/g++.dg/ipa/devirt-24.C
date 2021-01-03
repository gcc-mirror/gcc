/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-sra -fdump-ipa-inline -fdump-ipa-cp"  } */
void pad(void);
class A {};
class B {
public:
  A &operator[](int);
};
class C : B {
public:
  virtual int m_fn1() { return 0; }
  inline A &operator[](int p1) {
    int a;
    a = m_fn1();
    static_cast<void>(__builtin_expect(a, 0) ?: 0);
    return B::operator[](p1);
  }
};

int *e;
static void sort(C &p1, C &p2) {
  for (int i=0;; i++) {
    A c, d = p2[0];
	pad();
	pad();
	pad();
  }
}

int test ();

void update_sources() {
while (test()) {
    C f;
C *b = new (C);
    sort(f, *b);
  }
}
/* { dg-final { scan-ipa-dump-times "Discovered a virtual call to a known target" 1 "inline" { xfail *-*-* } } } */
/* { dg-final { scan-ipa-dump-times "Aggregate passed by reference" 2 "cp"  } } */
