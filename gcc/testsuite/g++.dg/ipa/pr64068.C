/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef int PROV_ENUMALGS_EX, PCCRYPT_OID_INFO;
class A {
  void m_fn2();
  virtual bool m_fn1(PCCRYPT_OID_INFO);
};
int fn1();
void fn2();
void A::m_fn2() { m_fn1(0); }

bool fn3() {
  for (;;) {
    if (fn1()) {
      if (fn1() != 259)
        fn2();
      break;
    }
    return 1;
  }
  return 0;
}

class B {
public:
  B() { fn3(); }
};
class C : A {
  bool m_fn1(PCCRYPT_OID_INFO) { m_fn3(); return true; }
  int isSupportedByProvider_algId;
  PROV_ENUMALGS_EX isSupportedByProvider_outEnumAlgs;
  PROV_ENUMALGS_EX isSupportedByProvider_enumAlgs;
  bool m_fn3() {
    while (1) {
      if (fn1()) {
        if (fn1() != 259)
          fn2();
        break;
      }
      if (isSupportedByProvider_algId)
        isSupportedByProvider_outEnumAlgs = isSupportedByProvider_enumAlgs;
      return 1;
    }
    return 0;
  }
};

void fn4() { B(); }
