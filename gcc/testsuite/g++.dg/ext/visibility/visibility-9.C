// Test that dllimport'd functions have default visibility.
// { dg-require-visibility "" }
// { dg-require-dll "" }
// { dg-options "-fvisibility=hidden" }
// { dg-final { scan-not-hidden "_Z2f1v" } }
// { dg-final { scan-not-hidden "_ZN1S2f3Ev" } }

extern void  __attribute__((dllimport)) f1();
void f2() {
  f1();
}

struct __attribute__((visibility("hidden")) S1 {
  __attribute__((dllimport)) void f3();
};

void f4() {
  S1 s1;
  s1.f3();
}

struct S2 {
  __attribute__((dllimport)) void f5();
};

void f6() {
  S2 s2;
  s2.f5();
}
