// Test that a definition marked with dllexport has default
// visibility.
// { dg-require-visibility "" }
// { dg-require-dll "" }
// { dg-options "-fvisibility=hidden" }
// { dg-final { scan-not-hidden "_ZN1A1fEv" } }
// { dg-final { scan-not-hidden "_Z1gv" } }
// { dg-final { scan-not-hidden "_Z1hv" } }
// { dg-final { scan-not-hidden "_ZN1B1iEv" } }
// { dg-final { scan-not-hidden "_ZN1B1jEv" } }
// { dg-final { scan-not-hidden "_ZN1A1a" } }
// { dg-final { scan-not-hidden "_ZN1B1b" } }
// { dg-final { scan-not-hidden "k" } }
// { dg-final { scan-not-hidden "l" } }

struct __declspec(dllexport) A {
  void f();
  static int a;
};

void A::f() {}

int A::a;

__declspec(dllexport) void g() {}

__declspec(dllexport) void h();
void h() {}

struct B {
  void i();
  __declspec(dllexport) void j();
  __declspec(dllexport) static int b;
};

__declspec(dllexport) void B::i() {}

void B::j() {}

int B::b;

__declspec(dllexport) int k;

__declspec(dllexport) extern int l;
int l;
