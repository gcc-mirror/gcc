// PR c++/100797
// { dg-do run }

bool ok = false;
struct S1 { virtual ~S1() {} };
struct S2 { virtual void f1() = 0; };
struct S3: S1, S2 {
    void f1() { f2(); }
    virtual void f2() = 0;
};
struct S4: S3 {
  void f2() { ok = true; }
  using S2::f1;
};
int main() {
  S4().f1();
  if (!ok) __builtin_abort ();
}
