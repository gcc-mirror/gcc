// { dg-additional-options "-w" }
struct S1 {
  virtual ~S1();
  virtual void v();
};
struct S2: S1 {};
struct S3: S1, S2 { void v(); };
struct S4: S3 { void v(); };
void S4::v() { S3::v(); }
struct R {
  S1 * m;
  void f(S2 * x) {
    static_cast<S1 *>(x)->v();
    x->v();
    m = x;
  }
};
void f() {
  R r;
  r.f(new S4);
  r.f(new S3);
}
