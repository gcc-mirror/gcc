// PR target/9786
// Origin: <nick@ilm.com>

// This used to fail on x86 because the reg-stack pass deleted
// an insn that could seemingly trap (but actually doesn't)
// without updating the CFG.

// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions" }

struct D1 {
    float l;
    D1 GS() const {D1 d;float f=.299*l;d.l=f;return d;}
    static D1 G() {return D1();}
};

struct D2 {
    D1 g;
    D2(const D1& gi) : g(gi) {}
    D2 GS() const {return D2(g.GS());}
};

class A {
  public:
    virtual ~A() {}
};

class B : public A {
  public:
    B(const D2& mi);
    D2 fm;
};

B::B(const D2 &mi) : fm(mi.GS()) {}
