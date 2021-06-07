/* { dg-do compile } */
/* { dg-options "-O0 -fPIC" } */

class A {
public:
  virtual int a (void);
};

class B {
public:
  virtual int b (void);
};

class C : public B, public A {
public:
  virtual int a (void);
};

int C::a (void) { return b(); }

/* { dg-final { scan-assembler {\n_ZThn8_N1C1aEv:\n} { target lp64 } } } */
/* { dg-final { scan-assembler {\n_ZThn4_N1C1aEv:\n} { target { ! lp64 } } } } */
/* { dg-final { scan-assembler {\n\tjg\t.LTHUNK0@PLT\n} { target lp64 } } } */
