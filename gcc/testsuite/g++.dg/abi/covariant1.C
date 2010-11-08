// { dg-do compile }
// { dg-options "-w" }

// If a covariant thunk is overriding a virtual primary base, we have to
// use the vcall offset even though we know it will be 0.

struct c4 {};

struct c6 : c4 { virtual c4* f17(); };

c4* c6::f17() { return 0; }

struct c11 : virtual c6 { int i; };

struct c12 : c11 { };

struct c14 : 
  virtual c12,
  virtual c11 { virtual void f(); c12* f17(); };

void c14::f() { }

// { dg-final { scan-assembler "_ZTcv0_n12_v0_n16_N3c143f17Ev" { target { ilp32  && { ! { ia64-*-hpux* } } } } } }
// { dg-final { scan-assembler-not "_ZTch0_v0_n16_N3c143f17Ev" } }
// { dg-final { scan-assembler "_ZTcv0_n24_v0_n32_N3c143f17Ev" { target { lp64  || { ia64-*-hpux* } } } } }
// { dg-final { scan-assembler-not "_ZTch0_v0_n32_N3c143f17Ev" } }
