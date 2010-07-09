// { dg-do compile }
// { dg-options "-w" }

// We don't want to use a covariant thunk to have a virtual
// primary base

struct c4 {};

struct c6 : c4 { virtual c4* f17(); };

c4* c6::f17() { return 0; }

struct c11 : virtual c6 { int i; };

struct c12 : c11 { };

struct c14 : 
  virtual c12,
  virtual c11 { virtual c12* f17(); };

// { dg-final { scan-assembler-not "\n_ZTch0_v0_n16_N3c143f17Ev\[: \t\n\]" } }
