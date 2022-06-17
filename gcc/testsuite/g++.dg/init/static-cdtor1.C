// { dg-do compile { target { lp64 && { i?86-*-linux* x86_64-*-linux* } } } }
// { dg-additional-options -fno-use-cxa-atexit }
// Make sure we emit initializers in the correct order.

// ctors
// { dg-final { scan-assembler {_Z41__static_initialization_and_destruction_0v:.*movl	\$var1[^\n]*\n[^\n]*_ZN5LeelaC1Ev[^\n]*\n[^\n]*movl	\$var2[^\n]*\n[^\n]*_ZN5LeelaC1Ev[^\n]*\n[^\n]*movl	\$var3[^\n]*\n[^\n]*_ZN5LeelaC1Ev} } }
// dtors
// { dg-final { scan-assembler {_Z41__static_initialization_and_destruction_1v:.*movl	\$var3[^\n]*\n[^\n]*_ZN5LeelaD1Ev[^\n]*\n[^\n]*movl	\$var2[^\n]*\n[^\n]*_ZN5LeelaD1Ev[^\n]*\n[^\n]*movl	\$var1[^\n]*\n[^\n]*_ZN5LeelaD1Ev} } }

struct Leela {
  Leela ();
  ~Leela ();
};

Leela var1;
Leela var2;
Leela var3;
