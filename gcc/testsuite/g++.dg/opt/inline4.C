// { dg-options "-O2 -ftemplate-depth-20000 --param min-inline-insns=100 --param max-inline-insns=3" }

template <int I>
inline void g() { g<I-1>(); return; }

template <>
inline void g<0>() { int i; return; }

void h() {
  g<1000>();
}

// { dg-final { scan-assembler-not "_Z1g"  } }
