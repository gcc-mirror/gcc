// { dg-options "-O2 -ftemplate-depth-20000" }

template <int I>
inline void g() { g<I-1>(); return; }

template <>
inline void g<0>() { int i; return; }

void h() {
  g<250>();
}

// { dg-final { scan-assembler-not "_Z1g"  } }
