/* { dg-additional-options "-flax-vector-conversions" } */

inline void foo (const __SVInt32_t &foo) {}
void bar (__SVUint32_t x) { foo(x); }
