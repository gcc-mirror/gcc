// PR c++/127668
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "_Z3fooIjEDTcl21__builtin_bitreversegfp_EET_" } }

template <typename T>
auto foo (T t) -> decltype (__builtin_bitreverseg (t)) { return t; }

template unsigned foo <unsigned> (unsigned);
