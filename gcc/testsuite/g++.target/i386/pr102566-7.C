/* { dg-do compile { target c++11 } } */
/* { dg-options "-O2" } */

#include <atomic>

template<uint32_t b>
void lock_bts(std::atomic<uint32_t> &a) { while (!(a.fetch_or(b) & b)); }
template<uint32_t b>
void lock_btr(std::atomic<uint32_t> &a) { while (a.fetch_and(~b) & b); }
template<uint32_t b>
void lock_btc(std::atomic<uint32_t> &a) { while (a.fetch_xor(b) & b); }
template void lock_bts<1U<<30>(std::atomic<uint32_t> &a);
template void lock_btr<1U<<30>(std::atomic<uint32_t> &a);
template void lock_btc<1U<<30>(std::atomic<uint32_t> &a);
template void lock_bts<1U<<31>(std::atomic<uint32_t> &a);
template void lock_btr<1U<<31>(std::atomic<uint32_t> &a);
template void lock_btc<1U<<31>(std::atomic<uint32_t> &a);

/* { dg-final { scan-assembler-times "lock;?\[ \t\]*btsl" 2 } } */
/* { dg-final { scan-assembler-times "lock;?\[ \t\]*btrl" 2 } } */
/* { dg-final { scan-assembler-times "lock;?\[ \t\]*btcl" 2 } } */
/* { dg-final { scan-assembler-not "cmpxchg" } } */
