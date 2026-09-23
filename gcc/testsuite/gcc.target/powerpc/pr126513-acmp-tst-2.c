/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */

// Need power8 for l<b,h,q>arx
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mno-quad-memory-atomic" } */

__int128 word_exchange_uti_ptr;
__int128 word_exchange_uti_ptr_expected;
__int128 word_exchange_uti_ptr_desired;

unsigned word_exchange_uti_desired() {
  __builtin_ppc_atomic_cas_local( /* { dg-error "'__builtin_ppc_atomic_cas_local' requires the '-mquad-memory-atomic' option for 16-byte operands" } */
      &word_exchange_uti_ptr, &word_exchange_uti_ptr_expected,
      &word_exchange_uti_ptr_desired, 0, __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
