/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */

// Need power8 for l<b,h,q>arx
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

__int128 word_exchange_uti_ptr;
int word_exchange_uti_expected;
unsigned word_exchange_uti_desired() {
  __builtin_ppc_atomic_cas_local( /* { dg-error "argument 2 to '__builtin_ppc_atomic_cas_local' must be a pointer" } */
      &word_exchange_uti_ptr, word_exchange_uti_expected,
      word_exchange_uti_desired, 0, __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}

unsigned word_exchange_uti_desired_fptr() {
  __builtin_ppc_atomic_cas_local( /* { dg-error "argument 1 to '__builtin_ppc_atomic_cas_local' must not be a pointer to a function" } */
      &word_exchange_uti_desired_fptr, &word_exchange_uti_desired,
      &word_exchange_uti_desired_fptr, 0, __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}

unsigned word_exchange_uti_desired_mismatch() {
  __builtin_ppc_atomic_cas_local( /* { dg-error "size mismatch in argument 2" } */
      &word_exchange_uti_ptr, &word_exchange_uti_expected,
      &word_exchange_uti_ptr, 0, __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
