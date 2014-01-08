/* PR target/56225 */
/* { dg-do compile { target { ia32 && fpic } } } */
/* { dg-options "-O2 -fno-omit-frame-pointer -march=i686 -fpic" } */

void NoBarrier_AtomicExchange (long long *ptr) {
  while (__sync_val_compare_and_swap (ptr, 1, 0) );
}
