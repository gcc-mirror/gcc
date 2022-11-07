/* { dg-do compile } */
/* { dg-options "-mraoint -O2 -mprefer-remote-atomic" } */
/* { dg-final { scan-assembler-times "aadd" 2 { target {! ia32 } } } } */
/* { dg-final { scan-assembler-times "aand" 2 { target {! ia32 } } } } */
/* { dg-final { scan-assembler-times "aor" 2 { target {! ia32 } } } } */
/* { dg-final { scan-assembler-times "axor" 2 { target {! ia32 } } } } */
/* { dg-final { scan-assembler-times "aadd" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "aand" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "aor" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "axor" 1 { target ia32 } } } */
volatile int x;
volatile long long y;
int *a;
long long *b;

void extern
rao_int_test (void)
{
  __atomic_add_fetch (a, x, __ATOMIC_RELAXED);
  __atomic_and_fetch (a, x, __ATOMIC_RELAXED);
  __atomic_or_fetch (a, x, __ATOMIC_RELAXED);
  __atomic_xor_fetch (a, x, __ATOMIC_RELAXED);
#ifdef __x86_64__
  __atomic_add_fetch (b, y, __ATOMIC_RELAXED);
  __atomic_and_fetch (b, y, __ATOMIC_RELAXED);
  __atomic_or_fetch (b, y, __ATOMIC_RELAXED);
  __atomic_xor_fetch (b, y, __ATOMIC_RELAXED);
#endif
}
