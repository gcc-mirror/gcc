/* PR target/87839 */
/* { dg-do compile } */
/* { dg-options "-O2 -w" } */

long long b[64];
void foo (void);
int bar (void (*) (void));
void qux (long long *, long long) __attribute__((noreturn));
void quux (long long *, long long);

void
baz (void)
{
  __sync_val_compare_and_swap (b, 4294967298LL, 78187493520LL);
  __sync_bool_compare_and_swap (b + 1, 8589934595LL, 21474836489LL);
  __sync_fetch_and_xor (b, 60129542145LL);
  quux (b, 42949672967LL);
  __sync_xor_and_fetch (b + 22, 60129542145LL);
  quux (b + 23, 42949672967LL);
  if (bar (baz))
    __builtin_abort ();
  foo ();
  __sync_val_compare_and_swap (b, 4294967298LL, 0);
  __sync_bool_compare_and_swap (b + 1, 8589934595LL, 78187493520LL);
  if (__sync_or_and_fetch (b, 21474836489LL) != 21474836489LL)
    qux (b + 22, 60129542145LL);
  __atomic_fetch_nand (b + 23, 42949672967LL, __ATOMIC_RELAXED);
  bar (baz);
}
