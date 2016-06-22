/* { dg-do run } */
/* { dg-options "-g" } */
/* { dg-skip-if "" { *-*-* }  { "*" } { "-O0" } } */

typedef __UINTPTR_TYPE__ uintptr_t;

typedef struct { uintptr_t pa; uintptr_t pb; } fatp_t
  __attribute__ ((aligned (2 * __alignof__ (uintptr_t))));

__attribute__((noinline, noclone)) void
clear_stack (void)
{
  char a[128 * 1024 + 128];

  __builtin_memset (a, 0, sizeof (a));
}

__attribute__((noinline, noclone)) void
foo (fatp_t str, int count)
{
  char a[128 * 1024];

  if (count > 0)
    foo (str, count - 1);
  clear_stack ();
  count--;  /* BREAK */
}

int
main (void)
{
  fatp_t ptr = { 31415927, 27182818 };
  foo (ptr, 1);
  return 0;
}

/* { dg-final { gdb-test 26 "str.pa" "31415927" } } */
/* { dg-final { gdb-test 26 "str.pb" "27182818" } } */
