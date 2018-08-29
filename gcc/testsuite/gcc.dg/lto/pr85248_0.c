/* PR lto/85248 */
/* { dg-lto-do run } */
/* { dg-lto-options { { -flto -O2 } } } */

#define STR1(X) #X
#define STR2(X) STR1(X)

extern void test_alias (int s, int e) 
  __asm__ (STR2(__USER_LABEL_PREFIX__) "test");
extern void test_noreturn (int s, int e)
  __asm__ (STR2(__USER_LABEL_PREFIX__)  "test")
  __attribute__ ((__noreturn__));

extern inline __attribute__ ((__always_inline__, __gnu_inline__)) void
test (int s, int e)
{
  if (__builtin_constant_p (s) && s != 0)
    test_noreturn (s, e);
  else
    test_alias (s, e);
}

int
foo (void)
{
  static volatile int a;
  return a;
}

static void
bar (void)
{
  test (0, 1);
  __builtin_exit (0);
}

static void
baz ()
{
  test (1, 0);
}

int
main ()
{
  if (foo ())
    baz ();
  bar ();
  __builtin_abort ();
}
