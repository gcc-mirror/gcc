/* PR middle-end/110027 */
/* { dg-do run } */
/* { dg-additional-options "-fstack-protector-strong" { target fstack_protector } } */
/* { dg-set-target-env-var ASAN_OPTIONS "detect_stack_use_after_return=1" } */

struct __attribute__((aligned (128))) S { char s[128]; };
struct __attribute__((aligned (64))) T { char s[192]; };
struct __attribute__((aligned (32))) U { char s[256]; };
struct __attribute__((aligned (64))) V { char s[320]; };
struct __attribute__((aligned (128))) W { char s[512]; };

__attribute__((noipa)) void
foo (void *p, void *q, void *r, void *s)
{
  if (((__UINTPTR_TYPE__) p & 31) != 0
      || ((__UINTPTR_TYPE__) q & 127) != 0
      || ((__UINTPTR_TYPE__) r & 63) != 0)
    __builtin_abort ();
  (void *) s;
}

__attribute__((noipa)) int
bar (void)
{
  struct U u;
  struct S s;
  struct T t;
  char p[4];
  foo (&u, &s, &t, &p);
  return 42;
}

__attribute__((noipa)) int
baz (void)
{
  struct W w;
  struct U u;
  struct V v;
  char p[4];
  foo (&u, &w, &v, &p);
  return 42;
}

int
main ()
{
  bar ();
  baz ();
  return 0;
}
