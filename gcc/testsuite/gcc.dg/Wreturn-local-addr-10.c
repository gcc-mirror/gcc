/* PR c/71924 - missing -Wreturn-local-addr returning alloca result
   Test reduced from libstdc++-v3/testsuite/ext/ext_pointer/1.cc.
   It verifies that iteration in find_implicit_erroneous_behavior
   in gimple-ssa-isolate-path.c terminates under specific conditions.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __UINTPTR_TYPE__ uintptr_t;

struct A { int i; };
struct P { uintptr_t d; };

static inline struct A* get (const struct P *p)
{
  if (p->d == 1)
    return 0;

  return (struct A*)((uintptr_t)p + p->d);
}

static inline void set (struct P *p, struct A* q)
{
  /* The basic block below would cause an infinite loop in
     find_implicit_erroneous_behavior due to assuming the DUPLICATE
     pointer returned from isolate_path would distinct from the one
     passed to it.  (Replacing the if statement with the ternary ?:
     expression did not have this effect (it gets optimized early
     on).
    <bb 4> [local count: 1073741823]:
    # _14 = PHI <0B(2), &MEM <struct A[2]> [(void *)&a + 4B](3)>
    _2 = _14->i;
    if (_2 != 2)
      goto <bb 5>; [0.00%]
    else
      goto <bb 6>; [100.00%]
  */
  if (!q)
    p->d = 1;
  else
    p->d = (uintptr_t)(q) - (uintptr_t)(p);
}

void f (void)
{
  struct A a[2] = { { 1 }, { 2 } };

  struct P p, q;
  set (&p, a);
  set (&q, get (&p));

  set (&q, get (&q) + 0);
  set (&q, get (&q) + 1);

  if (get (&q)[0].i != get (&p)[1].i)
    __builtin_abort ();
}
