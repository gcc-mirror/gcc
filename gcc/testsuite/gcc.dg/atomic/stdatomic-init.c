/* Test the atomic_init generic function.  Verify that __atomic_store_N
   is called with the last argument of memory_order_relaxed (i.e., 0)
   for each invocation of the atomic_init() macro in the test and that
   there are no calls to __atomic_store_N with a non-zero last argument.  */
/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple -std=c11 -pedantic-errors" } */
/* { dg-final { scan-tree-dump-times "__atomic_store_. \\(\[^\n\r]*, 0\\)" 54 "gimple" } } */
/* { dg-final { scan-tree-dump-not "__atomic_store_. \\(\[^\n\r]*, \[1-5\]\\)" "gimple" } } */

#include <stdatomic.h>

struct Atomic {
  /* Volatile to prevent re-initialization from being optimized away.  */
  volatile atomic_bool   b;
  volatile atomic_char   c;
  volatile atomic_schar  sc;
  volatile atomic_uchar  uc;
  volatile atomic_short  ss;
  volatile atomic_ushort us;
  volatile atomic_int    si;
  volatile atomic_uint   ui;
  volatile atomic_long   sl;
  volatile atomic_ulong  ul;
  volatile atomic_llong  sll;
  volatile atomic_ullong ull;
  volatile atomic_size_t sz;
};

struct Value {
  _Bool              b;
  char               c;
  signed char        sc;
  unsigned char      uc;
  short              ss;
  unsigned short     us;
  int                si;
  unsigned int       ui;
  long               sl;
  unsigned long      ul;
  long long          sll;
  unsigned long long ull;
  __SIZE_TYPE__      sz;
};

/* Exercise the atomic_init() macro with a literal argument.  */

void atomic_init_lit (struct Atomic *pa)
{
  atomic_init (&pa->b, 0);
  atomic_init (&pa->b, 1);

  atomic_init (&pa->c, 'x');
  atomic_init (&pa->c, 0);
  atomic_init (&pa->c, 1);
  atomic_init (&pa->c, 255);
  
  atomic_init (&pa->sc, (signed char)'x');
  atomic_init (&pa->sc, (signed char)0);
  atomic_init (&pa->sc, (signed char)1);
  atomic_init (&pa->sc, (signed char)__SCHAR_MAX__);

  atomic_init (&pa->uc, (unsigned char)'x');
  atomic_init (&pa->uc, (unsigned char)0);
  atomic_init (&pa->uc, (unsigned char)1);
  atomic_init (&pa->sc, (unsigned char)__SCHAR_MAX__);

  atomic_init (&pa->ss, (signed short)0);
  atomic_init (&pa->ss, (signed short)1);
  atomic_init (&pa->ss, (signed short)__SHRT_MAX__);

  atomic_init (&pa->us, (unsigned short)0);
  atomic_init (&pa->us, (unsigned short)1);
  atomic_init (&pa->us, (unsigned short)__SHRT_MAX__);

  atomic_init (&pa->si, (signed int)0);
  atomic_init (&pa->si, (signed int)1);
  atomic_init (&pa->si, (signed int)__INT_MAX__);

  atomic_init (&pa->ui, (unsigned int)0);
  atomic_init (&pa->ui, (unsigned int)1);
  atomic_init (&pa->ui, (unsigned int)__INT_MAX__);
  
  atomic_init (&pa->sl, (signed long)0);
  atomic_init (&pa->sl, (signed long)1);
  atomic_init (&pa->sl, (signed long)__LONG_MAX__);

  atomic_init (&pa->ul, (unsigned long)0);
  atomic_init (&pa->ul, (unsigned long)1);
  atomic_init (&pa->ul, (unsigned long)__LONG_MAX__);

  atomic_init (&pa->sll, (signed long long)0);
  atomic_init (&pa->sll, (signed long long)1);
  atomic_init (&pa->sll, (signed long long)__LONG_LONG_MAX__);

  atomic_init (&pa->ull, (unsigned long long)0);
  atomic_init (&pa->ull, (unsigned long long)1);
  atomic_init (&pa->ull, (unsigned long long)__LONG_LONG_MAX__); 

  atomic_init (&pa->sz, 0);
  atomic_init (&pa->sz, 1);
  atomic_init (&pa->sz, __SIZE_MAX__); 
}

/* Exercise the atomic_init() macro with an lvalue argument.  */

void atomic_init_lval (struct Atomic *pa, const struct Value *pv)
{
  atomic_init (&pa->b, pv->b);
  atomic_init (&pa->c, pv->c);
  atomic_init (&pa->sc, pv->sc);
  atomic_init (&pa->uc, pv->uc);
  atomic_init (&pa->ss, pv->ss);
  atomic_init (&pa->us, pv->us);
  atomic_init (&pa->si, pv->si);
  atomic_init (&pa->ui, pv->ui); 
  atomic_init (&pa->sl, pv->sl);
  atomic_init (&pa->ul, pv->ul);
  atomic_init (&pa->sll, pv->sll);
  atomic_init (&pa->ull, pv->ull);
  atomic_init (&pa->sz, pv->sz);
}
