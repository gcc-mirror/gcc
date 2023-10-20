/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fno-hardcfr-check-returning-calls -fdump-tree-hardcfr -ffat-lto-objects" } */

#if ! __OPTIMIZE__ && ! defined NO_OPTIMIZE
/* Without optimization, functions with cleanups end up with an extra
   resx that is not optimized out, so arrange to optimize them.  */
void __attribute__ ((__optimize__ (1))) h2(void);
void __attribute__ ((__optimize__ (1))) h2b(void);
#endif

/* Check that we insert cleanups for checking around the bodies of
   maybe-throwing functions.  */

extern void g (void);
extern void g2 (void);

void f(int i) {
  if (i)
    g ();
  /* Out-of-line checks here, and in the implicit handler.  */
}

void f2(int i) {
  if (i)
    g ();
  else
    g2 ();
  /* Out-of-line checks here, and in the implicit handler.  */
}

void h(void) {
  try {
    g ();
  } catch (...) {
    throw;
  }
  /* Out-of-line checks here, and in the implicit handler.  */
}

struct needs_cleanup {
  ~needs_cleanup();
};

void h2(void) {
  needs_cleanup y; /* No check in the cleanup handler.  */
  g();
  /* Out-of-line checks here, and in the implicit handler.  */
}

extern void __attribute__ ((__nothrow__)) another_cleanup (void*);

void h2b(void) {
  int x __attribute__ ((cleanup (another_cleanup)));
  g();
  /* Out-of-line checks here, and in the implicit handler.  */
}

void h3(void) {
  try {
    throw 1;
  } catch (...) {
  }
  /* Out-of-line checks here, and in the implicit handler.  */
}

void h4(void) {
  throw 1;
  /* Inline check in the cleanup around the __cxa_throw noreturn call.  */
}

/* { dg-final { scan-tree-dump-times "hardcfr_check" 12 "hardcfr" } } */
/* { dg-final { scan-tree-dump-times "builtin_trap" 1 "hardcfr" } } */
/* { dg-final { scan-tree-dump-times "Bypassing" 0 "hardcfr" } } */
