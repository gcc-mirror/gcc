/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-check-noreturn-calls=always -fdump-tree-hardcfr -ffat-lto-objects" } */

/* Check that we insert checking before all noreturn calls.  */

#ifndef ATTR_NOTHROW_OPT /* Overridden in harden-cfr-noret-noexcept.  */
#define ATTR_NOTHROW_OPT __attribute__ ((__nothrow__))
#endif

extern void __attribute__ ((__noreturn__)) ATTR_NOTHROW_OPT g (void);

void f(int i) {
  if (i)
    /* Out-of-line checks here...  */
    g ();
  /* ... and here.  */
}

void __attribute__ ((__noinline__, __noclone__))
h(void) {
  /* Inline check here.  */
  g ();
}

#ifndef OMIT_H2 /* from harden-cfr-noret-never.  */
void h2(void) {
  /* Inline check either here, whether because of noreturn or tail call...  */
  h ();
  /* ... or here, if not optimizing.  */
}
#endif

/* One out-of-line check before the noreturn call in f, and another at the end
   of f.  */
/* { dg-final { scan-tree-dump-times "hardcfr_check" 2 "hardcfr" } } */
/* One inline check in h, before the noreturn call, and another in h2, before
   or after the call, depending on noreturn detection.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 2 "hardcfr" } } */
