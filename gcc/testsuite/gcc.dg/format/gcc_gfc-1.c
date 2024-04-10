/* Test for gcc_gfc formats.  */
/* Origin: Kaveh Ghazi <ghazi@caip.rutgers.edu> */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */

#include "format.h"

/* Magic identifier must be set before the attribute is used.  */
typedef struct locus locus;

extern int gfc_warn (const char *, ...) __attribute__ ((__format__ (__gcc_gfc__, 1, 2))) __attribute__ ((__nonnull__));

void
foo (unsigned int u, int i, char *s, unsigned long int ul, long int l,
     llong ll, unsigned long long int ull, locus *loc, size_t sz,
     ptrdiff_t pd, ssize_t ssz, unsigned_ptrdiff_t upd)
{
  /* Acceptable C90 specifiers, flags and modifiers.  */
  gfc_warn ("%%");
  gfc_warn ("%u%d%i%c%s%%", u, i, i, i, s);
  gfc_warn ("%lu%ld%li%%", ul, l, l);

  /* Acceptable C99 specifiers, flags and modifiers.  */
  gfc_warn ("%llu%lld%lli%%", ull, ll, ll);
  gfc_warn ("%zu%zd%zi%%", sz, ssz, ssz);
  gfc_warn ("%tu%td%ti%%", upd, pd, pd);

  /* Extensions provided in gfc_warn.  */
  gfc_warn ("%C");
  gfc_warn ("%L", loc);

  /* Various tests of bad argument types.  */
  gfc_warn ("%d", l); /* { dg-warning "format" "bad argument types" } */
  gfc_warn ("%d", ll); /* { dg-warning "format" "bad argument types" } */
  gfc_warn ("%s", &i); /* { dg-warning "format" "bad argument types" } */
  gfc_warn ("%L", &i); /* { dg-warning "format" "bad argument types" } */
  gfc_warn ("%C", i); /* { dg-warning "format" "too many arguments" } */
}
