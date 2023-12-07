/* { dg-do compile } */
/* { dg-options "-fstrub=relaxed -Wpedantic" } */
/* { dg-require-effective-target strub } */

/* C++ does not warn about the partial incompatibilities.

   The d_p () calls are actually rejected, even in C++, but they are XFAILed
   here because we don't get far enough in the compilation as to observe them,
   because the incompatibilities are errors without -fpermissive.
   strub-ptrfn3.c uses -fpermissive to check those.
 */

extern int __attribute__ ((strub ("callable"))) bac (void);
extern int __attribute__ ((strub ("disabled"))) bad (void);
extern int __attribute__ ((strub ("internal"))) bar (void);
extern int __attribute__ ((strub ("at-calls"))) bal (void);

void __attribute__ ((strub))
bap (void)
{
  int __attribute__ ((strub ("disabled"))) (*d_p) (void) = bad;
  int __attribute__ ((strub ("callable"))) (*c_p) (void) = bac;
  int __attribute__ ((strub ("at-calls"))) (*a_p) (void) = bal;

  d_p = bac; /* { dg-warning "not quite compatible" "" { xfail c++ } } */
  c_p = bad; /* { dg-warning "not quite compatible" "" { xfail c++ } } */
  c_p = bar; /* { dg-warning "not quite compatible" "" { xfail c++ } } */
  c_p = bal; /* { dg-message "incompatible|invalid conversion" } */
  a_p = bac; /* { dg-message "incompatible|invalid conversion" } */

  d_p (); /* { dg-error "indirect non-.strub. call in .strub. context" "" { xfail *-*-* } } */
  c_p ();
  a_p ();
}

void __attribute__ ((strub))
baP (void)
{
  typedef int __attribute__ ((strub ("disabled"))) d_fn_t (void);
  typedef int __attribute__ ((strub ("callable"))) c_fn_t (void);
  typedef int __attribute__ ((strub ("at-calls"))) a_fn_t (void);

  d_fn_t *d_p = bad;
  c_fn_t *c_p = bac;
  a_fn_t *a_p = bal;

  d_p = bac; /* { dg-warning "not quite compatible" "" { xfail c++ } } */
  c_p = bad; /* { dg-warning "not quite compatible" "" { xfail c++ } } */
  c_p = bar; /* { dg-warning "not quite compatible" "" { xfail c++ } } */
  c_p = bal; /* { dg-message "incompatible|invalid conversion" } */
  a_p = bac; /* { dg-message "incompatible|invalid conversion" } */

  d_p (); /* { dg-error "indirect non-.strub. call in .strub. context" "" { xfail *-*-* } } */
  c_p ();
  a_p ();
}
