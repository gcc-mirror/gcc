/* { dg-do compile } */
/* { dg-options "-fstrub=relaxed" } */
/* { dg-require-effective-target strub } */

/* This is strub-ptrfn2.c without -Wpedantic.

   Even C doesn't report the (not-quite-)compatible conversions without it.  */

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

  d_p = bac;
  c_p = bad;
  c_p = bar;
  c_p = bal; /* { dg-message "incompatible|invalid conversion" } */
  a_p = bac; /* { dg-message "incompatible|invalid conversion" } */
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

  d_p = bac;
  c_p = bad;
  c_p = bar;
  c_p = bal; /* { dg-message "incompatible|invalid conversion" } */
  a_p = bac; /* { dg-message "incompatible|invalid conversion" } */
}
