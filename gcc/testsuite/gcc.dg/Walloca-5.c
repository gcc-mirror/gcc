/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-Walloca-larger-than=123 -O2" } */
/* { dg-xfail-if "Currently broken but Andrew's work should fix this" { *-*-* } } */

/* The argument to alloca ends up having a range of 0..MAXINT(32bits),
   so we think we have a range because of the upcast.  Consequently,
   we warn with "alloca may be too large", but we should technically
   warn with "unbounded use of alloca".

   We currently drill through casts to figure this stuff out, but we
   get confused because it's not just a cast.  It's a cast, plus a
   multiply.

   <bb 2>:
  # RANGE [0, 4294967295] NONZERO 4294967295
  _1 = (long unsigned int) something_4(D);
  # RANGE [0, 34359738360] NONZERO 34359738360
  _2 = _1 * 8;
  _3 = __builtin_alloca (_2);

  I don't know whether it's even worth such fine-grained warnings.
  Perhaps we should generically warn everywhere with "alloca may be
  too large".

  I'm hoping that this particular case will be easier to diagnose with
  Andrew's work.  */

void useit(void *);
void foobar(unsigned int something)
{
  useit(__builtin_alloca (something * sizeof (const char *))); // { dg-warning "unbounded use of alloca" "" { xfail *-*-* } }
}
