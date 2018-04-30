/* { dg-do compile } */
/* { dg-additional-options "-fprofile-generate" } */
/* { dg-require-effective-target global_constructor } */

void __attribute__ ((__cold__)) a (void);
void b (void);
void __attribute__ ((noinline)) c (void) { a (); }

void
d (void)
{
  b ();
  c ();
}
