/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

void
bar ()
{
  __attribute__ ((target_version ("dotprod"))) int
  foo1 (); /* { dg-message "versioned declarations are only allowed at file scope" } */

  __attribute__ ((target_version ("simd"))) int
  foo2 () { return 1; } /* { dg-message "versioned definitions are only allowed at file scope" } */
}
