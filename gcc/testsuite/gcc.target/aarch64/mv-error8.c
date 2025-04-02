/* { dg-do compile } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("default"))) int
foo (int a, int (*b)[4]) { return 1; }

int bar(void) {
  __attribute__ ((target_version ("dotprod"))) int
  foo (int a, int (*b)[5]) { return 3; } /* { dg-error "versioned definitions are only allowed at file scope" } */

  return 1;
}
