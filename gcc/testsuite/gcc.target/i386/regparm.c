/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-W -Wall" } */

/* Verify that GCC correctly detects non-matching regparm attributes.  */
int __attribute__((regparm(3))) f (void);  /* { dg-message "note: previous" } */

int __attribute__((regparm(2))) f (void) { /* { dg-error "conflicting" } */
  return 0;
}
