/* PR middle-end/81824 - Warn for missing attributes with function aliases
   { dg-do compile }
   { dg-require-alias "" }
   { dg-options "-Wall" } */

#define ATTR(list)   __attribute__ (list)


int alias_no_const (void);

ATTR ((const)) int
target_const (void)             /* { dg-message ".alias_no_const. target declared here" } */
{ return 0; }

ATTR ((alias ("target_const"))) int
alias_no_const (void);          /* { dg-warning ".alias_no_const. specifies less restrictive attribute than its target .target_const.: .const." } */


ATTR ((alloc_size (1), malloc)) void*
target_malloc (int n)           /* { dg-message ".alias_no_malloc. target declared here" } */
{ return __builtin_malloc (n); }

ATTR ((alias ("target_malloc"))) void*
alias_no_malloc (int);          /* { dg-warning ".alias_no_malloc. specifies less restrictive attributes than its target .target_malloc.: .alloc_size., .malloc." } */


ATTR ((leaf)) int
target_leaf (void)              /* { dg-message ".alias_no_leaf. target declared here" } */
{ return 0; }

ATTR ((alias ("target_leaf"))) int
alias_no_leaf (void);           /* { dg-warning ".alias_no_leaf. specifies less restrictive attribute than its target .target_leaf.: .leaf." } */


/* Verify that attributes noclone, noinline, and noipa on the target
   don't cause a warning for aliases without the attribute.  */

ATTR ((noclone)) int
target_noclone (void)
{ return 0; }

ATTR ((alias ("target_noclone"))) int
alias_no_noclone (void);


ATTR ((noipa)) int
target_noipa (void)
{ return 0; }

ATTR ((alias ("target_noipa"))) int
alias_no_noipa (void);


ATTR ((noinline)) int
target_noinline (void)
{ return 0; }

ATTR ((alias ("target_noinline"))) int
alias_no_noinline (void);


ATTR ((nothrow)) int
target_nothrow (void)           /* { dg-message ".alias_no_nothrow. target declared here" } */
{ return 0; }

ATTR ((alias ("target_nothrow"))) int
alias_no_nothrow (void);        /* { dg-warning ".alias_no_nothrow. specifies less restrictive attribute than its target .target_nothrow.: .nothrow." } */


/* Verify that attribute weak on the target doesn't cause and isn't
   mentioned in a warning for aliases without the attribute.  */

ATTR ((weak)) int
target_weak (void)
{ return 0; }

ATTR ((alias ("target_weak"))) int
alias_no_weak (void);


ATTR ((nothrow, weak)) int
target_nothrow_weak (void)      /* { dg-message ".alias_nothrow_no_weak. target declared here" } */
{ return 0; }

ATTR ((alias ("target_nothrow_weak"))) int
alias_nothrow_no_weak (void);        /* { dg-warning ".alias_nothrow_no_weak. specifies less restrictive attribute than its target .target_nothrow_weak.: .nothrow." } */


/* Verify that __typeof__ doesn't include attributes.  */

ATTR ((cold)) int
target_cold (void)
{ return 0; }

__typeof__ (target_cold) ATTR ((alias ("target_cold")))
alias_cold;                   /* { dg-warning ".alias_cold. specifies less restrictive attribute than its target .target_cold.: .cold." } */
