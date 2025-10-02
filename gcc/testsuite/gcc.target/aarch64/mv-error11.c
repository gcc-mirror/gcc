/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

int fn () asm("name");
int fn () { return 1; } /* { dg-error "cannot use function multiversioning on a renamed function" } */
int fn [[gnu::target_version("sve")]] () { return 1; }

int fn2 [[gnu::target_version("sve")]] () asm("name"); /* { dg-warning ".asm. declaration ignored due to conflict with previous rename" } */
