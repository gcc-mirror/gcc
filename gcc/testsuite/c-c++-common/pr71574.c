/* PR c/71574 */
/* { dg-do compile } */

int fn1 (void);
int fn2 (void) __attribute__ ((alloc_align (fn1))); /* { dg-warning "parameter outside range" } */
int fn3 (void) __attribute__ ((alloc_size (fn1))); /* { dg-warning "parameter outside range" } */
int fn4 (void) __attribute__ ((assume_aligned (fn1))); /* { dg-warning "not integer constant" } */
int fn5 (char *, char *) __attribute__((nonnull (fn1))); /* { dg-error "nonnull argument has invalid operand" } */
int fn6 (const char *, ...) __attribute__ ((sentinel (fn1))); /* { dg-warning "not an integer constant" } */

typedef int __attribute__((vector_size (fn1))) v4si; /* { dg-warning "attribute ignored" } */
typedef int T __attribute__((aligned (fn1))); /* { dg-error "requested alignment is not" } */
