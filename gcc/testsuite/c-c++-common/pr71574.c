/* PR c/71574 - ICE on code with alloc_align attribute */
/* { dg-do compile } */

int fn1 (int);
int fn2 (void) __attribute__ ((alloc_align (fn1))); /* { dg-warning ".alloc_align. attribute ignored on a function returning .int." } */
int fn3 (void) __attribute__ ((alloc_size (fn1))); /* { dg-warning ".alloc_size. attribute ignored on a function returning .int." } */
int fn4 (void) __attribute__ ((assume_aligned (fn1))); /* { dg-warning ".assume_aligned. attribute ignored on a function returning .int." } */
int fn5 (char *, char *) __attribute__((nonnull (fn1))); /* { dg-warning ".nonnull. attribute argument has type .int\\\(int\\\)." } */
int fn6 (const char *, ...) __attribute__ ((sentinel (fn1))); /* { dg-warning "not an integer constant" } */

void* fn7 (void) __attribute__ ((alloc_align (fn1))); /* { dg-warning ".alloc_align. attribute argument has type .int\\\(int\\\)." } */
void* fn8 (void) __attribute__ ((assume_aligned (fn1))); /* { dg-warning "not an integer constant" } */

typedef int __attribute__((vector_size (fn1))) v4si; /* { dg-error "'vector_size' attribute argument value 'fn1' is not an integer constant" } */
typedef int T __attribute__((aligned (fn1))); /* { dg-error "requested alignment is not" } */
