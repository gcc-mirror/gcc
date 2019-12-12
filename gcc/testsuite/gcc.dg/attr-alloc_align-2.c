/* { dg-do compile } */

int i;
void *f1 (int) __attribute__((alloc_align (1)));
void *f2 (int, int, int) __attribute__((alloc_align (3)));
void *f3 (void) __attribute__((alloc_align)); /* { dg-error "wrong number of arguments specified" } */
void *f4 (int, int) __attribute__((alloc_align (1, 2))); /* { dg-error "wrong number of arguments specified" } */
void *f5 (void) __attribute__((alloc_align (i))); /* { dg-warning ".alloc_align. attribute argument value .i. is not an integer constant" } */
void *f6 (int) __attribute__((alloc_align (0))); /* { dg-warning ".alloc_align. attribute argument value .0. does not refer to a function parameter" } */
void *f7 (int) __attribute__((alloc_align (2))); /* { dg-warning ".alloc_align. attribute argument value .2. exceeds the number of function parameters 1" } */
