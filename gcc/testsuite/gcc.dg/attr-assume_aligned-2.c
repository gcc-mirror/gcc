/* { dg-do compile } */

int i;
void *f1 (void) __attribute__((assume_aligned (32)));
void *f2 (void) __attribute__((assume_aligned (16, 4)));
void *f3 (void) __attribute__((assume_aligned)); /* { dg-error "wrong number of arguments specified" } */
void *f4 (void) __attribute__((assume_aligned (32, 16, 8))); /* { dg-error "wrong number of arguments specified" } */
void *f5 (void) __attribute__((assume_aligned (i))); /* { dg-warning "integer constant" } */
