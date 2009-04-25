/* Test use of sizeof with [*] in type name: should not refer to
   zero-size array.  PR 39582.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

void foo11d(int x[sizeof(int *[*])]); /* { dg-warning "not in a declaration" } */

/* Although the size is not constant, it may nevertheless appear in a
   constant expression if not evaluated.  */

void foo11e(int x[1 ? 0 : sizeof(int *[*])]); /* { dg-warning "not in a declaration" } */
/* { dg-error "zero-size array" "correct zero size" { target *-*-* } 11 } */
