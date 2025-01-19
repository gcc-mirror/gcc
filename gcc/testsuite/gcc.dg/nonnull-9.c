/* Test for the invalid use of the "nonnull_if_nonzero" function attribute.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu17 -pedantic-errors" } */

extern void func1 () __attribute__((nonnull_if_nonzero)); /* { dg-error "wrong number of arguments specified for 'nonnull_if_nonzero' attribute" } */
/* { dg-message "expected 2, found 0" "" { target *-*-* } .-1 } */

extern void func2 (char *) __attribute__((nonnull_if_nonzero(1))); /* { dg-error "wrong number of arguments specified for 'nonnull_if_nonzero' attribute" } */
/* { dg-message "expected 2, found 1" "" { target *-*-* } .-1 } */

extern void func3 (char *) __attribute__((nonnull_if_nonzero(1, 2, 3))); /* { dg-error "wrong number of arguments specified for 'nonnull_if_nonzero' attribute" } */
/* { dg-message "expected 2, found 3" "" { target *-*-* } .-1 } */

extern void func4 (char *, int) __attribute__((nonnull_if_nonzero(3, 2))); /* { dg-warning "'nonnull_if_nonzero' attribute argument 1 value '3' exceeds the number of function parameters 2" } */

extern void func5 (char *, int) __attribute__((nonnull_if_nonzero(1, 3))); /* { dg-warning "nonnull_if_nonzero' attribute argument 2 value '3' exceeds the number of function parameters 2" } */

extern void func6 (char *, int) __attribute__((nonnull_if_nonzero (foo, 2))); /* { dg-warning ".nonnull_if_nonzero. attribute argument 1 is invalid" } */
/* { dg-error ".foo. undeclared" "undeclared argument" { target *-*-* } .-1 } */

extern void func7 (char *, int) __attribute__((nonnull_if_nonzero (1, bar))); /* { dg-warning ".nonnull_if_nonzero. attribute argument 2 is invalid" } */
/* { dg-error ".bar. undeclared" "undeclared argument" { target *-*-* } .-1 } */

extern void func8 (int, int) __attribute__((nonnull_if_nonzero(1, 2))); /* { dg-warning "'nonnull_if_nonzero' attribute argument 1 value '1' refers to parameter type 'int'" } */

extern void func9 (char *, float) __attribute__((nonnull_if_nonzero(1, 2))); /* { dg-warning "'nonnull_if_nonzero' attribute argument 2 value '2' refers to parameter type 'float'" } */

extern void func10 (char *, _Bool) __attribute__((nonnull_if_nonzero(1, 2))); /* { dg-warning "'nonnull_if_nonzero' attribute argument 2 value '2' refers to parameter type '_Bool'" } */

extern void func11 (char *, char *) __attribute__((nonnull_if_nonzero(1, 2))); /* { dg-warning "'nonnull_if_nonzero' attribute argument 2 value '2' refers to parameter type 'char \\\*'" } */

void
foo (void)
{
}

void
bar (void)
{
}
