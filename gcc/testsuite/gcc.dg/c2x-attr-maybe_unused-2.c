/* Test C2x maybe_unused attribute: invalid contexts.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

/* This attribute is not valid in most cases on types other than their
   definitions, or on statements, or as an attribute-declaration.  */

[[maybe_unused]]; /* { dg-warning "ignored" } */

int [[maybe_unused]] var; /* { dg-warning "ignored" } */
/* { dg-message "that appertains to a type-specifier" "appertains" { target *-*-* } .-1 } */

int array_with_dep_type[2] [[maybe_unused]]; /* { dg-warning "ignored" } */
/* { dg-message "that appertains to a type-specifier" "appertains" { target *-*-* } .-1 } */

void fn_with_dep_type () [[maybe_unused]]; /* { dg-warning "ignored" } */
/* { dg-message "that appertains to a type-specifier" "appertains" { target *-*-* } .-1 } */

void
f (void)
{
  int a;
  [[maybe_unused]]; /* { dg-warning "ignored" } */
  [[maybe_unused]] a = 1; /* { dg-warning "ignored" } */
}
