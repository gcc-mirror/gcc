/* Test C2x deprecated attribute: invalid contexts.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

/* This attribute is not valid in most cases on types other than their
   definitions, or on statements, or as an attribute-declaration.  */

[[deprecated]]; /* { dg-warning "ignored" } */

int [[deprecated]] var; /* { dg-warning "ignored" } */
/* { dg-message "that appertains to a type-specifier" "appertains" { target *-*-* } .-1 } */

int array_with_dep_type[2] [[deprecated]]; /* { dg-warning "ignored" } */
/* { dg-message "that appertains to a type-specifier" "appertains" { target *-*-* } .-1 } */

void fn_with_dep_type () [[deprecated]]; /* { dg-warning "ignored" } */
/* { dg-message "that appertains to a type-specifier" "appertains" { target *-*-* } .-1 } */

void
f (void)
{
  int a;
  [[deprecated]]; /* { dg-warning "ignored" } */
  [[deprecated]] a = 1; /* { dg-warning "ignored" } */
}
