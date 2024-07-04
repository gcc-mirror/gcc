/* Test C23 maybe_unused attribute: invalid contexts.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

/* This attribute is not valid in most cases on types other than their
   definitions, or on statements, or as an attribute-declaration.  */

[[maybe_unused]]; /* { dg-error "ignored" } */

int [[maybe_unused]] var; /* { dg-error "ignored" } */

int array_with_dep_type[2] [[maybe_unused]]; /* { dg-error "ignored" } */

void fn_with_dep_type () [[maybe_unused]]; /* { dg-error "ignored" } */

int z = sizeof (int [[__maybe_unused__]]); /* { dg-error "ignored" } */

void
f (void)
{
  int a;
  [[maybe_unused]]; /* { dg-error "ignored" } */
  [[maybe_unused]] a = 1; /* { dg-error "ignored" } */
}
