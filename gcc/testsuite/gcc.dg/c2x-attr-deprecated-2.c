/* Test C2x deprecated attribute: invalid contexts.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

/* This attribute is not valid in most cases on types other than their
   definitions, or on statements, or as an attribute-declaration.  */

[[deprecated]]; /* { dg-error "ignored" } */

int [[deprecated]] var; /* { dg-error "ignored" } */

int array_with_dep_type[2] [[deprecated]]; /* { dg-error "ignored" } */

void fn_with_dep_type () [[deprecated]]; /* { dg-error "ignored" } */

int z = sizeof (int [[__deprecated__]]); /* { dg-error "ignored" } */

void
f (void)
{
  int a;
  [[deprecated]]; /* { dg-error "ignored" } */
  [[deprecated]] a = 1; /* { dg-error "ignored" } */
}
