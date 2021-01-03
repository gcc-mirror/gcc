/* Test C2x attribute syntax.  Invalid uses of attributes.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

/* Prefix attributes not allowed on declarations without declarators.  */

[[]] struct s { int a; }; /* { dg-error "empty declaration" } */

[[]] union u { int a; }; /* { dg-error "empty declaration" } */

void
f1 (void)
{
  [[]] struct t { int a; }; /* { dg-error "empty declaration" } */
}

/* Prefix attributes not allowed on _Static_assert.  */

[[]] _Static_assert (1); /* { dg-error "expected" } */

void
f2 (void)
{
  [[]] _Static_assert (1); /* { dg-error "expected" } */
}

/* Declarations, including attribute declarations, cannot appear after
   labels when a statement is expected.  */

void
f3 (void)
{
  if (1)
    x: [[]]; /* { dg-error "expected" } */
} 

/* Prefix attributes cannot appear on type names.  */

int z = sizeof ([[]] int); /* { dg-error "expected" } */

/* Attributes are not allowed after struct, union or enum, except when
   the type contents are being defined or the declaration is just
   "struct-or-union atribute-specifier-sequence identifier;".  */

const struct [[]] s2; /* { dg-warning "useless type qualifier" } */
/* { dg-error "invalid use of attributes in empty declaration" "invalid" { target *-*-* } .-1 } */

const union [[]] u2; /* { dg-warning "useless type qualifier" } */
/* { dg-error "invalid use of attributes in empty declaration" "invalid" { target *-*-* } .-1 } */

struct [[]] s3 *sv; /* { dg-error "expected" } */

union [[]] u3 *uv; /* { dg-error "expected" } */

enum e { E1 };

enum [[]] e *ev; /* { dg-error "expected" } */
