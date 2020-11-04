/* Test C2x nodiscard attribute: invalid contexts.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

/* This attribute is not valid on types other than their definitions,
   or on declarations other than function declarations, or on
   statements, or as an attribute-declaration.  */

[[nodiscard]]; /* { dg-error "ignored" } */

int [[nodiscard]] var; /* { dg-error "ignored" } */

int [[nodiscard ("reason")]] var2; /* { dg-error "ignored" } */

int array_with_nod_type[2] [[nodiscard]]; /* { dg-error "ignored" } */

void fn_with_nod_type () [[nodiscard]]; /* { dg-error "ignored" } */

int z = sizeof (int [[__nodiscard__]]); /* { dg-error "ignored" } */

[[nodiscard]] typedef int nod_int; /* { dg-error "can only be applied" } */

[[nodiscard]] int nvar; /* { dg-error "can only be applied" } */

struct s { int a; };

[[nodiscard]] typedef struct s nod_s; /* { dg-error "can only be applied" } */

struct t { [[nodiscard]] int b; }; /* { dg-error "can only be applied" } */

enum e { E [[nodiscard]] }; /* { dg-error "can only be applied" } */

void fx ([[nodiscard]] int p); /* { dg-error "can only be applied" } */

void
f (void)
{
  int a;
  [[nodiscard ("reason")]] int b = 1; /* { dg-error "can only be applied" } */
  [[nodiscard]]; /* { dg-error "ignored" } */
  [[nodiscard]] a = 1; /* { dg-error "ignored" } */
}
