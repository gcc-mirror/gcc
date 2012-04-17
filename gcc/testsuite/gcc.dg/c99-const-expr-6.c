/* Test for constant expressions: operands and casts not permitted in
   integer constant expressions.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

/* PR 29116.  */
int n = 0, p[n * 0 + 1]; /* { dg-error "variabl" } */

/* PR 31871.  */
extern int c[1 + ((__INTPTR_TYPE__) (void *) 0)]; /* { dg-error "variab" } */

/* Implicit conversions from floating-point constants are not OK,
   although explicit ones are.  */
extern int c1[1.0 ? 1 : 0]; /* { dg-error "variab" } */

extern int c2[(int)1.0 ? 1 : 0];

extern int c3[1.0 && 1]; /* { dg-error "variab" } */

extern int c4[(int)1.0 && 1];

extern int c5[1.0 || 1]; /* { dg-error "variab" } */

extern int c6[(int)1.0 || 1];

/* Similar with various other cases where integer constant expressions
   are required.  */

struct s {
  int a : (n * 0 + 1); /* { dg-error "constant" } */
};

enum e {
  E = (1 + ((__INTPTR_TYPE__) (void *) 0)), /* { dg-error "constant" } */
  E2 = 0
};

enum f {
  F = (1 ? 1 : n), /* { dg-error "constant" } */
  F2 = 0
};

/* Presume that a compound literal, being a reference to an anonymous
   variable, is not allowed in an integer constant expression
   regardless of what initializers it contains.  */
enum g {
  G = (1 ? 1 : (int){0}), /* { dg-error "constant" } */
  G2 = 0
};

int v[2] = { [(n * 0 + 1)] = 1 }; /* { dg-error "constant|near initialization" } */

void
f (int a)
{
  switch (a)
    {
    case (n * 0 + 1): /* { dg-error "constant" } */
      ;
    }
}
