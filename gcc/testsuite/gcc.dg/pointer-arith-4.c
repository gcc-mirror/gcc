/* Test diagnostics for arithmetic on void and function pointers.
   Test with -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

void *p;
void (*f)(void);

void
g (void)
{
  p + 0; /* { dg-error "pointer of type 'void \\*' used in arithmetic" } */
  p + 1; /* { dg-error "pointer of type 'void \\*' used in arithmetic" } */
  0 + p; /* { dg-error "pointer of type 'void \\*' used in arithmetic" } */
  1 + p; /* { dg-error "pointer of type 'void \\*' used in arithmetic" } */
  p - 0; /* { dg-error "pointer of type 'void \\*' used in arithmetic" } */
  p - 1; /* { dg-error "pointer of type 'void \\*' used in arithmetic" } */
  p += 0; /* { dg-error "pointer of type 'void \\*' used in arithmetic" } */
  p += 1; /* { dg-error "pointer of type 'void \\*' used in arithmetic" } */
  p -= 0; /* { dg-error "pointer of type 'void \\*' used in arithmetic" } */
  p -= 1; /* { dg-error "pointer of type 'void \\*' used in arithmetic" } */
  f + 0; /* { dg-error "pointer to a function used in arithmetic" } */
  f + 1; /* { dg-error "pointer to a function used in arithmetic" } */
  0 + f; /* { dg-error "pointer to a function used in arithmetic" } */
  1 + f; /* { dg-error "pointer to a function used in arithmetic" } */
  f - 0; /* { dg-error "pointer to a function used in arithmetic" } */
  f - 1; /* { dg-error "pointer to a function used in arithmetic" } */
  f += 0; /* { dg-error "pointer to a function used in arithmetic" } */
  f += 1; /* { dg-error "pointer to a function used in arithmetic" } */
  f -= 0; /* { dg-error "pointer to a function used in arithmetic" } */
  f -= 1; /* { dg-error "pointer to a function used in arithmetic" } */
  p[0]; /* { dg-warning "dereferencing 'void \\*' pointer" } */
  /* { dg-error "pointer of type 'void \\*' used in arithmetic" "array 1" { target *-*-* } 33 } */
  0[p]; /* { dg-warning "dereferencing 'void \\*' pointer" } */
  /* { dg-error "pointer of type 'void \\*' used in arithmetic" "array 1" { target *-*-* } 35 } */
  f[0]; /* { dg-error "subscripted value is pointer to function" } */
  0[f]; /* { dg-error "subscripted value is pointer to function" } */
  p - p; /* { dg-error "pointer of type 'void \\*' used in subtraction" } */
  f - f; /* { dg-error "pointer to a function used in subtraction" } */
}
