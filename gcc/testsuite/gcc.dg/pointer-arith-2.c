/* Test diagnostics for arithmetic on void and function pointers.
   Test with -Wpointer-arith.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wpointer-arith" } */

void *p;
void (*f)(void);

void
g (void)
{
  p + 0; /* { dg-warning "pointer of type 'void \\*' used in arithmetic" } */
  p + 1; /* { dg-warning "pointer of type 'void \\*' used in arithmetic" } */
  0 + p; /* { dg-warning "pointer of type 'void \\*' used in arithmetic" } */
  1 + p; /* { dg-warning "pointer of type 'void \\*' used in arithmetic" } */
  p - 0; /* { dg-warning "pointer of type 'void \\*' used in arithmetic" } */
  p - 1; /* { dg-warning "pointer of type 'void \\*' used in arithmetic" } */
  p += 0; /* { dg-warning "pointer of type 'void \\*' used in arithmetic" } */
  p += 1; /* { dg-warning "pointer of type 'void \\*' used in arithmetic" } */
  p -= 0; /* { dg-warning "pointer of type 'void \\*' used in arithmetic" } */
  p -= 1; /* { dg-warning "pointer of type 'void \\*' used in arithmetic" } */
  f + 0; /* { dg-warning "pointer to a function used in arithmetic" } */
  f + 1; /* { dg-warning "pointer to a function used in arithmetic" } */
  0 + f; /* { dg-warning "pointer to a function used in arithmetic" } */
  1 + f; /* { dg-warning "pointer to a function used in arithmetic" } */
  f - 0; /* { dg-warning "pointer to a function used in arithmetic" } */
  f - 1; /* { dg-warning "pointer to a function used in arithmetic" } */
  f += 0; /* { dg-warning "pointer to a function used in arithmetic" } */
  f += 1; /* { dg-warning "pointer to a function used in arithmetic" } */
  f -= 0; /* { dg-warning "pointer to a function used in arithmetic" } */
  f -= 1; /* { dg-warning "pointer to a function used in arithmetic" } */
  p[0]; /* { dg-warning "dereferencing 'void \\*' pointer" } */
  /* { dg-warning "pointer of type 'void \\*' used in arithmetic" "array 1" { target *-*-* } .-1 } */
  0[p]; /* { dg-warning "dereferencing 'void \\*' pointer" } */
  /* { dg-warning "pointer of type 'void \\*' used in arithmetic" "array 1" { target *-*-* } .-1 } */
  f[0]; /* { dg-error "subscripted value is pointer to function" } */
  0[f]; /* { dg-error "subscripted value is pointer to function" } */
  p - p; /* { dg-warning "pointer of type 'void \\*' used in subtraction" } */
  f - f; /* { dg-warning "pointer to a function used in subtraction" } */
}
