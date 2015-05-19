/* Check for error messages supposed to be issued during builtin expansion.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-march=z13 -mzarch -mzvector" } */

__vector int v4si;
__vector unsigned uv4si;

int *intptr;
unsigned long long ull;
const unsigned int *ucintptr;

void
foo ()
{
  /* A backend check makes sure the forth operand is a literal.  */
  __builtin_s390_vec_scatter_element (v4si, uv4si, intptr, ull); /* { dg-error "constant value required for builtin" } */
}
