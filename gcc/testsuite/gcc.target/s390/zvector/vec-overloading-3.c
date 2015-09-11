/* Check for error messages supposed to be issued during overloading.  */

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
  __builtin_s390_vec_gather_element (uv4si, uv4si, ucintptr, 256); /* { dg-error "constant argument 4 for builtin.*is out of range for target type" } */
  __builtin_s390_vec_gather_element (uv4si, uv4si, ucintptr, 5); /* { dg-error "constant argument 4 for builtin.*is out of range" } */
}
