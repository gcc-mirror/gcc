struct incomplete_t;
/* { dg-note "forward declaration of 'struct incomplete_t'" "" { target c++ } .-1 } */

/* Note: This note is only printed with C++ (trice); the loc is available due to TYPE_MAIN_DECL.  */

struct incomplete_t *ptr;
int i;

void
foo (void)
{
  #pragma omp target enter data map(to: i) map(to: ptr[0])
    /* All apply to the line above.  The first error is printed twice.  */
    /* { dg-error "invalid use of undefined type 'struct incomplete_t'" "" { target c } .-2 } */
    /* { dg-error "invalid use of incomplete type 'struct incomplete_t'" "" { target c++ } .-3 } */
    /* { dg-error "array section does not have mappable type in 'map' clause" "" { target *-*-* } .-4 } */
}
