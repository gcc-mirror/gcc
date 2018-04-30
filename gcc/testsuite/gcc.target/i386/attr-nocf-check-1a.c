/* { dg-do compile } */
/* { dg-options "-fcf-protection" } */

int func (int) __attribute__ ((nocf_check));
int (*fptr) (int) __attribute__ ((nocf_check));
typedef void (*nocf_check_t) (void) __attribute__ ((nocf_check));

int
foo1 (int arg)
{
  return func (arg) + fptr (arg);
}

void
foo2 (void (*foo) (void))
{
  void (*func) (void) __attribute__((nocf_check)) = foo; /* { dg-warning "incompatible pointer type" "" { target c } } */
							 /* { dg-error "invalid conversion" "" { target c++ } .-1 } */
  func ();
}

void
foo3 (nocf_check_t foo)
{
  foo ();
}

void
foo4 (void (*foo) (void) __attribute__((nocf_check)))
{
  foo ();
}
