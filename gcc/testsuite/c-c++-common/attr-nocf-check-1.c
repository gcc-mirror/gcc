/* { dg-do compile } */

int func (int) __attribute__ ((nocf_check)); /* { dg-warning "'nocf_check' attribute ignored" } */
int (*fptr) (int) __attribute__ ((nocf_check)); /* { dg-warning "'nocf_check' attribute ignored" } */
typedef void (*nocf_check_t) (void) __attribute__ ((nocf_check)); /* { dg-warning "'nocf_check' attribute ignored" } */

int
foo1 (int arg)
{
  return func (arg) + fptr (arg);
}

void
foo2 (void (*foo) (void))
{
  void (*func) (void) __attribute__((nocf_check)) = foo; /* { dg-warning "'nocf_check' attribute ignored" } */
  func ();
}

void
foo3 (nocf_check_t foo)
{
  foo ();
}

void
foo4 (void (*foo) (void) __attribute__((nocf_check))) /* { dg-warning "'nocf_check' attribute ignored" } */
{
  foo ();
}
