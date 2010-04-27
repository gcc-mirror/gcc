/* { dg-options "-fshow-column -Wall -Wfloat-equal -pedantic" } */

int i, j;
float a, b;

int *p;
struct {
	int a;
	char b;
} *q;

extern void bar();

void foo (void)
{
  if (a == b) /* { dg-warning "9:comparing floating point with" } */
    bar ();

  if (p < q) /* { dg-warning "9:comparison of distinct pointer types" } */
    bar ();

  if (&p == 0) /* { dg-warning "10:comparison will always evaluate as 'false'" } */
    bar();

  if (p == 4) /* { dg-warning "9:comparison between pointer and integer" } */
    bar();

  if (p < 0) /* { dg-warning "9:ordered comparison of pointer with" } */
    bar();

  -q;	 /* { dg-error "3:wrong type argument to unary" } */

  ~q;    /* { dg-error "3:wrong type argument to bit" } */

  ++*q; /* { dg-error "3:wrong type argument to increment" } */

  i = j / 0;  /* { dg-warning "9:division by zero" } */

  i /= 0; /* { dg-warning "5:division by zero" } */
}
