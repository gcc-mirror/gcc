/* { dg-options "-fshow-column -Wall -Wfloat-equal -pedantic" } */

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

  if (&p == 0) /* { dg-warning "10:will never be NULL" } */
    bar();

  if (p == 4) /* { dg-warning "9:comparison between pointer and integer" } */
    bar();

  if (p < 0) /* { dg-warning "9:ordered comparison of pointer with" } */
    bar();
}
