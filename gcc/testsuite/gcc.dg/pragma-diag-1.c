/* { dg-do compile } */
/* { dg-options "-Wuninitialized -O2" } */
/* { dg-message "warnings being treated as errors" "" {target "*-*-*"} 0 } */

void foo (int);

int
main()
{
  int a;
  int b;
  int c;
  int d;

#pragma GCC diagnostic error "-Wuninitialized"
  foo(a);			/* { dg-error "uninitialized" } */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wuninitialized"
  foo(b);
#pragma GCC diagnostic pop
  foo(c);			/* { dg-error "uninitialized" } */
#pragma GCC diagnostic pop
  foo(d);			/* { dg-warning "uninitialized" } */
}
